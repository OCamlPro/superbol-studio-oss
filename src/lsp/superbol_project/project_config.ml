(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1
open EzFile.OP
open Ez_toml.V1

module DIAGS = Cobol_common.Diagnostics

module TYPES = struct

  type path =
    | RelativeToProjectRoot of string
    | RelativeToFileDir of string

  type config = {
    mutable cobol_config: Cobol_config.t;
    mutable source_format: Cobol_config.source_format_spec;
    mutable libpath: path list;
    mutable copybook_extensions: string list;
    mutable copybook_if_no_extension: bool;
    mutable indent_config: (string * int) list;
    toml_handle: Ezr_toml.toml_handle;
  }

  exception ERROR of Project_diagnostics.error

end
include TYPES
type t = config


let __init_default_exn_printers =
  Printexc.register_printer begin function
    | ERROR e ->
        Some (Pretty.to_string "%a" Project_diagnostics.pp_error e)
    | _ ->
        None
  end


let default_libpath = [RelativeToProjectRoot "."]
let default_copybook_extensions = Cobol_common.Copybook.copybook_extensions
let default_copybook_if_no_extension = true
let default_indent_config = []

let default = {
  cobol_config = Cobol_config.default;
  source_format = Cobol_config.Auto;
  libpath = default_libpath;
  copybook_extensions = default_copybook_extensions;
  indent_config = default_indent_config;
  copybook_if_no_extension = default_copybook_if_no_extension;
  toml_handle = Ezr_toml.make_empty ();
}

let new_default () =
  { default with toml_handle = Ezr_toml.make_empty () }

(* Translation to TOML *)

let dialect_repr dialect =
  TOML.value_of_string @@ Cobol_config.DIALECT.to_string dialect

let format_repr format =
  TOML.value_of_string @@ Cobol_config.Options.string_of_format format

let path_repr = function
  | RelativeToProjectRoot dir ->
      TOML.table_of_list [
        "dir", TOML.string dir;
        "file-relative", TOML.bool false;
      ]
  | RelativeToFileDir dir ->
      TOML.table_of_list [
        "dir", TOML.string dir;
        "file-relative", TOML.bool true;
      ]

let libpath_repr libpath =
  TOML.value_of_array @@ Array.of_list @@ List.map path_repr libpath

let indent_repr indent =
  TOML.value_of_table @@
  List.fold_left
    (fun acc (n, v) -> EzCompat.StringMap.add n (TOML.int v) acc)
    EzCompat.StringMap.empty indent

let config_repr config ~name =
  Ezr_toml.section
    ~name
    ~after_comments: ["SuperBOL project configuration"]
    Ezr_toml.[
      option
        ~name: "dialect"
        ~after_comments: ["Default dialect for COBOL source files"]
        (dialect_repr @@ Cobol_config.dialect config.cobol_config);

      option
        ~name: "source-format"
        ~after_comments: ["Default source reference-format"]
        (format_repr config.source_format);

      option
        ~name: "copybooks"
        ~after_comments: ["Where to find copybooks"]
        (libpath_repr config.libpath);

      option
        ~name: "indent"
        ~after_comments: ["Indenter configuration"]
        (indent_repr config.indent_config)
    ]


let config_section_name = "cobol"

let cobol_config_from_dialect_name dialect_name =
  try Cobol_config.(from_dialect @@ DIALECT.of_string dialect_name) with
  | Invalid_argument e ->
      raise @@ ERROR (Unknown_dialect e)
  | Cobol_config.ERROR e ->
      raise @@ ERROR (Cobol_config_error e)

let get_source_format toml =
  try Cobol_config.Options.format_of_string @@
       TOML.get_string toml ["source-format"]
  with Not_found -> default.source_format

let get_dialect toml =
  TOML.get_string toml ["dialect"] ~default:"default"

let get_path_entry toml =
  let dir = TOML.get_string toml ["dir"] in
  if TOML.get_bool toml ["file-relative"] ~default:false
  then RelativeToFileDir dir
  else RelativeToProjectRoot dir

let get_libpath toml =
  try
    List.map get_path_entry @@
    Array.to_list @@ TOML.get_array toml ["copybooks"]
  with Not_found -> default_libpath

let get_indent_config toml =
  try
    EzCompat.StringMap.fold (fun name node v ->
      (name, TOML.extract_int node) :: v
    ) (TOML.get_table toml ["indent"]) []
  with Not_found -> default_indent_config

let load_file ?verbose config_filename =
  let toml_handle = Ezr_toml.load ?verbose config_filename in
  let load_section keys toml =
    let section = TOML.get keys toml in
    DIAGS.map_result
      (cobol_config_from_dialect_name @@ get_dialect section)
      ~f:(fun cobol_config ->
          { default with
            cobol_config;
            source_format = get_source_format section;
            libpath = get_libpath section;
            toml_handle = toml_handle ;
            indent_config = get_indent_config section})
  in
  try
    let DIAGS.{ result; _ } as config =
      let toml = Ezr_toml.toml toml_handle in
      try load_section toml [config_section_name]
      with Not_found -> DIAGS.result { default with toml_handle }
    in
    Ezr_toml.add_section_update toml_handle
      config_section_name (config_repr result);
    config
  with TOML.Types.Error (loc, _code, error) ->
    raise @@ ERROR (Invalid_toml { loc; error })

let save ?verbose ~config_filename config =
  Ezr_toml.save ?verbose config_filename config.toml_handle

(* --- *)

let libpath_for ~filename { libpath; _ } =
  List.map begin function
    | RelativeToProjectRoot str -> str
    | RelativeToFileDir str -> Filename.dirname filename // str
  end libpath

(* TODO: add config flags to libpath where some directories may only include
   copybooks. *)
let detect_copybook ~filename { copybook_extensions;
                                copybook_if_no_extension; _ } =
  List.exists (Filename.check_suffix filename) copybook_extensions ||
  (copybook_if_no_extension && Filename.extension filename = "")

(** Persistent representation (for caching) *)

exception BAD_CHECKSUM

type cached = t                                        (* same representation *)

let to_cache config =
  { config with toml_handle = Ezr_toml.cacheable config.toml_handle }

let of_cache ~config_filename ({ toml_handle; _ } as config) =
  if Ezr_toml.checksum toml_handle <> Digest.file config_filename
  then raise BAD_CHECKSUM;
  Ezr_toml.add_section_update toml_handle
    config_section_name (config_repr config);
  config
