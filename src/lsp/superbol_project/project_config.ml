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
    mutable dialect_name: string;
    mutable source_format: Cobol_config.source_format_spec;
    mutable source_format_by_extension:
      (string * Cobol_config.source_format_spec) list;
    mutable libpath: path list;
    mutable libexts: string list;
    mutable indent_config: (string * int) list;
    mutable exclude_dirs: string list;
    mutable source_encoding: Cobol_common.Text_encoding.t option;
    toml_handle: Ezr_toml.toml_handle;
  }

  exception ERROR of Project_diagnostics.error

end
include TYPES
type t = config

(* --- *)

let __init_default_exn_printers =
  Printexc.register_printer begin function
    | ERROR e ->
        Some (Pretty.to_string "%a" Project_diagnostics.pp_error e)
    | _ ->
        None
  end

(* --- *)

(* Intermediate converters *)

let cobol_config_n_file_from_dialect_name ~platform ~verbose dialect_name =
  try
    Cobol_config.(from_dialect_n_file ~platform ~verbose @@
                  DIALECT.of_string dialect_name)
  with
  | Invalid_argument e ->
      raise @@ ERROR (Unknown_dialect e)
  | Cobol_config.ERROR e ->
      raise @@ ERROR (Cobol_config_error e)

let cobol_config_from_dialect_name ~platform ~verbose dialect_name =
  DIAGS.map_result ~f:fst @@
  cobol_config_n_file_from_dialect_name ~platform ~verbose dialect_name

let cobol_source_format source_format_name =
  try Cobol_config.Options.format_of_string source_format_name
  with Invalid_argument e ->
    raise @@ ERROR (Unknown_source_format e)

(* --- *)

let default_libpath = [RelativeToProjectRoot "."]
let default_libexts = Cobol_common.Copybook.copybook_extensions
let default_indent_config = []
let default_source_format_by_extension = []
let default_exclude_dirs = ["_build"; "_opam"; "node_modules"]

let default_dialect_name = "default"

let default = {
  cobol_config = Cobol_config.default;
  dialect_name = default_dialect_name;
  source_format = Cobol_config.Auto;
  source_format_by_extension = default_source_format_by_extension;
  libpath = default_libpath;
  libexts = default_libexts;
  indent_config = default_indent_config;
  exclude_dirs = default_exclude_dirs;
  source_encoding = None;
  toml_handle = Ezr_toml.make_empty ();
}

let new_default () =
  { default with toml_handle = Ezr_toml.make_empty () }

(* Translation to TOML *)

let dialect_repr dialect_name =
  TOML.value_of_string dialect_name

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

let libexts_repr libexts =
  TOML.value_of_strings @@ Array.of_list libexts

let exclude_dirs_repr exclude_dirs =
  TOML.value_of_strings @@ Array.of_list exclude_dirs

let source_format_by_extension_entry_repr (ext, fmt) =
  TOML.table_of_list [
    "extension", TOML.string ext;
    "source-format", TOML.string @@
    Cobol_config.Options.string_of_format fmt;
  ]

let source_format_by_extension_repr entries =
  TOML.value_of_array @@ Array.of_list @@
  List.map source_format_by_extension_entry_repr entries

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
        (dialect_repr config.dialect_name);

      option
        ~name: "source-format"
        ~after_comments: ["Default source reference-format"]
        (format_repr config.source_format);

      option
        ~name: "source-format-by-extension"
        ~after_comments: ["Per-extension source reference-format overrides"]
        (source_format_by_extension_repr config.source_format_by_extension);

      option
        ~name: "copybooks"
        ~after_comments: ["Where to find copybooks"]
        (libpath_repr config.libpath);

      option
        ~name: "copyexts"
        ~after_comments: ["Copybook filename extensions"]
        (libexts_repr config.libexts);

      option
        ~name: "indent"
        ~after_comments: ["Indenter configuration"]
        (indent_repr config.indent_config);

      option
        ~name: "exclude-dirs"
        ~after_comments: ["Directories that hold no source file to consider \
                           in project-wide operations: names of directories \
                           (anywhere in the project), or paths relative to \
                           the project root"]
        (exclude_dirs_repr config.exclude_dirs)
    ]


let config_section_name = "cobol"

(*  *)

let get_source_format toml =
  try cobol_source_format @@ TOML.get_string toml ["source-format"]
  with Not_found ->
    default.source_format

let get_dialect toml =
  TOML.get_string toml ["dialect"] ~default:default_dialect_name

let get_source_format_by_extension_entry toml =
  let ext = TOML.get_string toml ["extension"] in
  let fmt = cobol_source_format @@ TOML.get_string toml ["source-format"] in
  (ext, fmt)

let get_source_format_by_extension toml =
  try
    List.map get_source_format_by_extension_entry @@
    Array.to_list @@ TOML.get_array toml ["source-format-by-extension"]
  with Not_found -> default_source_format_by_extension

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

let get_libexts toml =
  try Array.to_list @@ TOML.get_strings toml ["copyexts"]
  with Not_found -> default_libexts

let get_exclude_dirs toml =
  try Array.to_list @@ TOML.get_strings toml ["exclude-dirs"]
  with Not_found -> default_exclude_dirs

let get_indent_config toml =
  try
    EzCompat.StringMap.fold (fun name node v ->
      (name, TOML.extract_int node) :: v
    ) (TOML.get_table toml ["indent"]) []
  with Not_found -> default_indent_config


(* TODO: the functions below show return a list of [Project_diagnostics.error].
   This shall be easier to achieve after symbolization of diagnostics in
   [Cobol_config].

   For now, many of the `get_*` functions just raise {!ERROR}. *)

let load_file ~platform ?(verbose=false) config_filename =
  let load_section toml_handle keys toml =
    let section = TOML.get keys toml in
    let dialect_name = get_dialect section in
    let DIAGS.{ result = cobol_config; diags } =
      cobol_config_from_dialect_name ~platform ~verbose dialect_name in
    DIAGS.result ~diags
      { cobol_config;
        dialect_name;
        toml_handle;
        source_format = get_source_format section;
        source_format_by_extension = get_source_format_by_extension section;
        libpath = get_libpath section;
        libexts = get_libexts section;
        indent_config = get_indent_config section;
        exclude_dirs = get_exclude_dirs section;
        source_encoding = None }
  in
  try
    let toml_handle = Ezr_toml.load ~verbose config_filename in
    let DIAGS.{ result; _ } as config =
      let toml = Ezr_toml.toml toml_handle in
      try load_section toml_handle toml [config_section_name]
      with Not_found -> DIAGS.result { default with toml_handle }
    in
    Ezr_toml.add_section_update toml_handle
      config_section_name (config_repr result);
    config
  with TOML.Types.Error (loc, _code, error) ->
    raise @@ ERROR (Invalid_toml { loc; error })


let save ?verbose ~config_filename config =
  Ezr_toml.save ?verbose config_filename config.toml_handle


let reload ~platform ?(verbose=false) ~config_filename config =
  let reload_section keys toml =
    (* Note: we only mutate [config] when items have been properly
       parsed/checked, so that [config] remains unchanged in case an exception
       is thrown. *)
    let section = TOML.get keys toml in
    let dialect_name = get_dialect section in
    let DIAGS.{ result = cobol_config; diags } =
      cobol_config_from_dialect_name ~platform ~verbose dialect_name
    and source_format = get_source_format section
    and source_format_by_extension = get_source_format_by_extension section
    and libpath = get_libpath section
    and libexts = get_libexts section
    and indent_config = get_indent_config section
    and exclude_dirs = get_exclude_dirs section in
    let changed =
      config.source_format <> source_format ||
      config.source_format_by_extension <> source_format_by_extension ||
      config.libpath <> libpath ||
      config.libexts <> libexts ||
      config.indent_config <> indent_config ||
      config.dialect_name <> dialect_name ||
      config.exclude_dirs <> exclude_dirs ||
      config.cobol_config <> cobol_config
    in
    config.cobol_config <- cobol_config;
    config.dialect_name <- dialect_name;
    config.source_format <- source_format;
    config.source_format_by_extension <- source_format_by_extension;
    config.libpath <- libpath;
    config.libexts <- libexts;
    config.indent_config <- indent_config;
    config.exclude_dirs <- exclude_dirs;
    DIAGS.result changed ~diags
  in
  try
    Ezr_toml.reload ~verbose config_filename config.toml_handle;
    let toml = Ezr_toml.toml config.toml_handle in
    try
      reload_section toml [config_section_name]
    with Not_found ->                         (* missing section: use defaults *)
      config.cobol_config <- default.cobol_config;
      config.dialect_name <- default.dialect_name;
      config.source_format <- default.source_format;
      config.source_format_by_extension <-
        default.source_format_by_extension;
      config.libpath <- default.libpath;
      config.libexts <- default.libexts;
      config.indent_config <- default.indent_config;
      config.exclude_dirs <- default.exclude_dirs;
      DIAGS.result true
  with TOML.Types.Error (loc, _code, error) ->
    raise @@ ERROR (Invalid_toml { loc; error })


(* --- *)


(** Returns the source format for the given file: checks for a per-extension
    override first, and falls back to the global [source_format] setting. *)
let source_format_for ~filename config =
  let ext = Filename.extension filename in
  let ext =
    if ext <> ""
    then String.sub ext 1 (String.length ext - 1)
    else ext
  in
  (* search for user-defined association *)
  match List.assoc_opt (String.lowercase_ascii ext)
          config.source_format_by_extension with
  | Some fmt -> fmt
  | None -> config.source_format (* fallback to global setting *)

let libpath_for ~filename { libpath; _ } =
  List.map begin function
    | RelativeToProjectRoot str -> str
    | RelativeToFileDir str -> Filename.dirname filename // str
  end libpath

let excludes_dir ~relative_path { exclude_dirs; _ } =
  let trim s =
    let s = if String.starts_with ~prefix:"./" s
      then String.sub s 2 (String.length s - 2) else s in
    if String.length s > 1 && String.ends_with ~suffix:"/" s
    then String.sub s 0 (String.length s - 1) else s
  in
  let relative_path = trim relative_path in
  let name = Filename.basename relative_path in
  List.exists begin fun entry ->
    let entry = trim entry in
    if String.contains entry '/'
    then String.equal entry relative_path
    else String.equal entry name
  end exclude_dirs

let copybook_lookup_config_for ~filename config =
  Cobol_common.Copybook.lookup_config (libpath_for ~filename config)
    ~libexts: config.libexts


(** Persistent representation (for caching) *)


exception BAD_CHECKSUM

(* The COBOL configuration is deliberately absent here, and kept as the name of
   the dialect it is built from instead.  It is a first-class module full of
   objects and closures, and serialising those ties the result to the
   byte-identical executable -- something neither the producer tag nor anything
   else a cache can observe is able to establish.  Reading such an item back in
   another build is undefined behaviour rather than a miss, which a cache
   directory shared between users would run into routinely.

   Rebuilding it from the dialect costs nothing next to what caching saves, and
   is what the configuration file itself is read as: a dialect name is the only
   thing a COBOL configuration is ever built from here. *)
type cached =
  {
    cached_dialect: string;
    cached_source_format: Cobol_config.source_format_spec;
    cached_source_format_by_extension:
      (string * Cobol_config.source_format_spec) list;
    cached_libpath: path list;
    cached_libexts: string list;
    cached_indent_config: (string * int) list;
    cached_exclude_dirs: string list;
    cached_toml_handle: Ezr_toml.cacheable;
  }

(* The dialect is cached as configured, so that a configuration read from a
   file is read from that file again.  The encoding of source files is not
   cached, as it does not come from the configuration file. *)
let to_cache { dialect_name; source_format; source_format_by_extension;
               libpath; libexts; indent_config; exclude_dirs; toml_handle;
               cobol_config = _; source_encoding = _ } =
  {
    cached_dialect = dialect_name;
    cached_source_format = source_format;
    cached_source_format_by_extension = source_format_by_extension;
    cached_libpath = libpath;
    cached_libexts = libexts;
    cached_indent_config = indent_config;
    cached_exclude_dirs = exclude_dirs;
    cached_toml_handle = Ezr_toml.cacheable toml_handle;
  }

let of_cache ~platform ~config_filename
    { cached_dialect; cached_source_format; cached_source_format_by_extension;
      cached_libpath; cached_libexts; cached_indent_config;
      cached_exclude_dirs; cached_toml_handle = toml_handle } =
  if Ezr_toml.checksum toml_handle <> Digest.file config_filename
  then raise BAD_CHECKSUM;
  let DIAGS.{ result = cobol_config; _ } =
    cobol_config_from_dialect_name ~platform ~verbose:false cached_dialect in
  let config =
    { cobol_config;
      dialect_name = cached_dialect;
      source_format = cached_source_format;
      source_format_by_extension = cached_source_format_by_extension;
      libpath = cached_libpath;
      libexts = cached_libexts;
      indent_config = cached_indent_config;
      exclude_dirs = cached_exclude_dirs;
      source_encoding = None;                (* set anew by whoever loads it *)
      toml_handle }
  in
  Ezr_toml.add_section_update toml_handle
    config_section_name (config_repr config);
  config
