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

(** This module contains the functions to parse and get the configuration  *)

open Ez_file.V1
open EzFile.OP
open Toml.Types

module DIAGS = Cobol_common.Diagnostics

module TABLE =
  Ephemeron.K1.Make (struct
    include String                                                 (* rootdir *)
    let hash = Hashtbl.hash
  end)

module TYPES = struct

  type path =
    | RelativeToProjectRoot of string
    | RelativeToFileDir of string

  type rootdir = string                                            (* private *)

  type project = {                                                 (* private *)
    rootdir: rootdir;
    config_checksum: Digest.t option;
    cobol_config: Cobol_config.t;
    source_format: Cobol_config.source_format_spec;
    libpath: path list;
    copybook_extensions: string list;
    copybook_if_no_extension: bool;
  }

  type layout = {
    project_config_filename: string;
  }

end
include TYPES
type t = project

let rootdir { rootdir; _ } = rootdir
let string_of_rootdir = Fun.id

type cached = {                   (** Persistent representation (for caching) *)
  cached_config_checksum: Digest.t option;
  cached_cobol_config: Cobol_config.t;
  cached_source_format: Cobol_config.source_format_spec;
  cached_libpath: path list;
  cached_copybook_extensions: string list;
  cached_copybook_if_no_extensions: bool;
}

module M = struct
  type nonrec t = t
  let compare { rootdir = d1; _ } { rootdir = d2; _ } = String.compare d1 d2
end

module SET = struct
  include Set.Make (M)
  let for_rootdir ~rootdir s =
    let p = find_first (fun p -> String.compare p.rootdir rootdir >= 0) s in
    if p.rootdir = rootdir then p else raise Not_found
  let mem_rootdir ~rootdir s =
    try ignore (for_rootdir ~rootdir s); true with Not_found -> false
end

module MAP = Map.Make (M)

let rootdir_for ~uri ~layout:{ project_config_filename; _ } =
  let rec try_dir dir =
    if EzFile.exists (dir // project_config_filename)
    then Some dir
    else
      let new_dir = EzFile.dirname dir in
      if new_dir = dir
      then None                                             (* we are at root *)
      else try_dir new_dir
  in
  let filename = Lsp.Uri.to_path uri in
  let dir = match try_dir (EzFile.dirname filename) with
    | Some dir -> dir
    | None -> EzFile.dirname filename
  in
  (* Pretty.error "Project directory: %s@." dir; *)
  dir

let config_from_dialect_name ?(strict = false) dialect =
  try
    let dialect = Cobol_config.DIALECT.of_string dialect in
    Cobol_common.catch_diagnostics
      (fun diags dialect ->
         Ok (Cobol_config.from_dialect diags ~strict dialect,
             DIAGS.Set.none))
      dialect
  with Invalid_argument e ->
    Error (DIAGS.Set.error "Unknown dialect: %s" e)

let default_libpath = [RelativeToProjectRoot "."]
let default_copybook_extensions = Cobol_preproc.Copybook.copybook_extensions
let default_copybook_if_no_extension = true

let default ~rootdir = {
  rootdir;
  config_checksum = None;
  cobol_config = Cobol_config.default;
  source_format = Cobol_config.(SF SFFixed);
  libpath = default_libpath;
  copybook_extensions = default_copybook_extensions;
  copybook_if_no_extension = default_copybook_if_no_extension;
}

let dialect_key = Table.Key.of_string "dialect"
and source_format_key = Table.Key.of_string "source-format"
and strict_key = Table.Key.of_string "strict"
and dir_key = Table.Key.of_string "dir"
and file_relative_key = Table.Key.of_string "file-relative"
and copybook_key = Table.Key.of_string "copybook"

let expected acc ~kind ~key =
  DIAGS.Acc.error acc
    "Invalid@ entry@ type@ for@ key@ `%s',@ %s@ expected"
    (Table.Key.to_string key) kind
let expected_string = expected ~kind:"string"
let expected_bool = expected ~kind:"Boolean"

(** [read_config_file file] returns the project configuration from the file
    [file].  Errors and warnings raised when parsing this file are directly
    publised via {!Lsp_io}. *)
let read_config_file ~rootdir config_file =
  let copybook_entries table =
    List.fold_left begin fun (libpath, errors) entry ->
      match Table.find_opt dir_key entry,
            Table.find_opt file_relative_key entry with
      | Some TString str, Some TBool true ->
          RelativeToFileDir str :: libpath, errors
      | Some TString str, (None | Some TBool false) ->
          RelativeToProjectRoot str :: libpath, errors
      | Some TString _, Some _ ->
          libpath, expected_bool ~key:file_relative_key errors
      | Some _, _ ->
          libpath, expected_string ~key:dir_key errors
      | None, _ ->
          libpath, errors
    end (default_libpath, DIAGS.Set.none) table
  in
  (* TODO: push notifications directly via {!Lsp_io.pretty_notification} *)
  let config, diags = match Toml.Parser.from_filename config_file with
    | `Ok toml ->
        let config =
          match Table.find_opt dialect_key toml,
                Table.find_opt strict_key toml with
          | Some TString dialect, Some TBool strict ->
              config_from_dialect_name dialect ~strict
          | Some TString dialect, None ->
              config_from_dialect_name dialect
          | None, (None | Some TBool _) ->   (* match on strict so it is not caught
                                                in last case *)
              Ok (Cobol_config.default, DIAGS.Set.none)
          | Some _, _ ->
              Error (expected_string ~key:dialect_key DIAGS.Set.none)
          | _, Some _ ->
              Error (expected_bool ~key:strict_key DIAGS.Set.none)
        in
        let libpath, errors =
          match Table.find copybook_key toml with
          | TArray NodeTable tbl ->
              copybook_entries tbl
          | exception Not_found ->
              default_libpath, DIAGS.Set.none
          | _ ->
              default_libpath,
              expected ~key:copybook_key ~kind:"array of tables" DIAGS.Set.none
        in
        let source_format, sf_errors =
          let none = DIAGS.Set.none in
          match Table.find source_format_key toml with
          | TString str ->
              begin match String.uppercase_ascii str with
                | "FREE"     -> Cobol_config.SF SFFree, none
                | "FIXED"    -> SF SFFixed, none
                | "VARIABLE" -> SF SFVariable, none
                | "XOPEN"    -> SF SFXOpen, none
                | "XCARD"    -> SF SFxCard, none
                | "CRT"      -> SF SFCRT, none
                | "TERMINAL" -> SF SFTrm, none
                | "COBOLX"   -> SF SFCOBOLX, none
                | "AUTO"     -> Auto, none
                | _ -> Auto, DIAGS.Set.error "Invalid source format: %s" str
              end
          | exception Not_found ->
              Auto, none
          | _ ->
              Auto, expected_string ~key:source_format_key none
        in
        let errors = DIAGS.Set.union errors sf_errors in
        let errors =
          match config with
          | Error diags -> DIAGS.Set.union errors diags
          | Ok _ -> errors
        in
        let cobol_config, errors =
          match config with
          | Error _ ->
              Cobol_config.default,
              DIAGS.Acc.warn errors "Unsing the default configuration"
          | Ok (config, diags) ->
              config, DIAGS.Set.union errors diags
        in
        {
          rootdir;
          config_checksum = Some (Digest.file config_file);
          cobol_config;
          source_format;
          libpath;
          copybook_extensions = default_copybook_extensions;
          copybook_if_no_extension = default_copybook_if_no_extension;
        }, errors
    | `Error (msg, _) ->
        default ~rootdir,
        DIAGS.Set.error "Failed to parse `superbol.toml': %s@\n\
                         Using default configuration" msg
    | exception e ->
        default ~rootdir,
        DIAGS.Set.error "Failed to read `superbol.toml': %a@\n\
                         Using default configuration" Fmt.exn e
  in
  Lsp_diagnostics.publish @@
  Lsp_diagnostics.translate diags ~rootdir
    ~uri:(`Main (Lsp.Uri.of_path config_file));
  config

let try_reading_config_file ~layout:{ project_config_filename; _ } ~rootdir =
  let config_filename = rootdir // project_config_filename in
  if EzFile.exists config_filename
  then read_config_file ~rootdir config_filename
  else default ~rootdir

let table = TABLE.create 1

let in_existing_dir dir ~layout =
  if EzFile.is_directory dir then
    let project = try_reading_config_file ~layout ~rootdir:dir in
    TABLE.add table dir project;
    project
  else
    Fmt.invalid_arg "Expected existing directory: %s" dir

let for_ ~rootdir ~layout =
  try TABLE.find table rootdir
  with Not_found ->
    let project = try_reading_config_file ~layout ~rootdir in
    TABLE.add table rootdir project;
    project

let libpath_for ~uri { libpath; _ } =
  List.map begin function
    | RelativeToProjectRoot str -> str
    | RelativeToFileDir str -> Filename.dirname (Lsp.Uri.to_path uri) // str
  end libpath

(* TODO: add config flags to libpath where some directories may only include
   copybooks. *)
let detect_copybook ~uri { copybook_extensions;
                           copybook_if_no_extension; _ } =
  let path = Lsp.Uri.to_path uri in
  List.exists (Filename.check_suffix path) copybook_extensions ||
  (copybook_if_no_extension && Filename.extension path = "")

let to_cache { config_checksum; cobol_config; source_format; libpath;
               copybook_extensions; copybook_if_no_extension; _ } =
  {
    cached_config_checksum = config_checksum;
    cached_cobol_config = cobol_config;
    cached_source_format = source_format;
    cached_libpath = libpath;
    cached_copybook_extensions = copybook_extensions;
    cached_copybook_if_no_extensions = copybook_if_no_extension;
  }

let of_cache ~rootdir ~layout
    { cached_config_checksum = config_checksum;
      cached_cobol_config = cobol_config;
      cached_source_format = source_format;
      cached_libpath = libpath;
      cached_copybook_extensions = copybook_extensions;
      cached_copybook_if_no_extensions = copybook_if_no_extension }
  =
  let project
    = { rootdir; config_checksum; cobol_config; source_format; libpath;
        copybook_extensions; copybook_if_no_extension } in
  let config_filename = rootdir // layout.project_config_filename in
  try match config_checksum with
    | Some checksum when checksum = Digest.file config_filename ->
        TABLE.replace table rootdir project;
        project
    | _ -> raise Exit
  with _ ->                               (* read error or bad checksum (Exit) *)
    let project = try_reading_config_file ~rootdir ~layout in
    TABLE.add table rootdir project;
    project

let relative_path_for ~uri { rootdir; _ } =
  try Lsp_utils.relative_path ~uri rootdir
  with Invalid_argument _ -> Lsp.Uri.to_path uri  (* if not in project rootdir *)

let absolute_path_for ~filename { rootdir; _ } =
  if EzFile.is_absolute filename
  then filename       (* in case the file is not within its project directory *)
  else rootdir // filename
