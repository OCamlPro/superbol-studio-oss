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
open Toml.Types

module DIAGS = Cobol_common.Diagnostics

module TYPES = struct

  type path =
    | RelativeToProjectRoot of string
    | RelativeToFileDir of string

  type config = {                                                  (* private *)
    config_checksum: Digest.t;
    cobol_config: Cobol_config.t;
    source_format: Cobol_config.source_format_spec;
    libpath: path list;
    copybook_extensions: string list;
    copybook_if_no_extension: bool;
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

let default = {
  config_checksum = Digest.string "";
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

let config_from_dialect_name ?(strict = false) dialect_name =
  try Cobol_config.(from_dialect ~strict @@ DIALECT.of_string dialect_name) with
  | Invalid_argument e ->
      raise @@ ERROR (Unknown_dialect e)
  | Cobol_config.ERROR e ->
      raise @@ ERROR (Cobol_config_error e)

let from_file ~config_filename =
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
  let load config_filename =
    let config_string = EzFile.read_file config_filename in
    let config_checksum = Digest.string config_string in
    match Toml.Parser.from_string config_string with
    | `Ok toml ->
        let DIAGS.{ result = config; diags = errors } =
          match Table.find_opt dialect_key toml,
                Table.find_opt strict_key toml with
          | Some TString dialect, Some TBool strict ->
              config_from_dialect_name dialect ~strict
          | Some TString dialect, None ->
              config_from_dialect_name dialect
          | None, (None | Some TBool _) ->   (* match on strict so it is not caught
                                                in last case *)
              DIAGS.result Cobol_config.default
          | Some _, _ ->
              DIAGS.result Cobol_config.default
                ~diags:(expected_string ~key:dialect_key DIAGS.Set.none)
          | _, Some _ ->
              DIAGS.result Cobol_config.default
                ~diags:(expected_bool ~key:strict_key DIAGS.Set.none)
        in
        let libpath, errors =
          match Table.find copybook_key toml with
          | TArray NodeTable tbl ->
              copybook_entries tbl
          | exception Not_found ->
              default_libpath, errors
          | _ ->
              default_libpath,
              expected ~key:copybook_key ~kind:"array of tables" errors
        in
        let source_format, errors =
          match Table.find source_format_key toml with
          | TString str ->
              (try
                 Cobol_config.Options.format_of_string str, errors
               with Invalid_argument _ ->
                 Auto,
                 expected ~key:source_format_key ~kind:"source format" errors)
          | exception Not_found ->
              Auto, errors
          | _ ->
              Auto, expected_string ~key:source_format_key errors
        in
        {
          config_checksum;
          cobol_config = config;
          source_format;
          libpath;
          copybook_extensions = default_copybook_extensions;
          copybook_if_no_extension = default_copybook_if_no_extension;
        }, errors
    | `Error (msg, loc) ->
        let loc = { loc with source = config_filename } in
        raise @@ ERROR (Invalid_config { loc = Toml_loc loc; msg })
  in
  let config, diags =
    try load config_filename with
    | Toml.Parser.Error (msg, loc) ->
        let loc = { loc with source = config_filename } in
        raise @@ ERROR (Invalid_config { loc = Toml_loc loc; msg })
    | Sys_error _ as e ->
        raise @@ ERROR (Invalid_config { loc = Toml_file config_filename;
                                         msg = Pretty.to_string "%a" Fmt.exn e})
  in
  DIAGS.result ~diags config

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
let to_cache = Fun.id

let of_cache ~config_filename ({ config_checksum; _ } as config) =
  if config_checksum = Digest.file config_filename
  then config
  else raise BAD_CHECKSUM
