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

open Cobol_common.Diagnostics.TYPES

module DIAGS = Cobol_common.Diagnostics
module TYPES = struct
  include Superbol_project.Config.TYPES
  include Superbol_project.TYPES
end

include TYPES
type t = project

module SET = Superbol_project.SET
module MAP = Superbol_project.MAP

(* --- *)

let rootdir = Superbol_project.rootdir
let config = Superbol_project.config
let string_of_rootdir = Superbol_project.string_of_rootdir
let rooturi project = Lsp.Uri.of_path @@ string_of_rootdir @@ rootdir project

let rootdir_for ~uri ~layout =
  Superbol_project.rootdir_for ~filename:(Lsp.Uri.to_path uri) ~layout

let show_n_forget_diagnostics ?(force = false) { result = project; diags } =
  if force || diags <> DIAGS.Set.none then
    Lsp_diagnostics.publish @@
    Lsp_diagnostics.translate diags
      ~focus_on_main_doc: false
      ~rootdir:(Superbol_project.string_of_rootdir project.rootdir)
      ~uri:(Lsp.Uri.of_path project.config_filename);
  project

let on_project_config_error e ~rootdir ~layout =
  Lsp_io.pretty_notification ~type_:Error
    "Error@ in@ project@ configuration@ (%a):@ resorting@ to@ system@ defaults.\
    " Superbol_project.Diagnostics.pp_error e;
  let diags = DIAGS.Set.error "%a" Superbol_project.Diagnostics.pp_error e in
  show_n_forget_diagnostics ~force:true @@
  DIAGS.result ~diags @@ Superbol_project.with_default_config ~rootdir ~layout

let for_ ~rootdir ~layout =
  try
    show_n_forget_diagnostics @@
    Superbol_project.for_ ~rootdir ~layout
  with Superbol_project.Config.ERROR e ->
    on_project_config_error e ~rootdir ~layout

let in_existing_dir dirname ~layout =
  for_ ~rootdir:(Superbol_project.rootdir_at ~dirname) ~layout

let libpath_for ~uri project =
  Superbol_project.libpath_for ~filename:(Lsp.Uri.to_path uri) project

let detect_copybook ~uri project =
  Superbol_project.detect_copybook ~filename:(Lsp.Uri.to_path uri) project

let relative_path_for ~uri project =
  Superbol_project.relative_path_for ~filename:(Lsp.Uri.to_path uri) project

let absolute_path_for =
  Superbol_project.absolute_path_for

(** Config *)

let update_from_string project (s: Yojson.Safe.t) ~f : bool =
  match s with
  | `String "" -> false
  | `String s -> f project s
  | _ -> Pretty.invalid_arg "%s" (Yojson.Safe.to_string s)

let update_source_format: t -> Yojson.Safe.t -> bool =
  update_from_string ~f:begin fun { config; _ } s ->
    let source_format = Cobol_config.Options.format_of_string s in
    if source_format = config.source_format
    then false
    else begin
      config.source_format <- source_format;
      true
    end
  end

let update_dialect: t -> Yojson.Safe.t -> bool =
  update_from_string ~f:begin fun ({ config; _ } as project) s ->
    let { result; diags } =
      Superbol_project.Config.cobol_config_from_dialect_name s in
    if result = config.cobol_config            (* note: structural comparison *)
    then false
    else begin
      config.cobol_config <- result;
      ignore @@ show_n_forget_diagnostics { result = project; diags };
      true
    end
  end

(** [update_project_config assoc project] updates the configuration of [project]
    according to key/value paires in [assoc]; returns [true] whenever the
    configuration upon termination differs from the configuration upon call. *)
let update_project_config assoc project =
  let update_config assoc key update project =
    try match List.assoc_opt key assoc with
      | None -> false
      | Some v -> update project v
    with Invalid_argument msg ->
      Lsp_io.log_error "Invalid@ value@ for@ configuration@ item@ %s:@ %s"
        key msg;
      false
  in
  List.fold_left begin fun u (key, update) ->
    update_config assoc key update project || u
  end false [
    "dialect", update_dialect;
    "source-format", update_source_format;
  ]

(** Caching *)

type cached = Superbol_project.cached

let to_cache project =
  Superbol_project.to_cache project

let of_cache ~rootdir ~layout cached =
  try
    show_n_forget_diagnostics @@
    Superbol_project.of_cache ~rootdir ~layout cached
  with Superbol_project.Config.ERROR e ->
    on_project_config_error e ~rootdir ~layout
