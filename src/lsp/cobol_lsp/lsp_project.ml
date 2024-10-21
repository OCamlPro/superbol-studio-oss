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

module SET = struct
  include Superbol_project.SET
  let for_ ~uri = for_ ~filename:(Lsp.Uri.to_path uri)
end
module MAP = Superbol_project.MAP

(* --- *)

let rootdir = Superbol_project.rootdir
let config = Superbol_project.config
let string_of_rootdir = Superbol_project.string_of_rootdir
let rooturi project = Lsp.Uri.of_path @@ string_of_rootdir @@ rootdir project

let rootdir_for ~uri ~layout =
  Superbol_project.rootdir_for ~filename:(Lsp.Uri.to_path uri) ~layout

let show_diagnostics ?(force = false) project diags : unit =
  if force || diags <> DIAGS.Set.none then
    Lsp_diagnostics.publish @@
    Lsp_diagnostics.translate diags
      ~focus_on_main_doc: false
      ~rootdir:(Superbol_project.string_of_rootdir project.rootdir)
      ~uri:(Lsp.Uri.of_path project.config_filename)

let show_n_forget_diagnostics ?force { result = project; diags } =
  show_diagnostics ?force project diags;
  project

let on_project_config_error e ~rootdir ~layout =
  Lsp_io.log_error "Error@ in@ project@ configuration:@ resorting@ to@ defaults.";
  Lsp_io.log_info "Cause:@ %a" Superbol_project.Diagnostics.pp_error e;
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

let copybook_lookup_config_for ~uri project =
  Superbol_project.copybook_lookup_config_for ~filename:(Lsp.Uri.to_path uri)
    project

let detect_copybook ~uri ?contents project =
  Superbol_project.detect_copybook ~filename:(Lsp.Uri.to_path uri)
    ?contents project

let relative_path_for ~uri project =
  Superbol_project.relative_path_for ~filename:(Lsp.Uri.to_path uri) project

let absolute_path_for =
  Superbol_project.absolute_path_for

(** Configuration management

    {e Warning}: functions below that return a Boolean actually perform some
    mutations on project's configurations. They return [true] if the
    configuration of the given project has changed. *)

let update_source_format { config; _ } str : bool =
  try
    let source_format = Cobol_config.Options.format_of_string str in
    if source_format = config.source_format
    then false
    else (config.source_format <- source_format; true)
  with Invalid_argument e ->
    Lsp_io.notify_error "%a"
      Superbol_project.Diagnostics.pp_error (Unknown_source_format e);
    false

let update_dialect ({ config; _ } as project) str : bool =
  try
    let { result; diags } =
      Superbol_project.Config.cobol_config_from_dialect_name str
        ~verbose:false
    in
    if result = config.cobol_config            (* note: structural comparison *)
    then false
    else begin
      config.cobol_config <- result;
      show_diagnostics project diags;
      true
    end
  with Superbol_project.Config.ERROR e ->
    Lsp_io.notify_error "%a" Superbol_project.Diagnostics.pp_error e;
    false

let update_copybooks: t -> Yojson.Safe.t -> bool = fun { config; _ } json ->
  let open Yojson.Safe.Util in
  let to_libdir s =
    let dir = to_string @@ member "dir" s
    and file_relative = member "file-relative" s in
    if file_relative <> `Null && to_bool file_relative
    then RelativeToFileDir dir
    else RelativeToProjectRoot dir
  in
  try
    let libpath = convert_each to_libdir json in
    if libpath = config.libpath                (* note: structural comparison *)
    then false
    else (config.libpath <- libpath; true)
  with
    Yojson.Safe.Util.(Type_error _ | Undefined _) as e ->
      Pretty.invalid_arg "%s: %a" (Yojson.Safe.to_string json) Fmt.exn e

let update_copyexts: t -> Yojson.Safe.t -> bool = fun { config; _ } json ->
  let open Yojson.Safe.Util in
  try
    let libexts = convert_each to_string json in
    if libexts = config.libexts                (* note: structural comparison *)
    then false
    else (config.libexts <- libexts; true)
  with
    Yojson.Safe.Util.(Type_error _ | Undefined _) as e ->
      Pretty.invalid_arg "%s: %a" (Yojson.Safe.to_string json) Fmt.exn e

(** [update_project_config assoc project] updates the configuration of [project]
    according to key/value paires in [assoc]; returns [true] whenever the
    configuration upon termination differs from the configuration upon call. *)
let update_project_config assoc project : bool =
  let from_string project (s: Yojson.Safe.t) ~f : bool =
    match s with
    | `String "" -> false
    | `String s -> f project s
    | _ -> Pretty.invalid_arg "%s" (Yojson.Safe.to_string s)
  in
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
    "dialect", from_string ~f:update_dialect;
    "source-format", from_string ~f:update_source_format;
    "copybooks", update_copybooks;
    "copyexts", update_copyexts;
  ]


(** Returns [true] whenever the configuration has changed. *)
let reload_project_config project =
  try
    let { result = changed; diags } = Superbol_project.reload_config project in
    show_diagnostics ~force:true project diags;
    changed
  with
  | Superbol_project.Config.ERROR (Cobol_config_error _ as e) ->
      Lsp_io.notify_error "%a" Superbol_project.Diagnostics.pp_error e;
      false
  | Superbol_project.Config.ERROR e ->
      show_diagnostics ~force:true project @@
      DIAGS.Set.error "%a" Superbol_project.Diagnostics.pp_error e;
      false


(* --- *)


let get_project_config ?(flat = true) project : Yojson.Safe.t =
  let config = Superbol_project.config project in
  let module Config = (val config.cobol_config) in
  let copybooks =
    List.map begin function
      | RelativeToProjectRoot dir ->
          `Assoc ["dir", `String dir]
      | RelativeToFileDir dir ->
          `Assoc ["dir", `String dir; "file-relative", `Bool true]
    end config.libpath
  in
  let cobol =
    [
      "dialect",
      `String (Cobol_config.DIALECT.to_string Config.dialect);

      "source-format",
      `String (Cobol_config.Options.string_of_format config.source_format);

      "copybooks",
      `List copybooks;

      "copyexts",
      `List (List.map (fun s -> `String s) config.libexts);
    ]
  in
  if flat
  then `Assoc (List.map (fun (k, v) -> "cobol."^k, v) cobol)
  else `Assoc ["cobol", `Assoc cobol]

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
