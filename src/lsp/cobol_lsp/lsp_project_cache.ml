(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)


open Lsp_imports
open Lsp_types

module CACHE = Superbol_cache.Cache
module CACHING = Superbol_cache.Caching
module WITNESS = Superbol_cache.Witness

(* --- *)

(** What is held for a project: the project itself.  The documents' own entries
    are separate, so that editing one of them costs a miss on that one alone. *)
type project_index =
  {
    project_data: Lsp_project.cached;
  }

(* Code: *)

let cache_for ~config ~rootdir : CACHE.t =
  config.lsp_cache_for ~rootdir:(Lsp_project.string_of_rootdir rootdir)

(* --- Keys --- *)

(* Everything about the project that bears on how a document is preprocessed and
   parsed.  It takes part in every document key rather than being witnessed,
   because settings pushed by the client never reach a file we could witness. *)
let config_digest project =
  let config = Lsp_project.config project in
  match
    Marshal.to_string
      (Cobol_config.DIALECT.to_string @@
       Cobol_config.dialect config.cobol_config,
       config.source_format,
       config.source_format_by_extension,
       config.libpath,
       config.libexts) []
  with
  | s -> Some (Digest.string s)
  | exception Invalid_argument _ ->
      None              (* nothing here should be functional, but if ever *)

let project_key ~rootdir =
  CACHE.shallow_key ~label:"project"
    ~inputs:[Digest.string @@ Lsp_project.string_of_rootdir rootdir]

(* The document's contents take part in the key, rather than being witnessed:
   entries are published without ever replacing one another, so a document that
   is edited has to land under a key of its own. *)
let document_key ~project ~filename ~contents =
  Option.map begin fun config_digest ->
    CACHE.shallow_key ~label:"document"
      ~inputs:[Digest.string @@ WITNESS.canonical_path filename;
               Digest.string contents;
               config_digest]
  end (config_digest project)

(* --- Saving --- *)

(* Keyed on what the editor is showing, which is what the cached artifacts were
   actually parsed from -- and not on what the file holds, which may still differ
   from it.  Since a document is restored by reading the file, keying it on
   anything else would make it restorable in a state it was never parsed in. *)
let store_document ~cache ~project doc =
  let cached = Lsp_document.to_cache doc in
  let filename = cached.doc_cache_filename in
  let absolute = Lsp_project.absolute_path_for ~filename project in
  let contents = Lsp.Text_document.text doc.Lsp_document.textdoc in
  match document_key ~project ~filename:absolute ~contents with
  | None -> None
  | Some key ->
      match
        CACHE.store cache ~key ~label:filename
          ~witnesses:cached.doc_cache_witnesses
          ~write_item:CACHING.write_item cached
      with
      | Ok () -> Some filename
      | Error _ -> None

let store_project ~cache ~quiet (Lsp_project.{ rootdir; _ } as project) =
  match
    CACHE.store cache ~key:(project_key ~rootdir) ~label:"project"
      ~witnesses:[]        (* the configuration file is checked on the way in *)
      ~write_item:CACHING.write_item
      { project_data = Lsp_project.to_cache project }
  with
  | Ok () ->
      if not quiet then
        Lsp_io.log_info "Wrote cache for: %s"
          (Lsp_project.string_of_rootdir rootdir)
  | Error _ ->
      ()

let save_project ~config (Lsp_project.{ rootdir; _ } as project) =
  let cache = cache_for ~config ~rootdir in
  store_project ~cache ~quiet:false project;
  (* This is a rare enough operation for this moment to be worth seeing whether
     the cache still fits.  Only here, and not on every document saved: pruning
     walks the whole of it. *)
  match CACHE.prune cache ~policy:config.lsp_cache_pruning_policy with
  | Ok 0 | Error _ ->
      ()
  | Ok reclaimed ->
      Lsp_io.log_info "Reclaimed@ %d@ cache@ entries@ for:@ %s" reclaimed
        (Lsp_project.string_of_rootdir rootdir)

let do_save ~config docs =
  (* Pivot all active projects: associate projects with all their documents, and
     ignore any project that has none. *)
  URIMap.fold begin fun _ Lsp_document.{ project; _ } projects ->
    Lsp_project.SET.add project projects
  end docs Lsp_project.SET.empty |>
  Lsp_project.SET.iter (save_project ~config)

(* --- Saving one document --- *)

let do_save_document ~config (Lsp_document.{ project; _ } as doc) =
  let Lsp_project.{ rootdir; _ } = project in
  let cache = cache_for ~config ~rootdir in
  match store_document ~cache ~project doc with
  | None -> ()
  | Some filename ->
      if config.lsp_cache_verbose then
        Lsp_io.log_info "Wrote@ document@ cache@ for@ %s" filename

let save_document ~params:{ config; _ } doc =
  try do_save_document ~config doc with
  | Unix.Unix_error (e, op, args) ->
      if config.lsp_cache_verbose then
        Lsp_io.log_debug "While@ saving@ document@ cache:@ %s %s:@ %s"
          op args (Unix.error_message e)
  | e ->
      Lsp_error.internal
        "Exception@ caught@ while@ saving@ document@ cache:@ %a@." Fmt.exn e

let save ~params:{ config; _ } docs =
  try do_save ~config docs with
  | Unix.Unix_error (e, op, args) ->
      if config.lsp_cache_verbose then
        Lsp_io.log_debug "While@ saving@ project@ caches:@ %s %s:@ %s"
          op args (Unix.error_message e)
  | e ->
      Lsp_error.internal
        "Exception@ caught@ while@ saving@ project@ caches:@ %a@." Fmt.exn e

(* --- Loading --- *)

let load_document ~project ~params ~filename ~contents =
  let Lsp_project.{ rootdir; _ } = project in
  let cache = cache_for ~config:params.config ~rootdir in
  let absolute = Lsp_project.absolute_path_for ~filename project in
  let resolve =
    Superbol_cache.Recorder.resolver ~platform:Lsp_platform.record ()
  in
  match document_key ~project ~filename:absolute ~contents with
  | None ->
      None
  | Some key ->
      match CACHE.lookup cache ~key ~resolve ~read_item:CACHING.read_item with
      | Ok Some cached ->
          let doc = Lsp_document.of_cache ~project ~params ~contents cached in
          if params.config.lsp_cache_verbose then
            Lsp_io.log_info "Successfully@ read@ document@ cache@ for@ %s"
              (Lsp.Uri.to_string @@ Lsp_document.uri doc);
          Some doc
      | Ok None | Error _ ->
          if params.config.lsp_cache_verbose then
            Lsp_io.log_info "Not@ reading@ document@ cache@ for@ %s" filename;
          None

let load ~rootdir ~layout ~params =
  let cache = cache_for ~config:params.config ~rootdir in
  match
    CACHE.lookup cache ~key:(project_key ~rootdir) ~resolve:(fun _ -> true)
      ~read_item:CACHING.read_item
  with
  | Ok None | Error _ ->
      None
  | Ok Some { project_data } ->
      match Lsp_project.of_cache ~rootdir ~layout project_data with
      | project ->
          Lsp_io.log_info "Successfully@ read@ project@ cache@ for@ %s"
            Lsp.Uri.(to_string @@ Lsp_project.rooturi project);
          Some project
      | exception _ ->                                     (* TODO: avoid this *)
          None
