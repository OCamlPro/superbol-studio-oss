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

module ANALYSIS = Superbol_project.Analysis
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

(* --- *)

let cache_for ~config ~rootdir : CACHE.t =
  config.lsp_cache_for ~rootdir:(Lsp_project.string_of_rootdir rootdir)

(* --- Keys --- *)

let project_key ~rootdir =
  CACHE.shallow_key ~label:"project"
    ~inputs:[Digest.string @@ Lsp_project.string_of_rootdir rootdir]

(* Each document has two entries: its analysis for the LSP, which other
   front-ends may reuse, and LSP-specific data.  Both are keyed on the same inputs, and the latter
   depends on the former, so that any miss on the analysis entails a miss on the
   LSP one. *)
let document_keys ~project ~filename ~contents ~language_id =
  let overrides = Lsp_document.analysis_overrides ~language_id in
  match ANALYSIS.key_for_lsp ~overrides ~filename ~contents project,
        ANALYSIS.inputs ~overrides ~filename ~contents project with
  | Some analysis_key, Some inputs ->
      Some (analysis_key, CACHE.shallow_key ~label:"lsp-document" ~inputs)
  | _ ->
      None

(* --- Saving --- *)

(* Keyed on what the editor is showing, which is what the cached artifacts were
   actually parsed from -- and not on what the file holds, which may still differ
   from it.  Since a document is restored by reading the file, keying it on
   anything else would make it restorable in a state it was never parsed in. *)
let store_document ~cache ~project doc =
  let analysis, witnesses, cached = Lsp_document.to_cache doc in
  let filename = cached.doc_cache_filename in
  let absolute = Lsp_project.absolute_path_for ~filename project in
  let contents = Lsp.Text_document.text doc.Lsp_document.textdoc in
  let language_id = Lsp_document.language_id doc in
  match document_keys ~project ~filename:absolute ~contents ~language_id with
  | None -> None
  | Some (analysis_key, lsp_key) ->
      let stored_analysis =
        match witnesses with
        | None -> Ok ()        (* restored from cache: not stored over again *)
        | Some witnesses ->
            ANALYSIS.store_for_lsp cache ~key:analysis_key ~label:filename ~witnesses
              analysis
      in
      match
        Result.bind stored_analysis @@ fun () ->
        CACHE.store cache ~key:lsp_key ~label:filename
          ~witnesses:[WITNESS.sub analysis_key]
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

(* The analysis entry may have been stored by another front-end, in which case
   there is no LSP entry yet: the document is restored all the same, and the
   LSP entry is written whenever the document is next stored. *)
let load_document ~project ~params
    ~doc:(Lsp.Types.DidOpenTextDocumentParams.{ textDocument; _ } as doc) =
  let Lsp_project.{ rootdir; _ } = project in
  let cache = cache_for ~config:params.config ~rootdir in
  let filename = Lsp.Uri.to_path textDocument.uri in
  let resolve =
    Superbol_cache.Recorder.resolver ~platform:Lsp_platform.record ()
  in
  match
    document_keys ~project ~filename ~contents:textDocument.text
      ~language_id:textDocument.languageId
  with
  | None ->
      None
  | Some (analysis_key, lsp_key) ->
      match ANALYSIS.lookup_for_lsp cache ~key:analysis_key ~resolve with
      | Some analysis ->
          let cached =
            match
              CACHE.lookup cache ~key:lsp_key ~resolve
                ~read_item:CACHING.read_item
            with
            | Ok cached -> cached
            | Error _ -> None
          in
          let doc = Lsp_document.of_cache ~project ~params ~doc analysis cached in
          if params.config.lsp_cache_verbose then
            Lsp_io.log_info "Successfully@ read@ document@ cache@ for@ %s"
              (Lsp.Uri.to_string @@ Lsp_document.uri doc);
          Some doc
      | None ->
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
