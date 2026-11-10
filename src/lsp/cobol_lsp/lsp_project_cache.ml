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
open Cobol_common.Platform.TYPES

module ANALYSIS = Superbol_project.Analysis
module CACHED_DIAGS = Superbol_project.Cached_diagnostics
module SEMTOKS = Superbol_project.Semantic_tokens
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
   front-ends may reuse, and LSP-specific data.  Both are keyed on the same
   inputs, and the latter depends on the former, so that any miss on the
   analysis entails a miss on the LSP one.

   Both are recorded in a slot that every version of the document shares, so
   that storing a new version reclaims those of the previous one (see
   {!store_document}).

   Semantic tokens ({!Superbol_project.Semantic_tokens}) are stored on their
   own, depending on the analysis too, and only by servers whose client asks
   for them.  They are recorded in a slot of their own, or else a server that
   does not store them would reclaim those stored by another for the same
   version.  So are workspace symbols ({!Lsp_workspace_symbols}) and
   diagnostics ({!Superbol_project.Cached_diagnostics}), that can then be
   retrieved for documents that are not open without restoring their
   analysis. *)
let document_keys ~project ~filename ~contents ~language_id =
  let overrides = Lsp_document.analysis_overrides ~language_id in
  Option.map begin fun keys ->
    keys,
    CACHE.shallow_key ~label:"lsp-document" ~inputs:(ANALYSIS.inputs keys),
    ANALYSIS.slot ~label:"lsp-document" keys
  end (ANALYSIS.keys ~overrides ~filename ~contents project)

(* --- Saving --- *)

(* Keyed on what the editor is showing, which is what the cached artifacts were
   actually parsed from -- and not on what the file holds, which may still differ
   from it.  Documents are looked up likewise, on the text the editor sends
   when opening them ({!load_document}), so that a document is only ever
   restored for the very text it was parsed from.

   Contents being part of the keys, each version of a document that is stored
   lands under keys of its own; once it is, the entries of whichever version
   was stored before, by this session or an earlier one, are superseded.  The
   CLI records what it stores in slots of its own. *)
let store_document ~cache ~project ~with_semantic_tokens doc =
  let analysis, semtoks, witnesses, cached =
    Lsp_document.to_cache ~with_semantic_tokens doc
  in
  let filename = cached.doc_cache_filename in
  let absolute = Lsp_project.absolute_path_for ~filename project in
  let contents = Lsp.Text_document.text doc.Lsp_document.textdoc in
  let language_id = Lsp_document.language_id doc in
  match document_keys ~project ~filename:absolute ~contents ~language_id with
  | None -> None
  | Some (keys, lsp_key, slot) ->
      let analysis_key = ANALYSIS.key_for_lsp keys in
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
      | Ok () ->
          (* Failing to supersede only leaves the previous version behind. *)
          ignore @@ CACHE.supersede cache ~slot [analysis_key; lsp_key];
          Option.iter (SEMTOKS.store cache ~keys ~label:filename) semtoks;
          Lsp_workspace_symbols.store_for_analysis cache ~keys ~label:filename
            ~filename:absolute ~fresh:(Option.is_some witnesses) analysis;
          CACHED_DIAGS.store_for_analysis cache ~keys ~label:filename
            ~fresh:(Option.is_some witnesses) analysis;
          Some filename
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

let do_save_document ~config ~with_semantic_tokens
    (Lsp_document.{ project; _ } as doc) =
  let Lsp_project.{ rootdir; _ } = project in
  let cache = cache_for ~config ~rootdir in
  match store_document ~cache ~project ~with_semantic_tokens doc with
  | None -> ()
  | Some filename ->
      if config.lsp_cache_verbose then
        Lsp_io.log_info "Wrote@ document@ cache@ for@ %s" filename

let save_document ~params:{ config; with_semantic_tokens; _ } doc =
  try do_save_document ~config ~with_semantic_tokens doc with
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
   LSP entry is written whenever the document is next stored.

   A server whose client asks for semantic tokens needs them too: without
   them, the document is parsed afresh, and its semantic tokens are stored along
   with it. *)
let load_document ~project ~params
    ~doc:(Lsp.Types.DidOpenTextDocumentParams.{ textDocument; _ } as doc) =
  let Lsp_project.{ rootdir; _ } = project in
  let cache = cache_for ~config:params.config ~rootdir in
  let filename = Lsp.Uri.to_path textDocument.uri in
  let resolve =
    Superbol_cache.Recorder.resolver ~platform:Lsp_platform.record ()
  in
  let not_reading ?(reason = "") () =
    if params.config.lsp_cache_verbose then
      Lsp_io.log_info "Not@ reading@ document@ cache@ for@ %s%s" filename
        reason;
    None
  in
  match
    document_keys ~project ~filename ~contents:textDocument.text
      ~language_id:textDocument.languageId
  with
  | None ->
      None
  | Some (keys, lsp_key, _) ->
      match
        ANALYSIS.lookup_for_lsp cache ~key:(ANALYSIS.key_for_lsp keys) ~resolve
      with
      | None ->
          not_reading ()
      | Some analysis ->
          let semtoks =
            if analysis.lsp_copybook then None
            else SEMTOKS.lookup cache ~keys ~resolve
          in
          if params.with_semantic_tokens &&
             not analysis.lsp_copybook && Option.is_none semtoks
          then not_reading ~reason:" (no semantic tokens)" ()
          else
            let cached =
              match
                CACHE.lookup cache ~key:lsp_key ~resolve
                  ~read_item:CACHING.read_item
              with
              | Ok cached -> cached
              | Error _ -> None
            in
            let doc =
              Lsp_document.of_cache ~project ~params ~doc ?semtoks analysis cached
            in
            if params.config.lsp_cache_verbose then
              Lsp_io.log_info "Successfully@ read@ document@ cache@ for@ %s"
                (Lsp.Uri.to_string @@ Lsp_document.uri doc);
            Some doc

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

(* --- Documents that are not open --- *)

(** [read_source_file project filename] reads [filename] from disk, decoded as
    the client would send it (see {!set_file_encoding}). *)
let read_source_file project filename =
  Superbol_project.decode_source project @@
  Lsp_platform.record.read_text_file filename

(* --- Workspace symbols --- *)

(* Symbols retrieved for files that are not open, by absolute filename.  Each is
   retrieved again whenever the file changes on disk, or the project it was
   retrieved for is replaced (e.g. upon reloading its configuration).  Changes
   to the copybooks it depends upon go unnoticed until then.

   Misses are remembered for a little while only, so that symbols stored in the
   meantime (by the CLI, or upon closing the document) are noticed. *)
type memo =
  {
    memo_project: Lsp_project.t;
    memo_stamp: float * int;                                   (* mtime, size *)
    memo_symbols: Lsp_workspace_symbols.t option;
    memo_time: float;
  }

let symbols_memo: (string, memo) Hashtbl.t = Hashtbl.create 17
let miss_lifetime = 30.

let lookup_symbols ~cache ~config ~resolve ~now project filename =
  let platform = Lsp_platform.record in
  match platform.stat_file filename with
  | None ->
      Hashtbl.remove symbols_memo filename;
      None
  | Some { file_mtime; file_size; _ } ->
      let stamp = file_mtime, file_size in
      match Hashtbl.find_opt symbols_memo filename with
      | Some { memo_project; memo_stamp; memo_symbols; memo_time }
        when memo_project == project && memo_stamp = stamp &&
             (Option.is_some memo_symbols || now -. memo_time < miss_lifetime) ->
          memo_symbols
      | _ ->
          let symbols =
            match read_source_file project filename with
            | exception Sys_error _ ->
                None
            | contents ->
                let keys = ANALYSIS.document_keys config ~filename ~contents in
                Lsp_workspace_symbols.lookup cache ~keys ~resolve
          in
          Hashtbl.replace symbols_memo filename
            { memo_project = project; memo_stamp = stamp;
              memo_symbols = symbols; memo_time = now };
          symbols

let workspace_symbols ~params ~except project =
  let cache = cache_for ~config:params.config ~rootdir:(Lsp_project.rootdir project) in
  (* Files that are not open are analyzed without overrides (as the [languageId]
     of documents is only known once opened). *)
  match ANALYSIS.config_inputs project with
  | None ->
      Seq.empty
  | Some config ->
      let resolve =
        Superbol_cache.Recorder.resolver ~platform:Lsp_platform.record ()
      in
      let now = Lsp_platform.record.now () in
      Superbol_project.Walk.source_files project ~platform:Lsp_platform.record
        ~layout:params.config.lsp_project_layout |>
      List.to_seq |>
      Seq.filter (fun filename -> not (except filename)) |>
      Seq.filter_map begin fun filename ->
        Option.map (fun symbols -> Lsp.Uri.of_path filename, symbols) @@
        lookup_symbols ~cache ~config ~resolve ~now project filename
      end

(* --- Diagnostics --- *)

let cached_diagnostics ~params project filenames =
  let rootdir = Lsp_project.rootdir project in
  let cache = cache_for ~config:params.config ~rootdir in
  (* As for workspace symbols, files that are not open are analyzed without
     overrides. *)
  match ANALYSIS.config_inputs project with
  | None ->
      List.map (fun filename -> filename, None) filenames
  | Some config ->
      let platform = Lsp_platform.record in
      let resolve = Superbol_cache.Recorder.resolver ~platform () in
      List.map begin fun filename ->
        filename,
        match read_source_file project filename with
        | exception Sys_error _ ->
            None
        | contents ->
            let keys = ANALYSIS.document_keys config ~filename ~contents in
            CACHED_DIAGS.lookup cache ~keys ~resolve
      end filenames

(* --- *)

let set_file_encoding (project: Lsp_project.t) encoding =
  let same = match project.config.source_encoding, encoding with
    | None, None -> true
    | Some p, Some e -> Cobol_common.Text_encoding.(name p = name e)
    | _ -> false
  in
  if not same then begin
    project.config.source_encoding <- encoding;
    (* Symbols memoized for files of the project were looked up with keys of
       contents decoded otherwise. *)
    Hashtbl.filter_map_inplace begin fun _ memo ->
      if Superbol_project.have_same_rootdirs memo.memo_project project
      then None else Some memo
    end symbols_memo
  end
