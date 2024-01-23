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

open Lsp_imports
open Lsp.Types

module DIAGS = Cobol_common.Diagnostics

module TYPES = struct

  type config = {
    project_layout: Lsp_project.layout;
    cache_config: Lsp_project_cache.config;
  }

  type params = {
    config: config;
    root_uri: Lsp.Types.DocumentUri.t option;
    workspace_folders: Lsp.Types.DocumentUri.t list;     (* includes root_uri *)
  }

  type registry = {                                                (* private *)
    projects: Lsp_project.SET.t;
    docs: Lsp_document.t URIMap.t;
    indirect_diags: Lsp_diagnostics.t URIMap.t; (* diagnostics for other URIs
                                                   mentioned by docs in
                                                   `docs` *)
    params: params;
  }

  type state =
    | NotInitialized of config   (* At startup and until "initialize" request *)
    | Initialized of params      (* After "initialize" and before "initialized"
                                    notif *)
    | Running of registry        (* After "initialized" and before "shutdown" *)
    | ShuttingDown               (* From "shuntdown" until "exit" notif *)
    | Exit of exit_status        (* After "exit" notif *)

  and exit_status = (unit, string) result

  type 'a error =
    | InvalidStatus of state
    | UnhandledRequest of 'a Lsp.Client_request.t
    | UnknownRequest of string

  exception Document_not_found of TextDocumentIdentifier.t

end
include TYPES

type t = registry

(* Code: *)

let add_project proj r =
  let projects = Lsp_project.SET.add proj r.projects in
  if projects == r.projects then r else { r with projects }

let add_or_replace_doc doc r =
  let docs = URIMap.add (Lsp_document.uri doc) doc r.docs in
  if docs == r.docs then r else { r with docs }

(** {2 Handling of diagnostics for non-opened documents} *)

let dispatch_diagnostics (Lsp_document.{ project; diags; _ } as doc) registry =
  let uri = Lsp_document.uri doc in
  let rootdir = Lsp_project.rootdir project in
  let indirect4uri =
    if diags <> DIAGS.Set.none
    then URIMap.empty                         (* stick to the new diagnostics *)
    else URIMap.filter (fun _ -> URIMap.mem uri) registry.indirect_diags
  in
  if URIMap.is_empty indirect4uri then begin
    let all_diags =
      Lsp_diagnostics.translate diags ~uri
        ~rootdir:(Lsp_project.string_of_rootdir rootdir)
        ~focus_on_main_doc: false
    in
    (* Note here we may publish diagnostics for non-opened documents.  LSP
       protocol does not seem to forbid that (but some editors just ignore
       those).  *)
    Lsp_diagnostics.publish all_diags;
    { registry with
      indirect_diags =
        (* Register published diagnostics for the other documents in case they
           are opened in the future. *)
        URIMap.merge (fun _ _ new_ -> new_) registry.indirect_diags @@
        URIMap.singleton uri (URIMap.remove uri all_diags) }
  end else begin          (* publish indirect diagnostics for the doc instead *)
    let all_diags =
      URIMap.fold begin fun _main_uri ->
        URIMap.union (fun _ a b -> Some (List.rev_append a b))
      end indirect4uri URIMap.empty
    in
    Lsp_diagnostics.publish all_diags;
    registry
  end

(** {2 Management of per-project caches} *)

let save_project_caches { params = { config = { cache_config = config; _ }; _ };
                          docs; _ } =
  try Lsp_project_cache.save ~config docs
  with e ->
    Lsp_error.internal
      "Exception@ caught@ while@ saving@ project@ caches:@ %a@." Fmt.exn e

let load_project_cache ~rootdir
    ({ params = { config = { project_layout = layout;
                             cache_config = config; _ }; _ };
       projects; docs = old_docs; _ } as registry) =
  let new_docs = Lsp_project_cache.load ~config ~layout ~rootdir in
  let projects = match URIMap.choose_opt new_docs with
    | Some (_, Lsp_document.{ project = p; _ }) -> Lsp_project.SET.add p projects
    | None -> projects
  and docs = URIMap.union (fun _ _old new_ -> Some new_) old_docs new_docs in
  { registry with projects; docs }


(** {2 Registry management} *)

let load_project_in ~dir registry =
  let layout = registry.params.config.project_layout in
  let project = Lsp_project.in_existing_dir dir ~layout in
  load_project_cache ~rootdir:(Lsp_project.rootdir project) registry |>
  add_project project

let init ~params : registry =
  let registry =
    { params;
      projects = Lsp_project.SET.empty;
      docs = URIMap.empty;
      indirect_diags = URIMap.empty }
  in
  List.fold_left begin fun registry workspace_folder_uri ->
    load_project_in ~dir:(Lsp.Uri.to_path workspace_folder_uri) registry
  end registry params.workspace_folders

let create_or_retrieve_project ~uri registry =
  let layout = registry.params.config.project_layout in
  let rootdir = Lsp_project.rootdir_for ~uri ~layout in
  try Lsp_project.SET.for_rootdir ~rootdir registry.projects, registry
  with Not_found ->
    let project = Lsp_project.for_ ~rootdir ~layout in
    project, add_project project registry

let document_error_while_ operation doc e backtrace registry =
  let backtrace = Printexc.raw_backtrace_to_string backtrace in
  Lsp_io.pretty_notification ~log:true ~type_:Error
    "Internal error while %(%) document: %a%s" operation Fmt.exn e
    (if backtrace = "" then "" else "\n" ^ backtrace);
  add_or_replace_doc doc registry

let add (DidOpenTextDocumentParams.{ textDocument = { uri; _ }; _ } as doc)
    ?copybook registry =
  let project, registry = create_or_retrieve_project ~uri registry in
  try
    let doc = Lsp_document.load ~project ?copybook doc in
    let registry = dispatch_diagnostics doc registry in
    add_or_replace_doc doc registry
  with Lsp_document.Internal_error (doc, e, backtrace) ->
    document_error_while_"opening" doc e backtrace registry

let did_open (DidOpenTextDocumentParams.{ textDocument = { uri; text; _ };
                                          _ } as doc) ?copybook registry =
  (* Try first with a lookup for the project in a cache, and then by
     creating/loading the project. *)
  let rec aux ~try_cache registry =
    match URIMap.find_opt uri registry.docs with
    (* When opening, we need to check that the text we already have (in cache)
       matches what the client gives us. *)
    | Some doc when String.equal (Lsp.Text_document.text doc.textdoc) text ->
        dispatch_diagnostics doc registry
    | None | Some _ when try_cache ->
        let registry =
          let layout = registry.params.config.project_layout in
          let rootdir = Lsp_project.rootdir_for ~uri ~layout in
          if Lsp_project.SET.mem_rootdir ~rootdir registry.projects
          then registry
          else load_project_cache ~rootdir registry
        in
        aux ~try_cache:false registry          (* try again without the cache *)
    | None | Some _ ->
        add doc ?copybook registry
  in
  aux ~try_cache:true registry

(** Raises {!Document_not_found} if the document is not currently opened. *)
let find_document (TextDocumentIdentifier.{ uri; _ } as doc) { docs; _ } =
  try URIMap.find uri docs
  with Not_found -> raise @@ Document_not_found doc

let did_change DidChangeTextDocumentParams.{ textDocument = { uri; _ };
                                             contentChanges; _ } registry =
  try
    let doc = find_document TextDocumentIdentifier.{ uri } registry in
    let doc = Lsp_document.update doc contentChanges in
    let registry = dispatch_diagnostics doc registry in
    add_or_replace_doc doc registry
  with Lsp_document.Internal_error (doc, e, backtrace) ->
    document_error_while_"updating" doc e backtrace registry

let did_close DidCloseTextDocumentParams.{ textDocument = { uri } } registry =
  { registry with docs = URIMap.remove uri registry.docs }

(** {2 Miscellaneous} *)

let jsonrpc_of_error error method_ =
  let (code: Jsonrpc.Response.Error.Code.t), message = match error with
    | InvalidStatus NotInitialized _ ->
        ServerNotInitialized, "Expected 'initialize' request"
    | InvalidStatus Initialized _ ->
        InvalidRequest, "Unexpected request during initialization"
    | InvalidStatus ShuttingDown ->
        InvalidRequest, "Unexpected request while shutting down"
    | InvalidStatus Exit _ ->
        InvalidRequest, "Unexpected request while quitting"
    | InvalidStatus Running _ ->
        InvalidRequest, "Unexpected request while running"
    | UnhandledRequest _ ->
        RequestFailed, Fmt.str "Unhandled request: %s" method_
    | UnknownRequest method_ ->
        MethodNotFound, Fmt.str "Unknown request method: %s" method_
  in
  Jsonrpc.Response.Error.make ~code ~message ()
