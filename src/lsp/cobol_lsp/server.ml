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
    project_layout: Project.layout;
    cache_config: Project_cache.config;
  }

  type registry = {                                                (* private *)
    projects: Project.SET.t;
    docs: Document.t URIMap.t;
    indirect_diags: Diagnostics.t URIMap.t; (* diagnostics for other URIs
                                                   mentioned by docs in
                                                   `docs` *)
    config: config;
  }

  type state =
    | NotInitialized of config   (* At startup and until "initialize" request *)
    | Initialized of config (* After "initialize" and before "initialized" notif *)
    | Running of registry   (* After "initialized" and before "shutdown" *)
    | ShuttingDown          (* From "shuntdown" until "exit" notif *)
    | Exit of exit_status   (* After "exit" notif *)

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
  let projects = Project.SET.add proj r.projects in
  if projects == r.projects then r else { r with projects }

let add_or_replace_doc doc r =
  let docs = URIMap.add (Document.uri doc) doc r.docs in
  if docs == r.docs then r else { r with docs }

(** {2 Handling of diagnostics for non-opened documents} *)

let dispatch_diagnostics (Document.{ project; diags; _ } as doc) registry =
  let uri = Document.uri doc in
  let rootdir = Project.rootdir project in
  let indirect4uri =
    if diags <> DIAGS.Set.none
    then URIMap.empty                         (* stick to the new diagnostics *)
    else URIMap.filter (fun _ -> URIMap.mem uri) registry.indirect_diags
  in
  if URIMap.is_empty indirect4uri then begin
    let all_diags =
      Diagnostics.translate diags ~uri:(`Main uri)
        ~rootdir:(Project.string_of_rootdir rootdir)
    in
    (* Note here we may publish diagnostics for non-opened documents.  LSP
       protocol does not seem to forbid that (but some editors just ignore
       those).  *)
    Diagnostics.publish all_diags;
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
    Diagnostics.publish all_diags;
    registry
  end

(** {2 Management of per-project caches} *)

let save_project_caches { config = { cache_config = config; _ }; docs; _ } =
  try Project_cache.save ~config docs
  with e ->
    Lsp_error.internal
      "Exception@ caught@ while@ saving@ project@ caches:@ %a@." Fmt.exn e

let load_project_cache ~rootdir ({ config = { project_layout = layout;
                                              cache_config = config; _ };
                                   projects; docs = old_docs; _ } as registry) =
  let new_docs = Project_cache.load ~config ~layout ~rootdir in
  let projects = match URIMap.choose_opt new_docs with
    | Some (_, Document.{ project = p; _ }) -> Project.SET.add p projects
    | None -> projects
  and docs = URIMap.union (fun _ _old new_ -> Some new_) old_docs new_docs in
  { registry with projects; docs }


(** {2 Registry management} *)

let init ~config : registry =
  {
    config;
    projects = Project.SET.empty;
    docs = URIMap.empty;
    indirect_diags = URIMap.empty;
  }

let create_or_retrieve_project ~uri registry =
  let layout = registry.config.project_layout in
  let rootdir = Project.rootdir_for ~uri ~layout in
  try Project.SET.for_rootdir ~rootdir registry.projects, registry
  with Not_found ->
    let project = Project.for_ ~rootdir ~layout in
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
    let doc = Document.load ~project ?copybook doc in
    let registry = dispatch_diagnostics doc registry in
    add_or_replace_doc doc registry
  with Document.Internal_error (doc, e, backtrace) ->
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
          let layout = registry.config.project_layout in
          let rootdir = Project.rootdir_for ~uri ~layout in
          if Project.SET.mem_rootdir ~rootdir registry.projects
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
    let doc = Document.update doc contentChanges in
    let registry = dispatch_diagnostics doc registry in
    add_or_replace_doc doc registry
  with Document.Internal_error (doc, e, backtrace) ->
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
