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
module IMap = Map.Make (Int)

module TYPES = struct

  type config = {
    project_layout: Lsp_project.layout;
    cache_config: Lsp_project_cache.config;
    enable_client_configs: bool;
  }

  type params = {
    config: config;
    root_uri: DocumentUri.t option;
    workspace_folders: DocumentUri.t list;               (* includes root_uri *)
    with_semantic_tokens: bool;
    with_client_config_watcher: bool;
    with_client_file_watcher: bool;
  }

  type registry = {                                                (* private *)
    projects: Lsp_project.SET.t;
    docs: Lsp_document.t URIMap.t;
    indirect_diags: Lsp_diagnostics.t URIMap.t; (* diagnostics for other URIs
                                                   mentioned by docs in
                                                   `docs` *)
    pending_tasks: pending_tasks;
    sub_state: sub_state;
    params: params;
  }

  and sub_state =
    {
      (* Internal flag used to prevent emitting a warning when loading existing
         projects during server initialization.

         During this phase, we register for "workspace/didChangeConfiguration"
         notifications, but VSCode seems to send one global configuration change
         directly afterwards.  We want to ignore that one, as client
         configurations are pulled on an on-demand and per-project basis.  *)
      ignore_next_client_config_changes: bool;
    }

  and pending_tasks = {
    delayed_id: int;
    delayed: sink_continuation IMap.t;
  }

  and sink_continuation =
    | Sink: (registry, _) continuation -> sink_continuation

  and ('a, 'b) continuation = {
    request: 'b Lsp.Server_request.t;
    f: 'b -> registry -> 'a;
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



(** {2 Management of client responses}

    Implements scheduling of promises that await on responses for requests to
    the client.} *)


(** Promises, internal to this module. *)
type 'a promise =
  | Now: 'a -> 'a promise
  | Wait: ('a promise * registry, _) continuation -> 'a promise


let now x registry: _ promise * t =
  Now x, registry


let await_response ~request ~f registry: _ promise * t =
  Wait { request; f }, registry


let delay
    ({ pending_tasks = { delayed; delayed_id }; _ } as registry)
    ~(request: 'a Lsp.Server_request.t)
    ~(f: 'a -> t -> t)
  : t =
  (* Lsp_io.log_info "Sending@ server@ request@ with@ ID@ %d" delayed_id; *)
  Lsp_io.send_request @@
  Lsp.Server_request.to_jsonrpc_request ~id:(`Int delayed_id) request;
  { registry with
    pending_tasks =
      { delayed = IMap.add delayed_id (Sink { request; f }) delayed;
        delayed_id = succ delayed_id } }


let delay_unit: t -> request: unit Lsp.Server_request.t -> t =
  delay ~f:(fun () registry -> registry)


let rec perform (f: 'a -> t -> t) ~(after: 'a promise * t) : t =
  match after with
  | Now y, registry ->
      f y registry
  | Wait { request; f = f' }, registry ->
      delay registry ~request
        ~f:(fun x registry -> perform f ~after:(f' x registry))


let ignore_promise_result: 'a promise * t -> t = fun after ->
  perform (fun _ registry -> registry) ~after


let on_response id response
    ({ pending_tasks = { delayed; _ }; _ } as registry) =
  try                                      (* TODO: handle malformed response *)
    let Sink { request; f } = IMap.find id delayed in
    let registry =
      { registry with
        pending_tasks = { registry.pending_tasks with
                          delayed = IMap.remove id delayed } }
    in            (* TODO: dedicated handling of errors in `response_of_json` *)
    try
      f (Lsp.Server_request.response_of_json request response) registry
    with
    | Jsonrpc.Json.Of_json (error, json) ->
        Lsp_io.log_error
          "Recieved@ a@ response@ with@ invalid@ format@ (%s):@ %a"
          error Yojson.Safe.pp json;
        Lsp_io.log_info "request: %a" Yojson.Safe.pp
          (Jsonrpc.Request.yojson_of_t @@
           Lsp.Server_request.to_jsonrpc_request ~id:(`Int id) request);
        Lsp_io.log_info "response: %a" Yojson.Safe.pp response;
        registry
    | e ->
        Lsp_io.log_error "Exception@ raised@ in@ delayed@ computation:@ %a"
          Fmt.exn e;
        registry
  with Not_found ->
    Lsp_io.log_error "Response@ with@ unkown@ ID@ %d@ received" id;
    registry


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


(** Registration for client notifications *)


let start_watching_config_of ~project registry =
  let registration =
    RegistrationParams.create ~registrations:[
      let watchers =
        (* Note: basic glob patterns that try matching only in rootdir do not
           seem to work (in VS Code).  Watch in any sub-dir? *)
        let pattern = "**/superbol.toml" in
        let rooturi = Lsp_project.rooturi project in
        let globPattern =
          (* `Pattern pattern *)
          `RelativePattern
            (RelativePattern.create ~pattern ~baseUri:(`URI rooturi))
        in
        FileSystemWatcher.[
          create () ~globPattern ~kind:Create;
          create () ~globPattern ~kind:Change;
          create () ~globPattern ~kind:Delete;
        ]
      in
      Registration.create ()
        ~id:"watch-superbol.toml"
        ~method_:"workspace/didChangeWatchedFiles"
        ~registerOptions:
          DidChangeWatchedFilesRegistrationOptions.(yojson_of_t @@
                                                    create ~watchers)
    ]
  in
  delay_unit registry
    ~request:(ClientRegisterCapability registration)


let maybe_start_watching_config_of ~project registry =
  if registry.params.with_client_file_watcher
  then start_watching_config_of ~project registry
  else registry


let start_watching_client_config registry =
  let registration =
    RegistrationParams.create ~registrations:[
      let section = `String "superbol.cobol" in
      Registration.create ()
        ~id:"watch-client-settings"
        ~method_:"workspace/didChangeConfiguration"
        ~registerOptions:
          DidChangeConfigurationRegistrationOptions.(yojson_of_t @@
                                                     create () ~section)
    ]
  in
  delay_unit registry
    ~request:(ClientRegisterCapability registration)


let maybe_start_watching_client_config registry =
  if registry.params.config.enable_client_configs &&
     registry.params.with_client_config_watcher
  then start_watching_client_config registry
  else registry


(** {1 Registry management} *)


(** {2 Projects} *)

(* TODO: add ability to remove projects from the registry? *)


let add_project project r =
  let projects = Lsp_project.SET.add project r.projects in
  if projects == r.projects
  then r
  else maybe_start_watching_config_of ~project { r with projects }


(** {3 Per-project cache} *)


let save_project_caches
    { params = { config = { cache_config = config; _ }; _ };
      docs; _ } =
  try Lsp_project_cache.save ~config docs
  with e ->
    Lsp_error.internal
      "Exception@ caught@ while@ saving@ project@ caches:@ %a@." Fmt.exn e


let load_project_cache ~rootdir
    ({ params = { config = { project_layout = layout;
                             cache_config = config; _ }; _ };
       docs = old_docs; _ } as registry) =
  let new_docs = Lsp_project_cache.load ~config ~layout ~rootdir in
  let registry =
    { registry with
      docs = URIMap.union (fun _ _old new_ -> Some new_) old_docs new_docs }
  in
  match URIMap.choose_opt new_docs with
  | Some (_, Lsp_document.{ project; _ }) -> add_project project registry
  | None -> registry


(** {2 Documents} *)


let add_or_replace_doc doc r =
  let docs = URIMap.add (Lsp_document.uri doc) doc r.docs in
  if docs == r.docs then r else { r with docs }


(** {3 Error reporting} *)


let document_error_while_ operation doc e backtrace registry =
  let backtrace = Printexc.raw_backtrace_to_string backtrace in
  Lsp_io.log_error
    "Internal error while %(%) document: %a%s" operation Fmt.exn e
    (if backtrace = "" then "" else "\n" ^ backtrace);
  add_or_replace_doc doc registry



(** {3 Configuration} *)


let extract_docs_of ~project registry =
  let out_of_date_docs, docs =
    URIMap.partition begin fun _ doc ->
      Superbol_project.have_same_rootdirs doc.Lsp_document.project project
    end registry.docs
  in
  { registry with docs }, out_of_date_docs


let reload_docs_of ~project out_of_date_docs registry =
  (* FIXME: may there be out-of-date diagnostics in
     `registry.indirect_diags`? *)
  URIMap.fold begin fun _ doc registry ->
    (* TODO: send relevant "refresh" server requests. *)
    let doc = Lsp_document.reload { doc with project } in
    let registry = dispatch_diagnostics doc registry in
    let registry = add_or_replace_doc doc registry in
    if registry.params.with_semantic_tokens
    then delay_unit ~request:SemanticTokensRefresh registry
    else registry
  end out_of_date_docs registry


let use_client_config_for ~project (configs: Yojson.Safe.t list) registry =
  (* Every call to this function should be guarded with this flag: *)
  assert registry.params.config.enable_client_configs;
  if Sys.file_exists project.Lsp_project.config_filename
  then now project registry
  else begin
    Lsp_io.log_info "Received@ client@ configuration:@ @[%a@]"
      (Yojson.Safe.pretty_print ~std:false) (`List configs);
    let registry, out_of_date_docs = match configs with
      | [`Assoc assoc] ->
          if Lsp_project.update_project_config assoc project
          then extract_docs_of ~project registry (* configuration changed *)
          else registry, URIMap.empty            (* no doc is outdated *)
      | _ ->
          Lsp_io.log_error "Invalid@ configuration@ item: %s"
            (Yojson.Safe.to_string @@ `List configs);
          registry, URIMap.empty
    in
    now project @@ reload_docs_of ~project out_of_date_docs registry
  end


let request_n_use_client_config_for ~project
    ?(silence_ignored_settings_warning = false) registry =
  (* Every call to this function should be guarded with this flag: *)
  assert registry.params.config.enable_client_configs;
  if Sys.file_exists project.Lsp_project.config_filename
  then begin
    (* TODO (maybe): always carry on with the subsequent
       "workspace/configuration" request, and check whether some of the client
       settings don't not match with the project settings afterwards before
       emiting the warning (in `use_client_config_for`)? *)
    if not silence_ignored_settings_warning then
      Lsp_io.notify_warn "New@ settings@ overriden@ by@ existing@ project@ \
                          configuration@ file@ at@ %s.@\n@\nPlease@ edit@ this@ \
                          file@ instead." project.config_filename;
    now project registry
  end else begin
    let uri = Lsp_project.rooturi project in
    await_response registry
      ~request:(WorkspaceConfiguration
                  { items = [ ConfigurationItem.create ()
                                ~scopeUri:(Lsp.Uri.to_string uri)
                                ~section:"superbol.cobol" ] })
      ~f:(use_client_config_for ~project)
  end


let on_client_config_changes ?changes registry =
  if not registry.params.config.enable_client_configs
  then registry
  else if registry.sub_state.ignore_next_client_config_changes
  then begin
    (* Skip the first notification that is automatically sent after registration
       to "workspace/didChangeConfiguration" *)
    { registry with
      sub_state = { ignore_next_client_config_changes = false } }
  end
  else begin
    (* Note we ignore the provided changes as they are not project-specific.
       Instead, we re-request them manually on a per-project basis. *)
    ignore changes;
    Lsp_project.SET.fold begin fun project registry ->
      ignore_promise_result @@
      request_n_use_client_config_for ~project registry
    end registry.projects registry
  end


let on_project_config_file_changes changes registry =
  List.fold_left begin fun registry FileEvent.{ type_; uri } ->
    try
      let project = Lsp_project.SET.for_ ~uri registry.projects in
      match type_ with
      | Created | Changed ->
          let registry, out_of_date_docs =
            if Lsp_project.reload_project_config project
            then extract_docs_of ~project registry
            else registry, URIMap.empty
          in
          reload_docs_of ~project out_of_date_docs registry
      | Deleted when registry.params.config.enable_client_configs ->
          ignore_promise_result @@
          request_n_use_client_config_for ~project registry
      | Deleted ->                            (* TODO: reset to default config *)
          registry
    with Not_found ->                             (* change in unknown project *)
      registry
  end registry changes


let on_watched_file_changes changes registry =
  (* Note: for now, we only watch configuration files. *)
  on_project_config_file_changes changes registry



(** [retrieve_project_for ~uri registry] tries to load the project where a
    document at the given URI belongs.  Does nothing if the project is already
    loaded. *)
let retrieve_project_for ~uri registry =
  try
    ignore @@ Lsp_project.SET.for_ ~uri registry.projects;
    (* CHECKME: at this point, we have overlooked any `superbol.toml` that lies
       between the root of the (assumed) project for [uri] and the file at
       [uri].  Not sure it is desirable to support nested projects, though. *)
    registry
  with Not_found ->
    let layout = registry.params.config.project_layout in
    let rootdir = Lsp_project.rootdir_for ~uri ~layout in
    if Lsp_project.SET.mem_rootdir ~rootdir registry.projects
    then registry
    else load_project_cache ~rootdir registry


let create_or_retrieve_project_promise ~uri registry =
  try
    now (Lsp_project.SET.for_ ~uri registry.projects) registry
  with Not_found ->
    let layout = registry.params.config.project_layout in
    let rootdir = Lsp_project.rootdir_for ~uri ~layout in
    try
      now (Lsp_project.SET.for_rootdir ~rootdir registry.projects) registry
    with Not_found ->                              (* load or init new project *)
      let project = Lsp_project.for_ ~rootdir ~layout in
      add_project project registry |>
      if registry.params.config.enable_client_configs
      then
        request_n_use_client_config_for ~project
          ~silence_ignored_settings_warning:true
      else
        now project


let on_write_project_config_command uri registry =
  let write_project_config project registry =
    Lsp_io.log_info "Saving@ configuration@ for@ project@ of@ %s"
      (Lsp.Uri.to_string uri);
    Superbol_project.save_config ~verbose:true project;
    registry
  in
  perform write_project_config
    ~after:(create_or_retrieve_project_promise ~uri registry)


let get_project_config_command uri registry =
  Lsp_io.log_info "Getting@ configuration@ for@ project@ of@ %s"
    (Lsp.Uri.to_string uri);
  let registry = retrieve_project_for ~uri registry in
  try
    Lsp_project.get_project_config @@
    Lsp_project.SET.for_ ~uri registry.projects
  with Not_found ->
    (* NOTE: for this we need to be able to delay replies to server requests. *)
    Lsp_error.request_failed "%s is not part of any project from the\
                              workspace"
      (Lsp.Uri.to_string uri)


(** {3 Initialization} *)


let load_project_in ~dir registry =
  let layout = registry.params.config.project_layout in
  let project = Lsp_project.in_existing_dir dir ~layout in
  let registry =
    add_project project @@
    load_project_cache ~rootdir:(Lsp_project.rootdir project) registry
  in
  ignore_promise_result @@                   (* retrieve client configuration *)
  request_n_use_client_config_for ~project registry
    ~silence_ignored_settings_warning:true


let init ~params : registry =
  let registry =
    { params;
      projects = Lsp_project.SET.empty;
      docs = URIMap.empty;
      indirect_diags = URIMap.empty;
      pending_tasks = { delayed_id = 0; delayed = IMap.empty };
      sub_state = { ignore_next_client_config_changes = true } }
  in
  List.fold_left begin fun registry workspace_folder_uri ->
    load_project_in ~dir:(Lsp.Uri.to_path workspace_folder_uri) registry
  end registry params.workspace_folders |>
  URIMap.fold (fun _ -> dispatch_diagnostics) registry.docs |>
  maybe_start_watching_client_config



(** {2 Handling of document notifications} *)


let add ~doc:(DidOpenTextDocumentParams.{ textDocument = { uri; _ }; _ } as doc)
    ?copybook registry =
  let add_in_project project registry =
    try
      let doc = Lsp_document.load ~project ?copybook doc in
      let registry = dispatch_diagnostics doc registry in
      add_or_replace_doc doc registry
    with Lsp_document.Internal_error (doc, e, backtrace) ->
      document_error_while_"opening" doc e backtrace registry
  in
  perform add_in_project
    ~after:(create_or_retrieve_project_promise ~uri registry)


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
        retrieve_project_for ~uri registry |>
        aux ~try_cache:false                   (* try again without the cache *)
    | None | Some _ ->
        add ~doc ?copybook registry
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
