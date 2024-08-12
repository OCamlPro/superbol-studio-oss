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

module TYPES: sig

  type config = {
    project_layout: Lsp_project.layout;
    cache_config: Lsp_project_cache.config;
    enable_client_configs: bool;
    force_syntax_diagnostics: bool;
  }

  type params = {
    config: config;
    root_uri: Lsp.Types.DocumentUri.t option;
    workspace_folders: Lsp.Types.DocumentUri.t list;     (* includes root_uri *)
    with_semantic_tokens: bool;
    with_client_config_watcher: bool;
    with_client_file_watcher: bool;
  }

  type registry = private {
    projects: Lsp_project.SET.t;
    docs: Lsp_document.t URIMap.t;
    indirect_diags: Lsp_diagnostics.t URIMap.t;
    pending_tasks: pending_tasks;
    sub_state: sub_state;
    params: params;
  }

  and sub_state
  and pending_tasks

  type state =
    | NotInitialized of config
    | Initialized of params
    | Running of registry
    | ShuttingDown
    | Exit of exit_status

  and exit_status = (unit, string) result

  type 'a error =
    | CopybookRenamingForbidden
    | InvalidStatus of state
    | UnhandledRequest of 'a Lsp.Client_request.t
    | UnknownRequest of string

  exception Document_not_found of Lsp.Types.TextDocumentIdentifier.t

end
include module type of TYPES
  with type config = TYPES.config
   and type params = TYPES.params
   and type sub_state = TYPES.sub_state
   and type pending_tasks = TYPES.pending_tasks
   and type registry = TYPES.registry
   and type state = TYPES.state
   and type exit_status = TYPES.exit_status
   and type 'a error = 'a TYPES.error

type t = registry                                                    (* alias *)

val init
  : params: params
  -> t

val on_change_workspace_folders
  : Lsp.Types.DidChangeWorkspaceFoldersParams.t -> t -> t

(** When given, [copybook] indicates whether the document is a copybook (in
    which case it is not parsed directly as a normal program).  When absent,
    copybook detection is performed via project configuration (see
    {!Project.detect_copybook}). *)
val did_open
  : Lsp.Types.DidOpenTextDocumentParams.t -> ?copybook: bool -> t -> t

val did_change
  : Lsp.Types.DidChangeTextDocumentParams.t -> t -> t

val did_close
  : Lsp.Types.DidCloseTextDocumentParams.t -> t -> t

val find_document
  : Lsp.Types.TextDocumentIdentifier.t -> t -> Lsp_document.t

val jsonrpc_of_error
  : 'a error -> string -> Jsonrpc.Response.Error.t

val on_response
  : int -> Lsp.Import.Json.t -> t -> t

val on_client_config_changes
  : ?changes:Yojson.Safe.t -> t -> t

val on_watched_file_changes
  : Lsp.Types.FileEvent.t list -> t -> t

val on_write_project_config_command
  : ?uri:Lsp.Uri.t -> t -> t

val get_project_config_command
  : Lsp.Uri.t -> t -> Yojson.Safe.t

(** Note: May only raise {!Jsonrpc.Response.Error.E} *)
val save_project_caches
  : t -> unit
