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
    pending_tasks: delayed_continuations;
    params: params;
  }

  and delayed_continuations

  type state =
    | NotInitialized of config
    | Initialized of params
    | Running of registry
    | ShuttingDown
    | Exit of exit_status

  and exit_status = (unit, string) result

  type 'a error =
    | InvalidStatus of state
    | UnhandledRequest of 'a Lsp.Client_request.t
    | UnknownRequest of string

  exception Document_not_found of Lsp.Types.TextDocumentIdentifier.t

end
include module type of TYPES
  with type config = TYPES.config
   and type params = TYPES.params
   and type delayed_continuations = TYPES.delayed_continuations
   and type registry = TYPES.registry
   and type state = TYPES.state
   and type exit_status = TYPES.exit_status
   and type 'a error = 'a TYPES.error

type t = registry                                                    (* alias *)

val init
  : params: params
  -> t

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

val on_client_config_change
  : t -> t

val on_watched_file_changes
  : Lsp.Types.FileEvent.t list -> t -> t

(** Note: May only raise {!Jsonrpc.Response.Error.E} *)
val save_project_caches
  : t -> unit
