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
  }

  type registry = private {
    projects: Lsp_project.SET.t;
    docs: Lsp_document.t URIMap.t;
    indirect_diags: Lsp_diagnostics.t URIMap.t; (* diagnostics for other URIs
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

  exception Document_not_found of Lsp.Types.TextDocumentIdentifier.t

end
include module type of TYPES
  with type config = TYPES.config
   and type registry = TYPES.registry
   and type state = TYPES.state
   and type exit_status = TYPES.exit_status
   and type 'a error = 'a TYPES.error

type t = registry                                                    (* alias *)

val init
  : config:config
  -> t

(** When given, [copybook] indicates whether the document is a copybook (in
    which case it is not parsed directly as a normal program).  When absent,
    copybook detection is performed via project configuration (see
    {!Lsp_project.detect_copybook}). *)
val add
  : Lsp.Types.DidOpenTextDocumentParams.t -> ?copybook: bool -> t -> t

val update
  : Lsp.Types.DidChangeTextDocumentParams.t -> t -> t

val remove
  : Lsp.Types.DidCloseTextDocumentParams.t -> t -> t

val find_document
  : Lsp.Types.TextDocumentIdentifier.t -> t -> Lsp_document.t

val jsonrpc_of_error
  : 'a error -> string -> Jsonrpc.Response.Error.t

val save_project_caches
  : t -> unit
