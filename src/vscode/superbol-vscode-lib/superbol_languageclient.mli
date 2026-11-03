(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

type server_access =
  | Sub_process of Vscode_languageclient.ServerOptions.t
  | TCP of { host: string; port: int }

val server_access
  : context: Vscode.ExtensionContext.t
  -> server_prefix: string
  -> server_access

(** How to run commands of the executable that runs the language server, with
    the arguments that let them share its caches. *)
type cli_invocation =
  {
    program: string;
    args_prefix: string list;      (** to put before the command's arguments *)
    cli_env: string Interop.Dict.t;
    cli_storage_args: string list;
  }

(** [None] when the server is not run by the extension itself. *)
val cli_invocation
  : context: Vscode.ExtensionContext.t
  -> server_prefix: string
  -> cli_invocation option

val client_options
  : unit
  -> Vscode_languageclient.ClientOptions.t

val server_needs_restart_after
  : config_change: Vscode.ConfigurationChangeEvent.t
  -> bool
