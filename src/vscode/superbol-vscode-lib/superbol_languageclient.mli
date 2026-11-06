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

val server_options
  : context: Vscode.ExtensionContext.t
  -> server_prefix: string
  -> Vscode_languageclient.ServerOptions.t

(** How to run commands of the executable that runs the language server, with
    the arguments that let them share its caches. *)
type cli_invocation =
  {
    program: string;
    args_prefix: string list;      (** to put before the command's arguments *)
    cli_env: string Interop.Dict.t;
    cli_storage_args: string list;
  }

val cli_invocation
  : context: Vscode.ExtensionContext.t
  -> server_prefix: string
  -> cli_invocation

val client_options
  : unit
  -> Vscode_languageclient.ClientOptions.t

val server_needs_restart_after
  : config_change: Vscode.ConfigurationChangeEvent.t
  -> bool
