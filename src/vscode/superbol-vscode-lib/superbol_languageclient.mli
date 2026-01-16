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

val client_options
  : unit
  -> Vscode_languageclient.ClientOptions.t

val server_needs_restart_after
  : config_change: Vscode.ConfigurationChangeEvent.t
  -> bool
