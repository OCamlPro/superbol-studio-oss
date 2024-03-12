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
  -> Vscode_languageclient.ServerOptions.t

val client_options
  : unit
  -> Vscode_languageclient.ClientOptions.t

val server_needs_restart_after
  : config_change: Vscode.ConfigurationChangeEvent.t
  -> bool
