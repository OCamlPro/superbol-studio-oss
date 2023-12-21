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

module WS = Vscode.Workspace
module WS_CONF = Vscode.WorkspaceConfiguration

let section = "superbol"
let non_empy s = if s = "" then None else Some s

let superbol_exe ?scope () =
  let config = WS.getConfiguration ?scope ~section () in
  match WS_CONF.get ~section:"lsp-path" config with
  | None -> None
  | Some o when Ojs.is_null o -> None
  | Some s -> non_empy (Ojs.string_of_js s)

let cobc_exe ?scope () =
  let config = WS.getConfiguration ?scope ~section () in
  match WS_CONF.get ~section:"cobc-path" config with
  | None -> None
  | Some o when Ojs.is_null o -> None
  | Some s -> non_empy (Ojs.string_of_js s)
