(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

let debugSettings =
  let settings =
    Vscode.Workspace.getConfiguration ~section:"superbol.debugger" () in
  if Vscode.WorkspaceConfiguration.has settings ~section:"gdbPath" then
    settings
  else
    Vscode.Workspace.getConfiguration ~section:"superbol-vscode-debug" ()

let displayVariableAttributes () =
  let res =
    Vscode.WorkspaceConfiguration.get debugSettings
      ~section:"displayVariableAttributes" in
  Option.fold ~none:false ~some:[%js.to: bool] res

let gdbPath () =
  let res =
    Vscode.WorkspaceConfiguration.get debugSettings
      ~section:"gdbPath" in
  Option.fold ~none:"" ~some:[%js.to: string] res

let libcobPath () =
  let res =
    Vscode.WorkspaceConfiguration.get debugSettings
      ~section:"libcobPath" in
  Option.fold ~none:"" ~some:[%js.to: string] res

let cobcrunPath () =
  let res =
    Vscode.WorkspaceConfiguration.get debugSettings
      ~section:"cobcrunPath" in
  Option.fold ~none:"" ~some:[%js.to: string] res
