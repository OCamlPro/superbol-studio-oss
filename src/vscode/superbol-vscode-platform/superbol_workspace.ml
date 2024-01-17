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

let bool ?scope key =
  let config = WS.getConfiguration ?scope ~section () in
  match WS_CONF.get ~section:key config with
  | None -> false
  | Some o when Ojs.is_null o -> false
  | Some s -> Ojs.bool_of_js s

let string ?scope key =
  let config = WS.getConfiguration ?scope ~section () in
  match WS_CONF.get ~section:key config with
  | None -> ""
  | Some o when Ojs.is_null o -> ""
  | Some s -> Ojs.string_of_js s

let string_list ?scope key =
  let config = WS.getConfiguration ?scope ~section () in
  match WS_CONF.get ~section:key config with
  | None -> []
  | Some o when Ojs.is_null o -> []
  | Some s -> Ojs.(list_of_js string_of_js) s      (* TODO: how may this fail? *)
