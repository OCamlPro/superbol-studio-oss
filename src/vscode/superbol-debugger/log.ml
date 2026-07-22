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

module Level = struct
  type t = Debug | Info
end

let level = ref Level.Info

let setLevel l =
  level := l

let channel = Vscode.Window.createOutputChannel ~name:"SuperBOL Debugger"

let emit msg =
  let value = String.concat "" msg in
  let value =
    if String.ends_with ~suffix:"\n" value then
      String.sub value 0 (String.length value - 1)
    else
      value
  in
  Vscode.OutputChannel.appendLine channel ~value

let info = emit

let debug msg =
  if !level = Level.Debug then
    emit msg

let error msg =
  emit msg;
  Vscode.OutputChannel.show channel ()
