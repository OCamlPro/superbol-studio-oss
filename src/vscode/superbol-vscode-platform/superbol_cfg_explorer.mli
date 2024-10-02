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

type cfg_type = Graphviz | D3_arc_diagram

(*
  [open_cfg ~text_editor typ instance] will load a CFG of type [typ] after the user
  has selected the CFG scope (which program or which section of the program).
  The different scopes are listed based on the code of [text_editor], or
  the active text editor if no editor is given.
  The CFG are computed via the LSP [instance] given.
  Calling [open_cfg] twice with the same arguments will simply reload the CFG
  and refocus the existing webview.
 *)
val open_cfg
  : ?text_editor: Vscode.TextEditor.t
  -> typ:cfg_type
  -> Superbol_instance.t
  -> unit Promise.t
