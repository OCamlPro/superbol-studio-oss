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

val open_cfg
  : ?text_editor: Vscode.TextEditor.t
  -> typ:cfg_type
  -> Superbol_instance.t
  -> unit Promise.t
