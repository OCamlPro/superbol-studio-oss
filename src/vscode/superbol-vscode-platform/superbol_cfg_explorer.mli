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

val open_cfg
  : ?typ: [< `D3 | `Dot > `Dot]
  -> ?text_editor: Vscode.TextEditor.t
  -> Superbol_instance.t
  -> unit Promise.t

val open_webview
  : ?text_editor: Vscode.TextEditor.t
  -> Superbol_instance.t
  -> unit Promise.t
