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

open Promise.Syntax

let activate (extension: Vscode.ExtensionContext.t)
  : (* Superbol_vscode_lib.Types.superbol_instance *)
    Vscode_languageclient.LanguageClient.t option Promise.t =
  let lsp_server_prefix = "superbol-free" in
  let* instance = Superbol_vscode_lib.activate ~lsp_server_prefix extension in
  let* () = Superbol_debugger.activate extension in
  Promise.return (Superbol_vscode_lib.Instance.client instance)

let deactivate () =
  let* () = Superbol_debugger.deactivate () in
  let* () = Superbol_vscode_lib.deactivate () in
  Promise.return ()

(* see {{:https://code.visualstudio.com/api/references/vscode-api#Extension}
   activate() *)
let () =
  Js_of_ocaml.Js.(export "activate" (wrap_callback activate));
  Js_of_ocaml.Js.(export "deactivate" (wrap_callback deactivate))
