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

let config = Vscode.Workspace.getConfiguration ()

let serverOptions extension =
  let args = [ "lsp" ] in
  let ojs = Vscode.WorkspaceConfiguration.get config ~section:"superbol.path" in
  let cmd_opt =
    match ojs with
    | None -> None
    | Some o when Ojs.is_null o -> None
    | Some o -> Some (Ojs.string_of_js o)
  in
  match cmd_opt with
  | Some command ->
    Vscode_languageclient.ServerOptions.create ()
        ~command ~args
  | None ->
    let superbol_path =
        Vscode.ExtensionContext.asAbsolutePath extension
            ~relativePath:"_dist/superbol-free.bc.js"
    in
    Vscode_languageclient.ServerOptions.create ()
      ~command:"node"
      ~args:(superbol_path :: args)

let clientOptions =
  Vscode_languageclient.ClientOptions.create ()
    ~documentSelector:[|
      `Filter (Vscode_languageclient.DocumentFilter.createLanguage ()
                 ~language:"cobol");
    |]
