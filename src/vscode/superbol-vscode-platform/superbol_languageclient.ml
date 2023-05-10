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

let serverOptions =
  let command =
    match Vscode.WorkspaceConfiguration.get ~section:"superbol.path" config with
    | Some o -> Ojs.string_of_js o
    | None -> "superbol-free"
  in
  Vscode_languageclient.ServerOptions.create ()
    ~command
    ~args:["lsp"]

let clientOptions =
  Vscode_languageclient.ClientOptions.create ()
    ~documentSelector:[|
      `Filter (Vscode_languageclient.DocumentFilter.createLanguage ()
                 ~language:"cobol");
    |]
