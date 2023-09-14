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

let command =
  Vscode.Workspace.getConfiguration ()
  |> Vscode.WorkspaceConfiguration.get ~section:"superbol.path"
  |> function Some o -> Ojs.string_of_js o
            | None -> "superbol"

let args = ["x-lsp"]

let serverOptions = Vscode_languageclient.ServerOptions.create
    ~command
    ~args
    ()

let documentSelector =
  [| `Filter (Vscode_languageclient.DocumentFilter.createLanguage ~language:"cobol" ()) |]

let clientOptions = Vscode_languageclient.ClientOptions.create ~documentSelector ()

