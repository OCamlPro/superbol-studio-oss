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

let serverOptions ~bundled_superbol =
  let config = Vscode.Workspace.getConfiguration () in
  let cmd_opt =
    match Vscode.WorkspaceConfiguration.get ~section:"superbol.path" config with
    | None -> None
    | Some o when Ojs.is_null o -> None
    | Some o ->
      match Ojs.string_of_js o with
      | "" -> None
      | s -> Some s
  in
  let command = Option.value ~default:bundled_superbol cmd_opt in
  Vscode_languageclient.ServerOptions.create ()
    ~command
    ~args:["lsp"]

let clientOptions () =
  Vscode_languageclient.ClientOptions.create ()
    ~documentSelector:[|
      `Filter (Vscode_languageclient.DocumentFilter.createLanguage ()
                 ~language:"cobol");
    |]
