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

let serverOptions ~bundled_superbol ~storage_uri =
  let storage_options = match storage_uri with
    | None ->
        []
    | Some uri ->
        [ "--storage-directory"; Vscode.Uri.fsPath uri ] (* CHECKME: or .path? *)
  in
  Vscode_languageclient.ServerOptions.create ()
    ~command:(match Superbol_workspace.superbol_exe () with
        | None -> bundled_superbol
        | Some cmd -> cmd)
    ~args: ("lsp" :: storage_options)

let clientOptions () =
  Vscode_languageclient.ClientOptions.create ()
    ~documentSelector:[|
      `Filter (Vscode_languageclient.DocumentFilter.createLanguage ()
                 ~language:"cobol");
    |]
