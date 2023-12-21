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
  Vscode_languageclient.ServerOptions.create ()
    ~command:(match Superbol_workspace.superbol_exe () with
        | None -> bundled_superbol
        | Some cmd -> cmd)
    ~args:["lsp"]

let clientOptions () =
  Vscode_languageclient.ClientOptions.create ()
    ~documentSelector:[|
      `Filter (Vscode_languageclient.DocumentFilter.createLanguage ()
                 ~language:"cobol");
    |]
