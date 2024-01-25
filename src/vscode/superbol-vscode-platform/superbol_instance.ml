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

open Vscode_languageclient

type t = {
  mutable bundled_superbol : string;
  mutable language_client: LanguageClient.t option
}

let make ~bundled_superbol () = {
  bundled_superbol;
  language_client = None
}

let client { language_client; _ } = language_client

let stop_language_server t =
  match t.language_client with
  | None -> Promise.return ()
  | Some client ->
    t.language_client <- None;
    if LanguageClient.isRunning client then
      LanguageClient.stop client
    else
      Promise.return ()

let start_language_server t =
  let open Promise.Syntax in
  let* () = stop_language_server t in
  let serverOptions =
    Superbol_languageclient.serverOptions
      ~bundled_superbol:t.bundled_superbol
  in
  let clientOptions = Superbol_languageclient.clientOptions () in
  let client =
    LanguageClient.make ()
      ~id: "superbol-free-lsp"
      ~name: "SuperBOL Language Server"
      ~serverOptions
      ~clientOptions
  in
  let+ () = LanguageClient.start client in
  t.language_client <- Some client
