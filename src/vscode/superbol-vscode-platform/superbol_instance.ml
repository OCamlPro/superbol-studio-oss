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
  mutable language_client: LanguageClient.t option
}

let make () = {
  language_client = None
}

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
  let serverOptions = Superbol_languageclient.serverOptions () in
  let clientOptions = Superbol_languageclient.clientOptions () in
  let client = LanguageClient.make
    ~id:"cobolServer"
    ~name:"Cobol Server"
    ~serverOptions ~clientOptions () in
  let+ () = LanguageClient.start client in
  t.language_client <- Some client
