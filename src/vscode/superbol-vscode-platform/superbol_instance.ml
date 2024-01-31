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
type client = LanguageClient.t

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


let current_document_uri ?text_editor () =
  match
    match text_editor with None -> Vscode.Window.activeTextEditor () | e -> e
  with
  | None -> None
  | Some e -> Some (Vscode.TextDocument.uri @@ Vscode.TextEditor.document e)


let write_project_config ?text_editor instance =
  match client instance, current_document_uri ?text_editor () with
  | None, _ | _, None ->                             (* ignore; TODO; message? *)
      Promise.return ()
  | Some client, Some uri ->
      Vscode_languageclient.LanguageClient.sendRequest client ()
        ~meth:"superbol/writeProjectConfiguration"
        ~data:(Jsonoo.Encode.(object_ [
            "uri", string @@ Vscode.Uri.toString uri ();
          ])) |>
      Promise.(then_ ~fulfilled:(fun _ -> return ()))


let get_project_config instance =
  let open Promise.Syntax in
  match client instance, Vscode.Window.activeTextEditor () with
  | None, _ ->
      Promise.return @@ Error "SuperBOL client is not running"
  | _, None ->
      Promise.return @@ Error "Found no active text editor"
  | Some client, Some textEditor ->
      let document = Vscode.TextEditor.document textEditor in
      let uri = Vscode.TextDocument.uri document in
      let* assoc =
        Vscode_languageclient.LanguageClient.sendRequest client ()
          ~meth:"superbol/getProjectConfiguration"
          ~data:(Jsonoo.Encode.(object_ [
              "uri", string @@ Vscode.Uri.toString uri ();
            ]))
      in
      Promise.Result.return @@ Jsonoo.Decode.(dict id) assoc
