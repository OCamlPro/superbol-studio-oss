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
  context: Vscode.ExtensionContext.t;
  mutable language_client: LanguageClient.t option
}
type client = LanguageClient.t


let id = "superbol-free-lsp"

let name = "SuperBOL Language Server"

let make ~context = { context; language_client = None }

let client { language_client; _ } = language_client
let subscribe_disposable { context; _ } disposable =
  Vscode.ExtensionContext.subscribe context ~disposable

let stop_language_server t =
  match t.language_client with
  | None ->
      Promise.return ()
  | Some client ->
      t.language_client <- None;
      if LanguageClient.isRunning client then
        LanguageClient.stop client
      else
        Promise.return ()

let start_language_server ({ context; _ } as t) =
  let open Promise.Syntax in
  let* () = stop_language_server t in
  let client =
    match Superbol_languageclient.server_access ~context with
    | Sub_process serverOptions ->
        LanguageClient.make () ~id ~name ~serverOptions
          ~clientOptions:(Superbol_languageclient.client_options ())
    | TCP { host; port } ->
        LanguageClient.from_stream ~id ~name begin fun () ->
          let socket = Node.Net.Socket.(connect (make ())) ~host ~port in
          Node.Net.Socket.on socket @@ `Connect begin fun () ->
            subscribe_disposable t @@
            Vscode.Window.setStatusBarMessage ()
              ~text:(Printf.sprintf "SuperBOL LSP client successfully connected \
                                     to %s on port %u" host port)
              ~hide:(`AfterTimeout 10000)
          end;
          Promise.return @@
          Vscode_languageclient.StreamInfo.create ()
            ~writer:socket
            ~reader:socket
        end
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
  let write_project_config_for ?uri client =
    let assoc = match uri with
      | Some uri ->
          ["uri", Jsonoo.Encode.string @@ Vscode.Uri.toString uri ()]
      | None ->
          [] (* send without URI: the server will consider every known folder *)
    in
    Vscode_languageclient.LanguageClient.sendRequest client ()
      ~meth:"superbol/writeProjectConfiguration"
      ~data:(Jsonoo.Encode.object_ assoc) |>
    Promise.(then_ ~fulfilled:(fun _ -> return ()))
  in
  match client instance, current_document_uri ?text_editor () with
  | Some client, uri ->
      write_project_config_for ?uri client
  (* | Some client, None -> *)
  (*     Promise.race_list @@ List.map begin fun workspace_folder -> *)
  (*       let uri = Vscode.WorkspaceFolder.uri workspace_folder in *)
  (*       write_project_config_for ~uri ~client *)
  (*     end @@ Vscode.Workspace.workspaceFolders () *)
  | None, _ ->
      (* TODO: is there a way to activate the extension from here?  Starting the
         client/instance seems to launch two distinct LSP server processes. *)
      Promise.(then_ ~fulfilled:(fun _ -> return ())) @@
      Vscode.Window.showErrorMessage ()
        ~message:"The SuperBOL LSP client is not running; please retry after a \
                  COBOL file has been opened"


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


let open_cfg ?text_editor instance =
  let open_cfg_for ?uri client =
    let uri = match uri with
      | Some uri ->
          Jsonoo.Encode.string @@ Vscode.Uri.path uri
      | None ->
          Jsonoo.Encode.string ""
    in
    Vscode_languageclient.LanguageClient.sendRequest client ()
      ~meth:"superbol/openCFG"
      ~data:uri |>
    Promise.(then_ ~fulfilled:(fun res ->
      let dotfile = Jsonoo.Decode.string res in
      let webviewpanel = Vscode.Window.createWebviewPanel ~viewType:"cfg" ~title:"CFG webview"
      ~showOptions:(Vscode.ViewColumn.Two) in
      let webview = Vscode.WebviewPanel.webview webviewpanel in
      Vscode.WebView.set_html webview dotfile;
      return ()
      ))
  in
  match client instance, current_document_uri ?text_editor () with
  | Some client, uri ->
      open_cfg_for ?uri client
  | None, _ ->
      (* TODO: is there a way to activate the extension from here?  Starting the
         client/instance seems to launch two distinct LSP server processes. *)
      Promise.(then_ ~fulfilled:(fun _ -> return ())) @@
      Vscode.Window.showErrorMessage ()
        ~message:"The SuperBOL LSP client is not running; please retry after a \
                  COBOL file has been opened"


