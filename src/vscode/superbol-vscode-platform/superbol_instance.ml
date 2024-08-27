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

let webview_panels = Hashtbl.create 1
let create_or_get_webview ~uri =
  let filename = Vscode.Uri.path uri in
  Vscode.WebviewPanel.webview @@
  match Hashtbl.find_opt webview_panels filename with
  | Some webview_panel ->
    Vscode.WebviewPanel.reveal webview_panel ();
    webview_panel
  | None ->
    let webview_panel = Vscode.Window.createWebviewPanel
        ~viewType:"CFG" ~title:"COBOL CFG Viewer"
        ~showOptions:(Vscode.ViewColumn.Beside) in
    ignore(
      Vscode.WebviewPanel.onDidDispose webview_panel ()
        ~listener:(fun () -> Hashtbl.remove webview_panels filename)
        ~thisArgs:Ojs.null ~disposables:[]);
    Hashtbl.add webview_panels filename webview_panel;
    webview_panel

let _log message = ignore(Vscode.Window.showInformationMessage () ~message)

let create_decoration_type () =
  let backgroundColor = Ojs.string_to_js "#75ff3388" in
  let options = Ojs.obj [|("backgroundColor", backgroundColor)|] in
  Vscode.Window.createTextEditorDecorationType ~options

let on_click ~nodes_pos ~decorationType ~text_editor arg =
  let open Vscode in
  let uri = TextDocument.uri @@ TextEditor.document text_editor in
  let column = TextEditor.viewColumn text_editor in
  let node = Ojs.get_prop_ascii arg "node" |> Ojs.string_of_js in
  match Hashtbl.find_opt nodes_pos node with
  | None -> ()
  | Some range ->
    let range = Range.t_of_js @@ Jsonoo.t_to_js range in
    let _ =
      Window.showTextDocument ~document:(`Uri uri) ?column ()
      |> Promise.then_ ~fulfilled:(fun text_editor ->
          let selection = Selection.makePositions
              ~anchor:(Range.start range) ~active:(Range.start range) in
          TextEditor.revealRange text_editor ~range
            ~revealType:TextEditorRevealType.InCenterIfOutsideViewport ();
          TextEditor.set_selection text_editor selection;
          TextEditor.setDecorations text_editor ~decorationType
            ~rangesOrOptions:(`Ranges [range]);
          Promise.return ())
    in ()

let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let open_cfg_for ?(d3=false) ~text_editor ~extension_uri client =
  let open Vscode in
  let uri = TextEditor.document text_editor |> TextDocument.uri in
  let data =
    let uri = Jsonoo.Encode.string @@ Vscode.Uri.path uri in
    Jsonoo.Encode.object_ ["uri", uri; "is_d3", Jsonoo.Encode.bool d3]
  in
  let decorationType = create_decoration_type () in
  Vscode_languageclient.LanguageClient.sendRequest client ()
    ~meth:"superbol/openCFG" ~data
  |> Promise.(then_ ~fulfilled:(fun res ->
      let graph_content =
        Jsonoo.Decode.field "string_repr" Jsonoo.Decode.string res in
      let nodes_pos = Jsonoo.Decode.field "nodes_pos" Jsonoo.Decode.(dict id) res in
      let webview = create_or_get_webview ~uri in
      let img_uri = Uri.joinPath extension_uri
          ~pathSegments:["assets"; if d3 then "cfg-d3-renderer.html" else "cfg-dot-renderer.html"] in
      let html_file =
        read_whole_file @@ Uri.fsPath img_uri in
      let html = Ez_subst.V2.EZ_SUBST.string ~sep:'%' ~brace:(fun () _ -> graph_content) ~ctxt:() html_file in
      WebView.set_html webview html;
      WebView.set_options webview (WebviewOptions.create ~enableScripts:true ());
      ignore(
        WebView.onDidReceiveMessage webview ()
          ~listener:(on_click ~text_editor ~decorationType ~nodes_pos)
          ~thisArgs:Ojs.null ~disposables:[]);
      return ()
    ))

let open_cfg ?(d3=false) ?text_editor instance =
  let text_editor = match text_editor with
    | None -> Vscode.Window.activeTextEditor ()
    | e -> e in
  match client instance, text_editor with
  | Some client, Some text_editor ->
    let extension_uri = Vscode.ExtensionContext.extensionUri instance.context in
    open_cfg_for ~d3 ~extension_uri ~text_editor client
  | _ -> Promise.return ()

(* debug TO REMOVE *)
let debugWebviewPanelRef = ref None
let open_webview ?text_editor instance =
  let open Vscode in
  let open_cfg_for ~text _client =
    let webviewPanel = match !debugWebviewPanelRef with
    | None ->
      Window.createWebviewPanel
        ~viewType:"cfg" ~title:"Tester webview"
        ~showOptions:(Vscode.ViewColumn.Two)
    | Some wvp -> wvp in
    debugWebviewPanelRef := Some webviewPanel;
    let webview = Vscode.WebviewPanel.webview webviewPanel in
    Vscode.WebView.set_html webview text;
    Vscode.WebView.set_options webview (Vscode.WebviewOptions.create ~enableScripts:true ());
    let thisArgs, disposables = Ojs.null, [] in
    ignore(
      WebviewPanel.onDidDispose webviewPanel ()
        ~listener:(fun () -> debugWebviewPanelRef:=None)
        ~thisArgs ~disposables);
    let listener arg =
      let typ = Ojs.type_of arg in
      let com = Ojs.get_prop_ascii arg "command" |> Ojs.string_of_js in
      let message = "Listener clicked " ^ typ ^ "    " ^ com in
      let _ = Vscode.Window.showErrorMessage () ~message in
      ()
    in
    let _ = Vscode.WebView.onDidReceiveMessage webview ~listener ~thisArgs:Ojs.null ~disposables:[] () in
    (* let onDidReceiveMessage ~listener ?thisArgs ?disposables () = *)
    (*   ignore(listener, thisArgs, disposables); *)
    (*   Vscode.Disposable.make ~dispose:Fun.id *)
    (* in *)
    (* let postMessage _ojs = Promise.return true in *)
    (* let webview = Vscode.WebView.create *)
    (*     ~onDidReceiveMessage *)
    (*     ~cspSource:"" *)
    (*     ~close:Fun.id *)
    (*     ~asWebviewUri:Fun.id *)
    (*     ~html:text *)
    (*     ~postMessage *)
    (*     ~options:(Vscode.WebviewOptions.create ~enableScripts:true ()) *)
    (* in *)
    (* Vscode.WebviewPanel.set_webview newWebviewPanel webview; *)
    WebviewPanel.reveal webviewPanel ~preserveFocus:true ();
    Promise.return ()
  in
  let current_text ?text_editor () =
    match
      match text_editor with None -> Vscode.Window.activeTextEditor () | e -> e
    with
    | None -> None
    | Some e -> Some (Vscode.TextDocument.getText (Vscode.TextEditor.document e) ())
  in
  match client instance, current_text ?text_editor () with
  | Some client, Some text ->
    open_cfg_for ~text client
  | _ ->
    (* TODO: is there a way to activate the extension from here?  Starting the
       client/instance seems to launch two distinct LSP server processes. *)
    Promise.(then_ ~fulfilled:(fun _ -> return ())) @@
    Vscode.Window.showErrorMessage ()
      ~message:"The SuperBOL LSP client is not running; please retry after a \
                COBOL file has been opened"



