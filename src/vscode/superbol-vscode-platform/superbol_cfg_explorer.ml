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

let _log message = ignore(Vscode.Window.showInformationMessage () ~message)

let webview_panels = Hashtbl.create 1
let window_listener = ref None
let create_or_get_webview ~decorationType ~uri =
  let filename = Vscode.Uri.path uri in
  Vscode.WebviewPanel.webview @@
  match Hashtbl.find_opt webview_panels filename with
  | Some webview_panel ->
    Vscode.WebviewPanel.reveal webview_panel ();
    let webview = Vscode.WebviewPanel.webview webview_panel in
    let _ : bool Promise.t = Vscode.WebView.postMessage webview (Ojs.int_to_js 2) in
    webview_panel
  | None ->
    let webview_panel = Vscode.Window.createWebviewPanel
        ~viewType:"CFG" ~title:"COBOL CFG Viewer"
        ~showOptions:(Vscode.ViewColumn.Beside) in
    ignore(
      Vscode.WebviewPanel.onDidDispose webview_panel ()
        ~listener:begin fun () ->
          Hashtbl.remove webview_panels filename;
          if Hashtbl.length webview_panels == 0
          then (
            ignore(Option.map Vscode.Disposable.dispose !window_listener);
            window_listener := None);
          match Vscode.Window.activeTextEditor () with
          | None -> ()
          | Some text_editor ->
            let uri = Vscode.TextEditor.document text_editor
                      |> Vscode.TextDocument.uri in
            if String.equal filename @@ Vscode.Uri.path uri
            then Vscode.TextEditor.setDecorations text_editor
                ~decorationType ~rangesOrOptions:(`Ranges []);
        end ~thisArgs:Ojs.null ~disposables:[]);
    Hashtbl.add webview_panels filename webview_panel;
    webview_panel

let webview_find_opt ~uri =
  Hashtbl.find_opt webview_panels @@ Vscode.Uri.path uri
  |> Option.map Vscode.WebviewPanel.webview

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
    let _ : unit Promise.t =
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

let setup_window_listener ~client =
  let open Vscode in
  let listener event =
    let text_editor = TextEditorSelectionChangeEvent.textEditor event in
    let uri = TextEditor.document text_editor |> TextDocument.uri in
    let webview = webview_find_opt ~uri in
    match webview with
    | None -> ()
    | Some webview ->
      match TextEditorSelectionChangeEvent.selections event with
      | [] -> ()
      | selection::_ ->
        let pos_start = Selection.start selection in
        let data =
          let uri = Jsonoo.Encode.string @@ Vscode.Uri.path uri in
          Jsonoo.Encode.object_
            ["uri", uri;
             "line", Jsonoo.Encode.int @@ Position.line pos_start;
             "character", Jsonoo.Encode.int @@ Position.character pos_start]
        in
        ignore(
          Vscode_languageclient.LanguageClient.sendRequest client ()
            ~meth:"superbol/findProcedure" ~data
          |> Promise.(then_ ~fulfilled:begin fun res ->
              WebView.postMessage webview @@ Jsonoo.t_to_js res
            end))
  in
  let disposable_listener =
    match !window_listener with
    | Some listener -> listener
    | None -> Window.onDidChangeTextEditorSelection () ()
                ~listener ~thisArgs:Ojs.null ~disposables:[] in
  window_listener := Some disposable_listener

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
      let webview = create_or_get_webview ~decorationType ~uri in
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
      setup_window_listener ~client;
      return ()
    ))

let open_cfg ?(d3=false) ?text_editor instance =
  let text_editor = match text_editor with
    | None -> Vscode.Window.activeTextEditor ()
    | e -> e in
  match Superbol_instance.client instance, text_editor with
  | Some client, Some text_editor ->
    let extension_uri = Vscode.ExtensionContext.extensionUri
      @@ Superbol_instance.context instance in
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
      let _ : _ option Promise.t = Vscode.Window.showErrorMessage () ~message in
      ()
    in
    let _ : Disposable.t = Vscode.WebView.onDidReceiveMessage webview ~listener ~thisArgs:Ojs.null ~disposables:[] () in
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
  match Superbol_instance.client instance, current_text ?text_editor () with
  | Some client, Some text ->
    open_cfg_for ~text client
  | _ ->
    (* TODO: is there a way to activate the extension from here?  Starting the
       client/instance seems to launch two distinct LSP server processes. *)
    Promise.(then_ ~fulfilled:(fun _ -> return ())) @@
    Vscode.Window.showErrorMessage ()
      ~message:"The SuperBOL LSP client is not running; please retry after a \
                COBOL file has been opened"



