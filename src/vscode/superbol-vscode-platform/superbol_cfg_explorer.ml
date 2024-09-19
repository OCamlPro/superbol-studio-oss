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

open Vscode

type cfg_type = Graphviz | D3_arc_diagram

let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  Fun.protect
    begin fun () -> really_input_string ch (in_channel_length ch) end
    ~finally:begin fun () -> close_in ch end

let graphviz_html = ref None
let d3_arc_html = ref None
let get_html_js_content ~extension_uri typ =
  match typ, !graphviz_html, !d3_arc_html  with
  | D3_arc_diagram, _, Some value
  | Graphviz, Some value, _ -> Ok(`CompleteHtml value)
  | _ ->
    let html_uri = Uri.joinPath extension_uri
        ~pathSegments:
          ["assets"; match typ with
            | Graphviz -> "cfg-dot-renderer.html"
            | D3_arc_diagram -> "cfg-arc-renderer.html"] in
    try
      let html = read_whole_file @@ Uri.fsPath html_uri in
      let js = match typ with
        | Graphviz -> "cfg-dot.js"
        | D3_arc_diagram -> "cfg-arc.js" in
      let localResource =
        Uri.joinPath extension_uri ~pathSegments:["assets"; js] in
      Ok( `IncompleteHtml (html, localResource))
    with Sys_error e -> Error(e)
       | End_of_file -> Error("End_of_file")

let setup_html_js_content ~webview ~typ html_js =
  match html_js with
  | `CompleteHtml html -> html
  | `IncompleteHtml (html, js_path) ->
    let html_content =
      let path = Uri.toString
          (WebView.asWebviewUri webview ~localResource:js_path) () in
      Printf.sprintf "%s<script src=\"%s\"></script>"
        html path in
    begin match typ with
      | Graphviz -> graphviz_html := Some (html_content)
      | D3_arc_diagram -> d3_arc_html := Some (html_content) end;
    html_content


let _log message = ignore(Window.showInformationMessage () ~message)

(* DECORATION TYPE *)

let decorationType =
  let backgroundColor = Ojs.string_to_js "#75ff3388" in
  let options = Ojs.obj [|("backgroundColor", backgroundColor)|] in
  Window.createTextEditorDecorationType ~options

(* GRAPH FROM LSP *)

type graph = {
  string_repr_dot: string;
  string_repr_d3: string;
  nodes_pos: (string * Jsonoo.t) list;
  name: string;
}

let decode_graph res =
  let string_repr_dot =
    Jsonoo.Decode.field "string_repr_dot" Jsonoo.Decode.string res in
  let string_repr_d3 =
    Jsonoo.Decode.field "string_repr_d3" Jsonoo.Decode.string res in
  let nodes_pos = Jsonoo.Decode.field "nodes_pos" Jsonoo.Decode.(dict id) res in
  let nodes_pos = Hashtbl.to_seq nodes_pos |> List.of_seq in
  let name = Jsonoo.Decode.field "name" Jsonoo.Decode.string res in
  { name; nodes_pos; string_repr_dot; string_repr_d3 }


(* WEBVIEW MANAGEMENT *)

type stored_data = {
  webview_panel: WebviewPanel.t;
  graph: graph;
  render_options: Jsonoo.t;
}

let webview_panels = Hashtbl.create 1
let window_listener = ref None

let webviewpanel_disposal ~filename ~typ () =
  Hashtbl.remove webview_panels (filename, typ);
  if Hashtbl.length webview_panels == 0
  then (
    Option.iter Disposable.dispose !window_listener;
    window_listener := None);
  match Window.activeTextEditor () with
  | None -> ()
  | Some text_editor ->
    let uri = TextEditor.document text_editor
              |> TextDocument.uri in
    if String.equal filename @@ Uri.path uri
    then TextEditor.setDecorations text_editor
        ~decorationType ~rangesOrOptions:(`Ranges [])

let create_or_get_webview ~graph ~uri ~typ =
  let render_options = Jsonoo.Encode.object_
      ["graph_name", Jsonoo.Encode.string graph.name] in
  let filename = Uri.path uri in
  match Hashtbl.find_opt webview_panels (filename, typ) with
  | Some { webview_panel; _ } ->
    WebviewPanel.reveal webview_panel ();
    Hashtbl.replace webview_panels (filename, typ)
      { webview_panel; graph; render_options };
    WebviewPanel.webview webview_panel, false
  | None ->
    let viewType = match typ with
      | Graphviz -> "superbol.cfg.dot"
      | D3_arc_diagram -> "superbol.cfg.arc" in
    let webview_panel = Window.createWebviewPanel ~viewType
        ~title:"SuperBOL CFG Viewer" ~showOptions:(ViewColumn.Beside) in
    let _ : Disposable.t =
      WebviewPanel.onDidDispose webview_panel ()
        ~listener:(webviewpanel_disposal ~filename ~typ)
        ~thisArgs:Ojs.null ~disposables:[] in
    let webview = WebviewPanel.webview webview_panel in
    WebView.set_options webview (WebviewOptions.create ~enableScripts:true ());
    Hashtbl.add webview_panels (filename, typ)
      { webview_panel; graph; render_options };
    webview, true

let webview_data_find_opt ~uri ~typ =
  Hashtbl.find_opt webview_panels (Uri.path uri, typ)
  |> Option.map begin fun { webview_panel; graph; render_options } ->
    WebviewPanel.webview webview_panel, graph, render_options
  end

let update_webview_data ~uri ~typ ?graph ?render_options () =
  let filename = Uri.path uri in
  match Hashtbl.find_opt webview_panels (filename, typ) with
  | Some { webview_panel; render_options=current_ro; graph=current_g } ->
    let render_options = Option.value ~default:current_ro render_options in
    let graph = Option.value ~default:current_g graph in
    Hashtbl.replace webview_panels (filename, typ)
      { webview_panel; graph; render_options }
  | None -> ()

(* CLICK ON NODE *)

let on_click ~nodes_pos ~text_editor arg =
  let open Vscode in
  let uri = TextDocument.uri @@ TextEditor.document text_editor in
  let column = TextEditor.viewColumn text_editor in
  let node = Ojs.get_prop_ascii arg "node" |> Ojs.int_of_js |> string_of_int in
  List.assoc_opt node nodes_pos
  |> Option.iter begin fun range ->
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
  end

let setup_window_listener ~client =
  let listener event =
    if TextEditorSelectionChangeEvent.kind event ==
       TextEditorSelectionChangeKind.Command
    then ()
    else
      match TextEditorSelectionChangeEvent.selections event with
      | [] -> ()
      | selection::_ ->
        let text_editor = TextEditorSelectionChangeEvent.textEditor event in
        TextEditor.setDecorations text_editor ~decorationType
          ~rangesOrOptions:(`Ranges []);
        let uri = TextEditor.document text_editor |> TextDocument.uri in
        let process_selection_change webview =
          let pos_start = Selection.start selection in
          let data =
            let uri = Jsonoo.Encode.string @@ Uri.path uri in
            Jsonoo.Encode.object_
              ["uri", uri;
               "line", Jsonoo.Encode.int @@ Position.line pos_start;
               "character", Jsonoo.Encode.int @@ Position.character pos_start]
          in
          let _ : bool Promise.t =
            Vscode_languageclient.LanguageClient.sendRequest client ()
              ~meth:"superbol/findProcedure" ~data
            |> Promise.(then_ ~fulfilled:begin fun res ->
                let ojs = Ojs.empty_obj () in
                Ojs.set_prop_ascii ojs "type" (Ojs.string_to_js "focused_proc");
                Ojs.set_prop_ascii ojs "procedure" @@ Jsonoo.t_to_js res;
                WebView.postMessage webview ojs
              end)
          in ()
        in
        let webview = webview_data_find_opt ~uri ~typ:Graphviz in
        begin match webview with
          | None -> ()
          | Some (webview, _, _) -> process_selection_change webview end;
        let webview = webview_data_find_opt ~uri ~typ:D3_arc_diagram in
        match webview with
        | None -> ()
        | Some (webview, _, _) -> process_selection_change webview
  in
  let disposable_listener =
    match !window_listener with
    | Some listener -> listener
    | None -> Window.onDidChangeTextEditorSelection () ()
                ~listener ~thisArgs:Ojs.null ~disposables:[] in
  window_listener := Some disposable_listener

(* MESSAGE MANAGER *)

let send_graph ~typ webview graph =
  let ojs = Ojs.empty_obj () in
  Ojs.set_prop_ascii ojs "type" (Ojs.string_to_js "graph_content");
  if typ == Graphviz
  then Ojs.set_prop_ascii ojs "dot" (Ojs.string_to_js graph.string_repr_dot);
  Ojs.set_prop_ascii ojs "graph" (Ojs.string_to_js graph.string_repr_d3);
  let _ : bool Promise.t = WebView.postMessage webview ojs
  in ()

let get_and_send_graph ~client ~uri ~typ ~webview ~render_options =
  let data =
    let uri = Jsonoo.Encode.string @@ Uri.path uri in
    Jsonoo.Encode.object_ ["uri", uri; "render_options", render_options] in
  let _ : unit Promise.t =
    Vscode_languageclient.LanguageClient.sendRequest client ()
      ~meth:"superbol/getCFG" ~data
    |> Promise.then_ ~fulfilled:begin fun jsonoo_res ->
      let graphs = (Jsonoo.Decode.list decode_graph) jsonoo_res in
      match graphs with
      | [] ->
        Window.showErrorMessage ()
          ~message:"Unable to perform operation, try reloading the CFG"
        |> Promise.map (Fun.const ())
      | graph::_ ->
        update_webview_data ~uri ~typ ~graph ~render_options ();
        send_graph ~typ webview graph;
        Promise.return ()
    end
  in ()

let on_graph_update ~webview ~client ~uri ~typ name arg =
  let render_options =
    Ojs.get_prop_ascii arg "renderOptions"
    |> begin fun ojs ->
      Ojs.set_prop_ascii ojs "graph_name" @@ Ojs.string_to_js name;
      ojs end
    |> Jsonoo.t_of_js in
  get_and_send_graph ~client ~uri ~typ ~webview ~render_options

let on_message ~client ~text_editor ~typ arg =
  let uri = TextEditor.document text_editor |> TextDocument.uri in
  let request_type = Ojs.get_prop_ascii arg "type" |> Ojs.string_of_js in
  webview_data_find_opt ~uri ~typ
  |> Option.iter begin fun (webview, graph, _) ->
    match request_type with
    | "click" ->
      on_click ~nodes_pos:graph.nodes_pos ~text_editor arg
    | "graph_update" ->
      on_graph_update ~client ~webview ~uri ~typ graph.name arg
    | "ready" ->
      send_graph ~typ webview graph
    | _ -> ()
  end

let open_cfg_for ~typ ~text_editor ~extension_uri client =
  let open Promise in
  let uri = TextEditor.document text_editor |> TextDocument.uri in
  let data =
    let uri = Jsonoo.Encode.string @@ Uri.path uri in
    Jsonoo.Encode.object_ ["uri", uri]
  in
  match get_html_js_content ~extension_uri typ with
  | Error e ->
    let _ : _ option Promise.t = Window.showErrorMessage
        ~message:("Unable to display control-flow: " ^ e) () in
    return ()
  | Ok html_js ->
    Vscode_languageclient.LanguageClient.sendRequest client ()
      ~meth:"superbol/getCFG" ~data
    |> then_ ~fulfilled:begin fun jsonoo_graphs ->
      let graphs = (Jsonoo.Decode.list decode_graph) jsonoo_graphs in
      Window.showQuickPick ~items:(Stdlib.List.map (fun g -> g.name) graphs) ()
      |> then_ ~fulfilled:begin function
        | None -> return ()
        | Some name ->
          let graph = Stdlib.List.find begin fun g ->
              String.equal g.name name end graphs in
          let webview, is_new = create_or_get_webview ~graph ~typ ~uri in
          let html_content = setup_html_js_content ~webview ~typ html_js in
          let _ : Disposable.t =
            WebView.onDidReceiveMessage webview ()
              ~listener:(on_message ~client ~text_editor ~typ)
              ~thisArgs:Ojs.null ~disposables:[]
          in
          if is_new
          then WebView.set_html webview html_content
          else send_graph ~typ webview graph;
          setup_window_listener ~client;
          return ()
      end
    end

let open_cfg ?text_editor ~typ instance =
  let text_editor = match text_editor with
    | None -> Window.activeTextEditor ()
    | e -> e in
  match Superbol_instance.client instance, text_editor with
  | Some client, Some text_editor ->
    let extension_uri = ExtensionContext.extensionUri
      @@ Superbol_instance.context instance in
    open_cfg_for ~typ ~extension_uri ~text_editor client
  | _ -> Promise.return ()
