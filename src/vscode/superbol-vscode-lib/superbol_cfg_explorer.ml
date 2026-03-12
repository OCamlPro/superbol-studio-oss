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

module VS = Vscode

type cfg_type = Graphviz | D3_arc_diagram

let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename
  in Fun.protect
    begin fun () -> really_input_string ch (in_channel_length ch) end
    ~finally:begin fun () -> close_in ch end

let graphviz_html = ref None
let d3_arc_html = ref None
let get_html_js_content ~extension_uri typ =
  match typ, !graphviz_html, !d3_arc_html with
  | D3_arc_diagram, _, Some value
  | Graphviz, Some value, _ -> Ok(`CompleteHtml value)
  | _ ->
    let html_uri = VS.Uri.joinPath extension_uri
        ~pathSegments:
          ["assets"; match typ with
            | Graphviz -> "cfg-dot-renderer.html"
            | D3_arc_diagram -> "cfg-arc-renderer.html"]
    in
    try
      let html = read_whole_file @@ VS.Uri.fsPath html_uri in
      let file_name = match typ with
        | Graphviz -> "cfg-dot"
        | D3_arc_diagram -> "cfg-arc"
      in
      let vendor_scripts =
        let vendor name =
          VS.Uri.joinPath extension_uri
            ~pathSegments:["assets"; "vendor"; name]
        in
        match typ with
        | Graphviz ->
          [vendor "d3.v7.min.js";
           vendor "hpcc-wasm.v2.20.0.umd.js";
           vendor "d3-graphviz.v5.6.0.js"]
        | D3_arc_diagram ->
          [vendor "d3.v7.min.js"]
      in
      let localResource ext =
        VS.Uri.joinPath extension_uri ~pathSegments:["assets"; file_name ^ ext]
      in Ok (`IncompleteHtml (
        html,
        vendor_scripts,
        localResource ".js",
        localResource ".css"
      ))
    with Sys_error e -> Error(e)
       | End_of_file -> Error("End_of_file")

let setup_html_js_content ~webview ~typ html_js =
  match html_js with
  | `CompleteHtml html -> html
  | `IncompleteHtml (html, vendor_scripts, js_path, css_path) ->
    let html_content =
      let vendor_tags = String.concat ""
          (List.map (fun uri ->
               let src = VS.Uri.toString
                   (VS.WebView.asWebviewUri webview ~localResource:uri) ()
               in
               Printf.sprintf "<script src=\"%s\"></script>" src)
              vendor_scripts)
      in
      let js_path = VS.Uri.toString
          (VS.WebView.asWebviewUri webview ~localResource:js_path) ()
      in
      let css_path = VS.Uri.toString
          (VS.WebView.asWebviewUri webview ~localResource:css_path) ()
      in Printf.sprintf "%s%s<script src=\"%s\"></script>\
                         <link rel=\"stylesheet\" type=\"text/css\" href=\"%s\"/>"
        html vendor_tags js_path css_path
    in
    begin match typ with
      | Graphviz -> graphviz_html := Some (html_content)
      | D3_arc_diagram -> d3_arc_html := Some (html_content) end;
    html_content


let _log message = ignore(VS.Window.showInformationMessage () ~message)

(* DECORATION TYPE *)
(* TODO: we might want to do this via CSS tags to enable user customization *)
let decorationType =
  let backgroundColor = Ojs.string_to_js "#75ff3388" in
  let options = Ojs.obj [|("backgroundColor", backgroundColor)|] in
  VS.Window.createTextEditorDecorationType ~options

(* PERSISTENT OPTION MANAGEMENT *)

let state = ref None

let update_state ~key value =
  match !state with
  | None -> ()
  | Some state ->
    let _ : Promise.void = VS.Memento.update state ~key ~value
    in ()

let get_state_value ~key =
  match !state with
  | None -> None
  | Some state -> VS.Memento.get ~key state

(* GRAPH FROM LSP *)

type graph = {
  string_repr_dot: string;
  string_repr_d3: string;
  nodes_pos: (string * Jsonoo.t) list;
  name: string;
}

let decode_graph res =
  let string_repr_dot =
    Jsonoo.Decode.field "string_repr_dot" Jsonoo.Decode.string res
  in
  let string_repr_d3 =
    Jsonoo.Decode.field "string_repr_d3" Jsonoo.Decode.string res
  in
  let nodes_pos = Jsonoo.Decode.field "nodes_pos" Jsonoo.Decode.(dict id) res
  in
  let nodes_pos = Hashtbl.to_seq nodes_pos |> List.of_seq in
  let name = Jsonoo.Decode.field "name" Jsonoo.Decode.string res in
  { name; nodes_pos; string_repr_dot; string_repr_d3 }

let callGetCFG ?render_options ~uri ~name client =
  let uri = VS.Uri.toString uri () in
  let data =
    let base = ["uri", Jsonoo.Encode.string uri;
                "name", Jsonoo.Encode.string name]
    in
    let full =
      match render_options,
            get_state_value ~key:(uri ^ ":" ^ name) with
      | Some options, _ -> ("render_options", options) :: base
      | _, Some options -> ("render_options", Jsonoo.t_of_js options) :: base
      | _ -> base
    in Jsonoo.Encode.object_ full
  in
  Vscode_languageclient.LanguageClient.sendRequest client ()
    ~meth:"superbol/getCFG" ~data |>
  Promise.then_
    ~rejected:begin fun _ ->
      VS.Window.showErrorMessage
        ~message:"Impossible to render graph, \
                  try closing and reopening the webview" ()
    end
    ~fulfilled:begin fun jsonoo ->
      Promise.return (Some (decode_graph jsonoo))
    end

(* WEBVIEW MANAGEMENT *)

type stored_data = {
  webview_panel: VS.WebviewPanel.t;
  graph: graph;
  render_options: Jsonoo.t;
}

let webview_panels = Hashtbl.create 1
let window_listener = ref None

let webviewpanel_disposal ~filename ~typ () =
  Hashtbl.remove webview_panels (filename, typ);
  if Hashtbl.length webview_panels == 0
  then begin
    (match !window_listener with
     | Some listener -> VS.Disposable.dispose listener
     | None -> ());
    window_listener := None;
  end;
  match VS.Window.activeTextEditor () with
  | None -> ()
  | Some text_editor ->
    let uri = VS.TextEditor.document text_editor |> VS.TextDocument.uri in
    if String.equal filename @@ VS.Uri.path uri
    then VS.TextEditor.setDecorations text_editor
        ~decorationType ~rangesOrOptions:(`Ranges [])

let create_or_get_webview ~graph ~uri ~typ =
  let render_options = Jsonoo.Encode.object_ [] in
  let filename = VS.Uri.path uri in
  match Hashtbl.find_opt webview_panels (filename, typ) with
  | Some { webview_panel; render_options; _ } ->
    VS.WebviewPanel.reveal webview_panel ();
    Hashtbl.replace webview_panels (filename, typ)
      { webview_panel; graph; render_options };
    VS.WebviewPanel.webview webview_panel, false
  | None ->
    let viewType = match typ with
      | Graphviz -> "superbol.cfg.dot"
      | D3_arc_diagram -> "superbol.cfg.arc"
    in
    let webview_panel = VS.Window.createWebviewPanel ~viewType
        ~title:"SuperBOL CFG Viewer" ~showOptions:(VS.ViewColumn.Beside)
    in
    let _ : VS.Disposable.t =
      VS.WebviewPanel.onDidDispose webview_panel ()
        ~listener:(webviewpanel_disposal ~filename ~typ)
        ~thisArgs:Ojs.null ~disposables:[]
    in
    let webview = VS.WebviewPanel.webview webview_panel in
    VS.WebView.set_options webview (VS.WebviewOptions.create ~enableScripts:true ());
    Hashtbl.add webview_panels (filename, typ)
      { webview_panel; graph; render_options };
    webview, true

let webview_data_find_opt ~uri ~typ =
  match Hashtbl.find_opt webview_panels (VS.Uri.path uri, typ) with
  | None ->
      None
  | Some { webview_panel; graph; render_options } ->
    Some (VS.WebviewPanel.webview webview_panel, graph, render_options)

let update_webview_data ~uri ~typ ?graph ?render_options () =
  let filename = VS.Uri.path uri in
  match Hashtbl.find_opt webview_panels (filename, typ) with
  | Some { webview_panel; render_options=current_ro; graph=current_g } ->
    let render_options = Option.value ~default:current_ro render_options in
    let graph = Option.value ~default:current_g graph in
    Hashtbl.replace webview_panels (filename, typ)
      { webview_panel; graph; render_options }
  | None -> ()

(* CLICK ON NODE *)

let on_click ~nodes_pos ~text_editor arg =
  let uri = VS.TextDocument.uri @@ VS.TextEditor.document text_editor in
  let column = VS.TextEditor.viewColumn text_editor in
  let node = Ojs.get_prop_ascii arg "node" |> Ojs.int_of_js |> string_of_int in
  try
    let jsonoo_range = List.assoc node nodes_pos in
    let range = VS.Range.t_of_js @@ Jsonoo.t_to_js jsonoo_range in
    let _ : unit Promise.t =
      VS.Window.showTextDocument ~document:(`Uri uri) ?column () |>
      Promise.then_ ~fulfilled:(fun text_editor ->
          let selection = VS.Selection.makePositions
              ~anchor:(VS.Range.start range) ~active:(VS.Range.start range)
          in
          VS.TextEditor.revealRange text_editor ~range
            ~revealType:VS.TextEditorRevealType.InCenterIfOutsideViewport ();
          VS.TextEditor.set_selection text_editor selection;
          VS.TextEditor.setDecorations text_editor ~decorationType
            ~rangesOrOptions:(`Ranges [range]);
          Promise.return ())
    in ()
  with Not_found -> ()

let setup_window_listener ~client =
  let listener event =
    if VS.TextEditorSelectionChangeEvent.kind event ==
       VS.TextEditorSelectionChangeKind.Command
    then ()
    else
      match VS.TextEditorSelectionChangeEvent.selections event with
      | [] -> ()
      | selection::_ ->
        let text_editor = VS.TextEditorSelectionChangeEvent.textEditor event in
        VS.TextEditor.setDecorations text_editor ~decorationType
          ~rangesOrOptions:(`Ranges []);
        let uri = VS.TextEditor.document text_editor |> VS.TextDocument.uri in
        let process_selection_change webview =
          let pos_start = VS.Selection.start selection in
          let data =
            let uri = Jsonoo.Encode.string @@ VS.Uri.toString uri () in
            Jsonoo.Encode.object_
              ["uri", uri;
               "line", Jsonoo.Encode.int @@ VS.Position.line pos_start;
               "character", Jsonoo.Encode.int @@ VS.Position.character pos_start]
          in
          let _ : bool Promise.t =
            Vscode_languageclient.LanguageClient.sendRequest client ()
              ~meth:"superbol/findProcedure" ~data |>
            Promise.then_
              ~rejected:begin fun _ -> Promise.return false end
              ~fulfilled:begin fun res ->
                let ojs = Ojs.empty_obj () in
                Ojs.set_prop_ascii ojs "type" (Ojs.string_to_js "focused_proc");
                Ojs.set_prop_ascii ojs "procedure" @@ Jsonoo.t_to_js res;
                VS.WebView.postMessage webview ojs
              end
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
    | Some listener ->
        listener
    | None -> VS.Window.onDidChangeTextEditorSelection () ()
                ~listener ~thisArgs:Ojs.null ~disposables:[]
  in window_listener := Some disposable_listener

(* MESSAGE MANAGER *)

let send_graph ?(as_new_graph=false) ~uri ~typ webview graph =
  let message_type = if as_new_graph
    then "new_graph_content"
    else "graph_content"
  in
  let ojs = Ojs.empty_obj () in
  (match get_state_value ~key:((VS.Uri.toString uri ()) ^ ":" ^ graph.name) with
   | None -> ()
   | Some options -> Ojs.set_prop_ascii ojs "render_options" options);
  Ojs.set_prop_ascii ojs "type" (Ojs.string_to_js message_type);
  if typ == Graphviz
  then Ojs.set_prop_ascii ojs "dot" (Ojs.string_to_js graph.string_repr_dot);
  Ojs.set_prop_ascii ojs "graph" (Ojs.string_to_js graph.string_repr_d3);
  Ojs.set_prop_ascii ojs "graph_name" (Ojs.string_to_js graph.name);
  let _ : bool Promise.t = VS.WebView.postMessage webview ojs
  in ()

let on_graph_update ~webview ~client ~uri ~typ name arg =
  let render_options_ojs = Ojs.get_prop_ascii arg "renderOptions" in
  let render_options = Jsonoo.t_of_js render_options_ojs in
  let _ : unit Promise.t = Promise.then_
      (callGetCFG ~uri ~name ~render_options client)
      ~fulfilled:begin function
        | None ->
          Promise.return ()
        | Some graph ->
          update_webview_data ~uri ~typ ~graph ~render_options ();
          update_state ~key:((VS.Uri.toString uri ()) ^ ":" ^ name) render_options_ojs;
          send_graph ~typ ~uri webview graph;
          Promise.return ()
      end
  in ()

let on_message ~client ~text_editor ~typ arg =
  let uri = VS.TextEditor.document text_editor |> VS.TextDocument.uri in
  let request_type = Ojs.get_prop_ascii arg "type" |> Ojs.string_of_js in
  match webview_data_find_opt ~uri ~typ with
  | None ->
    ()
  | Some (webview, graph, _) ->
    match request_type with
    | "click" ->
      on_click ~nodes_pos:graph.nodes_pos ~text_editor arg
    | "graph_update" ->
      on_graph_update ~client ~webview ~uri ~typ graph.name arg
    | "ready" ->
      send_graph ~as_new_graph:true ~typ ~uri webview graph
    | _ -> ()

(* USER REQUEST LOGIC *)

let open_cfg_for ~typ ~text_editor ~extension_uri client =
  let open Promise in
  let uri = VS.TextEditor.document text_editor |> VS.TextDocument.uri in
  let data =
    let uri = Jsonoo.Encode.string @@ VS.Uri.toString uri () in
    Jsonoo.Encode.object_ ["uri", uri]
  in
  match get_html_js_content ~extension_uri typ with
  | Error e ->
    let _ : _ option Promise.t = VS.Window.showErrorMessage
        ~message:("Unable to display control-flow: " ^ e) ()
    in return ()
  | Ok html_js ->
    Vscode_languageclient.LanguageClient.sendRequest client ()
      ~meth:"superbol/getPossibleCFG" ~data |>
    then_ ~fulfilled:begin fun jsonoo_graph_names ->
      let items = Jsonoo.Decode.(list string) jsonoo_graph_names in
      VS.Window.showQuickPick ~items () |>
      then_ ~fulfilled:begin function
        | None -> return ()
        | Some name ->
          then_ (callGetCFG ~uri ~name client) ~fulfilled:begin function
            | None -> return ()
            | Some graph ->
              let webview, is_new = create_or_get_webview ~graph ~typ ~uri in
              let html_content = setup_html_js_content ~webview ~typ html_js in
              let _ : VS.Disposable.t =
                VS.WebView.onDidReceiveMessage webview ()
                  ~listener:(on_message ~client ~text_editor ~typ)
                  ~thisArgs:Ojs.null ~disposables:[]
              in
              if is_new
              then VS.WebView.set_html webview html_content
              else send_graph ~as_new_graph:true ~typ ~uri webview graph;
              setup_window_listener ~client;
              return ()
          end
      end
    end

let open_cfg ?text_editor ~typ instance =
  let text_editor = match text_editor with
    | None -> VS.Window.activeTextEditor ()
    | e -> e
  in
  match Superbol_instance.client instance, text_editor with
  | Some client, Some text_editor ->
    let extension_uri = VS.ExtensionContext.extensionUri
      @@ Superbol_instance.context instance
    in state := Some (VS.ExtensionContext.workspaceState
                      @@ Superbol_instance.context instance);
    open_cfg_for ~typ ~extension_uri ~text_editor client
  | _ -> Promise.return ()
