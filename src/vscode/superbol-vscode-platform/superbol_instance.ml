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

(** Credit @beiclause in https://github.com/beicause/call-graph/blob/master/src/html.ts *)
let html dot = Printf.sprintf {|
<!DOCTYPE html>
    <html lang="en">

    <head>
        <meta charset="UTF-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <title>Call Graph</title>
        <script src="https://d3js.org/d3.v5.min.js"></script>
        <script src="https://unpkg.com/@hpcc-js/wasm@0.3.11/dist/index.min.js"></script>
        <script src="https://unpkg.com/d3-graphviz@3.0.5/build/d3-graphviz.js"></script>
    </head>

    <body>
        <div id="app"></div>
    </body>
    <script>
        const vscode = acquireVsCodeApi()
        d3.select('#app').graphviz().renderDot(`%s`).zoom(false)
        .on('end', addListener);
        function addListener() {
          nodes = d3.selectAll('.node');
          nodes
          .on("click", function () {
            console.log("click");
            var title = d3.select(this).selectAll('title').text().trim();
            vscode.postMessage({
                command: 'click',
                node:title
            })
          })
        }
    </script>
    </html>
  |} dot

let _open_cfg ?text_editor instance =
  let open_cfg_for ?uri client =
    let uri, uri_ojs = match uri with
      | Some uri ->
        Jsonoo.Encode.string @@ Vscode.Uri.path uri,
        Vscode.Uri.t_to_js uri
      | None ->
        Jsonoo.Encode.string "", Ojs.null
    in
    Vscode_languageclient.LanguageClient.sendRequest client ()
      ~meth:"superbol/openCFG"
      ~data:uri |>
    Promise.(then_ ~fulfilled:(fun res ->
        (* TODO: do not reopen a different window for each call *)
        let dot_content = Jsonoo.Decode.string res in
        let newWebviewPanel =
          Vscode.Window.createWebviewPanel
            ~viewType:"cfg" ~title:"CFG webview"
            ~showOptions:(Vscode.ViewColumn.Two) in
        let webview = Vscode.WebviewPanel.webview newWebviewPanel in
        Vscode.WebView.set_html webview (html dot_content);
        Vscode.WebView.set_options webview (Vscode.WebviewOptions.create ~enableScripts:true ());
        let listener arg =
          let node = Ojs.get_prop_ascii arg "node" |> Ojs.string_of_js in
          let message = "Heard click on node: " ^ node in
          let _ = Vscode.Window.showErrorMessage () ~message in
          let pos = Vscode.Position.t_to_js @@ Vscode.Position.make ~line:12 ~character:20 in
          let args = Ojs.empty_obj () in
          Ojs.set_prop_ascii pos "position" args;
          let _ = Vscode.Commands.executeCommand
              ~command:"editor.action.goToLocations"
              (* ~args:[uri_ojs; pos] *)
              ~args:[uri_ojs; pos; Ojs.array_make 0]
          in
          ()
        in
        let _ = Vscode.WebView.onDidReceiveMessage webview ~listener ~thisArgs:Ojs.null ~disposables:[] () in
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

let html_d3 = Printf.sprintf {|
<!DOCTYPE html>
<html>
  <head>
  <meta charset="utf-8">
  <style>
  html, body {
    height: 100%%;
  }
  .link {
    fill: none;
    stroke: #666;
    stroke-width: 1.5px;
  }
  #licensing {
    fill: green;
  }
  .link.licensing {
    stroke: green;
  }
  .link.resolved {
    stroke-dasharray: 0,2 1;
  }
  circle {
    fill: #ccc;
    stroke: #333;
    stroke-width: 1.5px;
  }
  text {
    font: 10px sans-serif;
    pointer-events: none;
    text-shadow: 0 1px 0 #fff, 1px 0 0 #fff, 0 -1px 0 #fff, -1px 0 0 #fff;
  }
  svg {
      background-color: white;
  }
  </style>
  </head>
  <body></body>
  <script src="https://d3js.org/d3.v3.min.js"></script>
<script>

// http://blog.thomsonreuters.com/index.php/mobile-patent-suits-graphic-of-the-day/
var links = %s;

var nodes = {};

// Compute the distinct nodes from the links.
links.forEach(function(link) {
  link.source = nodes[link.source] || (nodes[link.source] = {name: link.source});
  link.target = nodes[link.target] || (nodes[link.target] = {name: link.target});
});

var width = window.innerWidth,
    height = window.innerHeight;

var force = d3.layout.force()
    .nodes(d3.values(nodes))
    .links(links)
    .size([width, height])
    .linkDistance(50)
    .charge(-300)
    .on("tick", tick)
    .start();


var svg = d3.select("body").append("svg")
    .attr("width", "")
    .attr("height", "");

// Per-type markers, as they don't inherit styles.
svg.append("defs").selectAll("marker")
    .data(["suit", "licensing", "resolved"])
    .enter().append("marker")
    .attr("id", function(d) { return d; })
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 15)
    .attr("refY", -1.5)
    .attr("markerWidth", 6)
    .attr("markerHeight", 6)
    .attr("orient", "auto")
    .append("path")
    .attr("d", "M0,-5L10,0L0,5");

var path = svg.append("g").selectAll("path")
    .data(force.links())
  .enter().append("path")
    .attr("class", function(d) { return "link " + d.type; })
    .attr("marker-end", function(d) { return "url(#" + d.type + ")"; });

var circle = svg.append("g").selectAll("circle")
    .data(force.nodes())
    .enter().append("circle")
    .attr("r", 6)
    .call(force.drag);

var text = svg.append("g").selectAll("text")
    .data(force.nodes())
  .enter().append("text")
    .attr("x", 8)
    .attr("y", ".31em")
    .text(function(d) { return d.name; });

addEventListener('resize', function() {
    force.size([window.innerWidth, window.innerHeight]);
    console.log('resized');
 });

// Use elliptical arc path segments to doubly-encode directionality.
function tick() {
  path.attr("d", linkArc);
  circle.attr("transform", transform);
  text.attr("transform", transform);
}

function linkArc(d) {
  var dx = d.target.x - d.source.x,
      dy = d.target.y - d.source.y,
      dr = Math.sqrt(dx * dx + dy * dy).toFixed(3);
  return "M" + d.source.x.toFixed(3) + "," + d.source.y.toFixed(3)
       + "A" + dr + "," + dr + " 0 0,1 " + d.target.x.toFixed(3) + "," + d.target.y.toFixed(3);
}

function transform(d) {
  return "translate(" + d.x.toFixed(1) + "," + d.y.toFixed(1) + ")";
}

</script>
</html>
  |}

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

let open_cfg_for ?(d3=false) ~text_editor client =
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
      let html = (if d3 then html_d3 else html) graph_content in
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
    open_cfg_for ~d3 ~text_editor client
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



