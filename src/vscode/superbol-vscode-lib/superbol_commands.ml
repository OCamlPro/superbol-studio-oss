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

type handler =
  | Instance of     (* Intended for partial application of [handler instance] *)
      (Superbol_instance.t -> args:Ojs.t list -> unit)
  (* | Text of *)
  (*     (Superbol_instance.t -> textEditor:TextEditor.t -> *)
  (*      edit:TextEditorEdit.t -> args:Ojs.t list -> unit) *)

type t =
  {
    id: string;
    handler: handler;
  }

let extension_oc : Vscode.OutputChannel.t Lazy.t =
  lazy (Vscode.Window.createOutputChannel ~name:"SuperBOL Studio Extension")

let commands = ref []

let command id handler =
  let command = { id; handler } in
  commands := command :: !commands;
  command

let _open_cfg =
  command "superbol.cfg.open" @@ Instance
    begin fun _instance ~args:_ ->
      let _ : unit Promise.t = Superbol_cfg_explorer.open_cfg
          ~typ:Graphviz _instance in
      ()
    end

let _open_cfg_arc =
  command "superbol.cfg.open.arc" @@ Instance
    begin fun _instance ~args:_ ->
      let _ : unit Promise.t = Superbol_cfg_explorer.open_cfg
          ~typ:D3_arc_diagram _instance in
      ()
    end

let _editor_action_findReferences =
  let command_name = "superbol.editor.action.findReferences"  in
  command command_name @@ Instance
    begin fun _instance ~args ->
      match args with
      | [arg1; arg2] ->
        let uri = Uri.t_to_js @@ Uri.parse (Ojs.string_of_js arg1) () in
        let pos =
          let line = Ojs.get_prop_ascii arg2 "line" |> Ojs.int_of_js in
          let character = Ojs.get_prop_ascii arg2 "character" |> Ojs.int_of_js in
          Position.t_to_js @@ Position.make ~line ~character in
        let _ = Commands.executeCommand
            ~command:"editor.action.findReferences"
            ~args:[uri; pos]
        in ()
      | _ ->
        let types_given = List.map Ojs.type_of args |> String.concat ", " in
        let lazy oc = extension_oc in
        let value = Printf.sprintf
            "Internal warning: unexpected arguments given to %s: \
             expected uri & position, got [%s]" command_name types_given in
        OutputChannel.appendLine oc ~value
    end

(** {2 Workspace-wide analysis} *)

(* An open document gets diagnostics even with no editor showing it, and they
   stay in the Problems view once it is closed. *)

(* Same extensions as the `cobol' language contribution. *)
let cobol_file_patterns =
  [
    "**/*.[cC]{ob,OB,bl,BL,py,PY,bx,BX,bsql}";
    "**/*.[pP]{co,CO}";
  ]

let find_cobol_files ~token =
  let open Promise.Syntax in
  let rec aux acc = function
    | [] ->
        Promise.return (List.rev acc)
    | pattern :: patterns ->
        let* uris = Workspace.findFiles () ~includes:(`String pattern) ~token in
        aux (List.rev_append uris acc) patterns
  in
  aux [] cobol_file_patterns

(* The reply only comes once the server has handled the `didOpen' for [uri], so
   waiting for it paces the loop on real work. *)
let await_analysis_of ~uri instance =
  Superbol_instance.lsp_request instance
    ~meth:"textDocument/documentSymbol"
    ~data:Jsonoo.Encode.(object_ [
        "textDocument", object_ ["uri", string @@ Uri.toString uri ()];
      ]) |>
  Promise.then_
    ~fulfilled:(fun _ -> Promise.return ())
    ~rejected:(fun _ -> Promise.return ())

let analyze_document ~uri instance =
  Promise.catch ~rejected:(fun _ -> Promise.return ()) @@
  let open Promise.Syntax in
  let* _doc = Workspace.openTextDocument (`Uri uri) in
  await_analysis_of ~uri instance

let report_completion ~analyzed ~missed =
  let _ =
    Window.showInformationMessage ()
      ~message:begin
        if missed = 0 then
          Printf.sprintf "SuperBOL: analyzed %u file(s); diagnostics are \
                          listed in the Problems view" analyzed
        else
          Printf.sprintf "SuperBOL: analysis interrupted after %u of %u file(s)"
            analyzed (analyzed + missed)
      end
  in
  Promise.return ()

let analyze_workspace instance ~progress ~token =
  let open Promise.Syntax in
  let* uris = find_cobol_files ~token in
  let total = List.length uris in
  let percent i = i * 100 / max 1 total in
  let rec loop i = function
    | remaining when CancellationToken.isCancellationRequested token ->
        report_completion ~analyzed:i ~missed:(List.length remaining)
    | [] ->
        report_completion ~analyzed:i ~missed:0
    | uri :: remaining ->
        Progress.report progress ~value:Progress.{
            message = Some (Printf.sprintf "%u/%u: %s" (succ i) total @@
                            Workspace.asRelativePath () ~pathOrUri:(`Uri uri));
            increment = Some (percent (succ i) - percent i);
          };
        let* () = analyze_document ~uri instance in
        loop (succ i) remaining
  in
  loop 0 uris

(* The server drops all diagnostics unless `forceSyntaxDiagnostics' is set or
   the dialect is COBOL85 (see `dispatch_diagnostics' in `lsp_server.ml').  The
   dialect is per project, so we warn instead of refusing. *)
let check_diagnostics_reported () =
  if Superbol_workspace.bool "forceSyntaxDiagnostics" ||
     Superbol_workspace.string "cobol.dialect" = "cobol85" then
    Promise.return `Scan
  else
    let open Promise.Syntax in
    let+ choice =
      Window.showWarningMessage ()
        ~message:"SuperBOL only reports diagnostics for projects that use the \
                  COBOL85 dialect, unless `superbol.forceSyntaxDiagnostics' is \
                  enabled.  The scan may find nothing to report."
        ~choices:["Enable and Restart Server", `Enable;
                  "Scan Anyway", `Scan]
    in
    Option.value choice ~default:`Abort

(* Writing the setting restarts the server.  We cannot await that, so we ask
   for a new run. *)
let enable_syntax_diagnostics () =
  let open Promise.Syntax in
  let target =
    if Workspace.workspaceFolders () = []
    then ConfigurationTarget.Global
    else ConfigurationTarget.Workspace
  in
  let+ () =
    WorkspaceConfiguration.update
      (Workspace.getConfiguration ~section:"superbol" ())
      ~section:"forceSyntaxDiagnostics"
      ~value:(Ojs.bool_to_js true)
      ~configurationTarget:(`ConfigurationTarget target) ()
  in
  let _ =
    Window.showInformationMessage ()
      ~message:"Diagnostics enabled.  The language server is restarting; \
                please run the analysis again."
  in
  ()

let scan_workspace instance =
  Window.withProgress (module Interop.Js.Unit)
    ~options:(ProgressOptions.create
                ~location:(`ProgressLocation ProgressLocation.Notification)
                ~title:"SuperBOL: analyzing COBOL files"
                ~cancellable:true ())
    ~task:(analyze_workspace instance)

let run_analysis instance =
  match Superbol_instance.client instance with
  | None ->
      Superbol_printer.show_error_message @@
      Error Superbol_types.Client_not_running
  | Some _ ->
      let open Promise.Syntax in
      let* decision = check_diagnostics_reported () in
      match decision with
      | `Abort -> Promise.return ()
      | `Enable -> enable_syntax_diagnostics ()
      | `Scan -> scan_workspace instance

let _analyze_workspace =
  command "superbol.analyze.workspace" @@ Instance
    begin fun instance ~args:_ ->
      let _: unit Promise.t = run_analysis instance in
      ()
    end

let _restart_language_server =
  command "superbol.server.restart" @@ Instance
    begin fun instance ~args:_ ->
      let _: unit Promise.t =
        Superbol_instance.start_language_server instance
      in
      ()
    end

let _write_project_config =
  command "superbol.write.project.config" @@ Instance
    begin fun instance ~args:_ ->
      let _: unit Promise.t =
        Superbol_instance.write_project_config instance
      in ()
    end

let _show_coverage =
  command "superbol.coverage.show" @@ Instance
    (fun _instance ~args:_ ->
      let _ =
        Commands.executeCommand ~command:"gcov-viewer.show" ~args:[]
      in
      ())

let _hide_coverage =
  command "superbol.coverage.hide" @@ Instance
    (fun _instance ~args:_ ->
      let _ =
        Commands.executeCommand ~command:"gcov-viewer.hide" ~args:[]
      in
      ())

let _reload_coverage =
  command "superbol.coverage.reload" @@ Instance
    (fun _ ~args:_ ->
      let _ =
        Commands.executeCommand ~command:"gcov-viewer.reloadGcdaFiles" ~args:[]
      in
      ())

let register extension instance { id; handler } =
  match handler with
  | Instance callback ->
      let callback = callback instance in
      ExtensionContext.subscribe extension
        ~disposable:(Commands.registerCommand ~command:id ~callback)
  (* | Text callback -> *)
  (*     let callback = callback instance in *)
  (*     ExtensionContext.subscribe extension *)
  (*       ~disposable:(Commands.registerTextEditorCommand ~command:id ~callback) *)

let register_all extension instance =
  List.iter (register extension instance) !commands
