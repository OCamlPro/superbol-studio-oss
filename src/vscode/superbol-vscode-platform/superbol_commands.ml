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
      let _ : unit Promise.t = Superbol_cfg_explorer.open_cfg ~typ:`Dot _instance in
      ()
    end

let _open_cfg_arc =
  command "superbol.cfg.open.arc" @@ Instance
    begin fun _instance ~args:_ ->
      let _ : unit Promise.t = Superbol_cfg_explorer.open_cfg ~typ:`Arc _instance in
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
