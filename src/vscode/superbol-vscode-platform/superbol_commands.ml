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

type t =
  {
    id: string;
    handler:
      [ `Basic of
          (args:Ojs.t list -> unit)
      | `Instance of (* Intended for partial application of [handler
                        instance] *)
          (Superbol_instance.t -> args:Ojs.t list -> unit)
      | `Text of
          (Superbol_instance.t -> textEditor:TextEditor.t ->
           edit:TextEditorEdit.t -> args:Ojs.t list -> unit) ]
  }

let commands = ref []

let command id handler =
  let command = { id; handler } in
  commands := command :: !commands;
  command

let _restart_language_server =
  command "superbol.server.restart" @@ `Instance
    begin fun instance ~args:_ ->
      let (_ : unit Promise.t) =
        Superbol_instance.start_language_server instance
      in ()
    end

let _write_project_config =
  command "superbol.write.project.config" @@ `Text
    begin fun instance ~textEditor ~edit:_ ~args:_ ->
      match Superbol_instance.client instance with
      | None ->                                      (* ignore; TODO; message? *)
          ()
      | Some client ->
          let document = TextEditor.document textEditor in
          let uri = TextDocument.uri document in
          let (_ : _ Promise.t) =
            Vscode_languageclient.LanguageClient.sendRequest client ()
              ~meth:"superbol/writeProjectConfiguration"
              ~data:(Jsonoo.Encode.(object_ [
                  "uri", string @@ Uri.toString uri ();
                ]))
          in ()
    end

let register extension instance { id; handler } =
  match handler with
  | `Basic callback ->
      ExtensionContext.subscribe extension
        ~disposable:(Commands.registerCommand ~command:id ~callback)
  | `Instance callback ->
      let callback = callback instance in
      ExtensionContext.subscribe extension
        ~disposable:(Commands.registerCommand ~command:id ~callback)
  | `Text callback ->
      let callback = callback instance in
      ExtensionContext.subscribe extension
        ~disposable:(Commands.registerTextEditorCommand ~command:id ~callback)

let register_all extension instance =
  List.iter (register extension instance) !commands
