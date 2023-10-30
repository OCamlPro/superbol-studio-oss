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
  { id : string ;
    handler : Superbol_instance.t -> args:Ojs.t list -> unit
    (* Intended for partial application of [handler instance] *)
}

let commands = ref []

let command id handler =
  let command = { id; handler } in
  commands := command :: !commands;
  command

let _restart_language_server =
  command "superbol.server.restart" @@
  fun instance ~args:_ ->
  let (_ : unit Promise.t) =
    Superbol_instance.start_language_server instance
  in ()

let register extension instance { id; handler } =
  let callback = handler instance in
  ExtensionContext.subscribe extension
    ~disposable:(Commands.registerCommand ~command:id ~callback)

let register_all extension instance =
  List.iter (register extension instance) !commands
