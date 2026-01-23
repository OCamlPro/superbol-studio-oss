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

open Promise.Syntax

module Types = struct
  type superbol_instance = Superbol_instance.t
  include Superbol_types
end

(* --- *)

let current_instance = ref None

let check_first_run (context : Vscode.ExtensionContext.t) =
  let global_state = Vscode.ExtensionContext.globalState context in
  let key = "superbol.hasShownWelcome" in
  match Vscode.Memento.get global_state ~key with
  | Some _ -> ()
  | None ->
    let _ =
      let* () = Vscode.Memento.update global_state ~key
          ~value:(Ojs.bool_to_js true) in
      let options = Vscode.MessageOptions.create ~modal:true () in
      let* _ = Vscode.Window.showInformationMessage
          ~message:"Welcome to SuperBOL! Enter a license key to unlock Pro features."
          ~options
          ~choices:[
            ("Enter License Key", `EnterKey);
            ("Continue", `Continue);
          ] () in
      Promise.return ()
    in
    ()

let activate ~lsp_server_prefix (extension: Vscode.ExtensionContext.t) =
  let instance = Superbol_instance.make ~lsp_server_prefix ~context:extension in
  current_instance := Some instance;

  check_first_run extension;

  Superbol_instance.subscribe_disposable instance @@
  Vscode.Tasks.registerTaskProvider
    ~type_:Superbol_tasks.type_
    ~provider:(Superbol_tasks.provider instance);
  Superbol_instance.start_autorestarter instance;

  Superbol_commands.register_all extension instance;
  let* () = Superbol_instance.start_language_server instance in
  Promise.return instance

let deactivate () =
  match !current_instance with
  | Some instance ->
      Superbol_instance.stop_language_server instance
  | None ->
      Promise.return ()

module Printer = Superbol_printer
module Instance = Superbol_instance
module Workspace = Superbol_workspace
