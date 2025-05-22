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

let start_lsp_server_autorestarter instance =
  Superbol_instance.subscribe_disposable instance @@
  Vscode.Workspace.onDidChangeConfiguration ()
    ~listener:begin fun config_change ->
      if Superbol_languageclient.server_needs_restart_after ~config_change
      then begin
        Superbol_instance.subscribe_disposable instance @@
        Vscode.Window.setStatusBarMessage ()
          ~text:"Restarting SuperBOL Language Server…"
          ~hide:(`AfterTimeout 2000);
        let _ : unit Promise.t =
          Superbol_instance.start_language_server instance
        in ()
      end;
    end

(* --- *)

let current_instance = ref None

let activate (extension: Vscode.ExtensionContext.t) =
  let instance = Superbol_instance.make ~context:extension in
  current_instance := Some instance;

  Superbol_instance.subscribe_disposable instance @@
  Vscode.Tasks.registerTaskProvider
    ~type_:Superbol_tasks.type_
    ~provider:(Superbol_tasks.provider instance);
  start_lsp_server_autorestarter instance;

  Superbol_commands.register_all extension instance;
  Superbol_instance.start_language_server instance

let deactivate () =
  match !current_instance with
  | Some instance ->
      Superbol_instance.stop_language_server instance
  | None ->
      Promise.return ()

(* see {{:https://code.visualstudio.com/api/references/vscode-api#Extension}
   activate() *)
let () =
  Js_of_ocaml.Js.(export "activate" (wrap_callback activate));
  Js_of_ocaml.Js.(export "deactivate" (wrap_callback deactivate))
