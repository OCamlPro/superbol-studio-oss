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

(* Helpers to find the bundled superbol executable *)
let rec find_existing = function
  | [] -> raise Not_found
  | uri :: uris ->
    if Node.Fs.existsSync (Vscode.Uri.fsPath uri) then uri
    else find_existing uris

(* Look for the most specific `superbol-free` executable amongst (in order):

  - `superbol-free-${platform}-${arch}${suffix}`
  - `superbol-free-${platform}${suffix}`
  - `superbol-free${suffix}`

  The `platform` and `arch` used are from the corresponding `process`
  attributes in node.js. The `suffix` is `".exe"` on Windows, and empty
  otherwise.

  https://nodejs.org/api/process.html#processplatform
  https://nodejs.org/api/process.html#processarch
*)
let find_superbol root =
  let open Node.Process in
  let prefix = "superbol-free" in
  let suffix = if platform == "win32" then ".exe" else "" in
  Vscode.Uri.fsPath @@ find_existing @@ List.map (fun name ->
    Vscode.Uri.joinPath root ~pathSegments:[name]) @@ [
    Format.asprintf "%s-%s-%s%s" prefix platform arch suffix;
    Format.asprintf "%s-%s%s" prefix platform suffix;
    Format.asprintf "%s%s" prefix suffix
  ]

(* --- *)

let lsp_server_autorestarter instance : Vscode.Disposable.t =
  Vscode.Workspace.onDidChangeConfiguration ()
    ~listener:begin fun config_change ->
      let affects ?scope section : bool =
        Vscode.ConfigurationChangeEvent.affectsConfiguration config_change ()
          ~section ?scope
      in
      if affects "superbol.lsp-path"  (* machine setting: no need for a scope *)
      then
        let _: unit Promise.t =
          Superbol_instance.start_language_server instance
        in ()
      else ()
    end

(* --- *)

let current_instance = ref None

let activate (extension: Vscode.ExtensionContext.t) =
  let bundled_superbol =
    try
      find_superbol
          (Vscode.Uri.joinPath ~pathSegments:["_dist"]
            (Vscode.ExtensionContext.extensionUri extension));
    with Not_found ->
      (* If there is no bundled executable for the current platform, fall back
         to looking for superbol-free in the PATH *)
      "superbol-free"
  in

  let instance = Superbol_instance.make ~bundled_superbol () in
  current_instance := Some instance;

  let task =
    Vscode.Tasks.registerTaskProvider
      ~type_:Superbol_tasks.type_
      ~provider:(Superbol_tasks.provider instance)
  in
  Vscode.ExtensionContext.subscribe extension ~disposable:task;

  Vscode.ExtensionContext.subscribe extension
    ~disposable:(lsp_server_autorestarter instance);

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
