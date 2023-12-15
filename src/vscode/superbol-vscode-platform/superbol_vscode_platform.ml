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

(* open Js_of_ocaml *)

let indentRange
    ~document
    ~options:_
    ~token:_
    ~range
  =
  let range =
    match range with
    | Some range -> range
    | None ->
      let endLine = Vscode.TextDocument.lineCount document - 1 in
      let endCharacter =
        String.length @@ Vscode.TextLine.text @@ Vscode.TextDocument.lineAt document ~line:endLine
      in
      (* selects entire document range *)
      Vscode.Range.makeCoordinates ~startLine:0 ~startCharacter:0 ~endLine ~endCharacter
  in
  let input_text = Vscode.TextDocument.getText document ~range () in

  let output_text = "=---=" ^ input_text in
  let promise = Some [ Vscode.TextEdit.replace ~range ~newText:output_text ] in
  (*
   match output with
    | Ok newText -> Some [ TextEdit.replace ~range ~newText ]
    | Error msg ->
      show_message `Error "Dune formatting failed: %s" msg;
      Some []
*)
  `Value promise

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

let current_instance = ref None

let activate (extension : Vscode.ExtensionContext.t) =
  let providerFull = Vscode.DocumentFormattingEditProvider.create
      ~provideDocumentFormattingEdits:(indentRange ~range:None)
  in
  let disposable = Vscode.Languages.registerDocumentFormattingEditProvider
      ~selector: ( `Filter (Vscode.DocumentFilter.create ~scheme:"file" ~language:"cobol" ()))
      ~provider:providerFull
  in
  Vscode.ExtensionContext.subscribe extension ~disposable;

  let debug =
    Vscode.Debug.registerDebugConfigurationProvider ()
      ~debugType:"cobol"
      ~provider:Debugger.debugConfigurationProvider
  in

  Vscode.ExtensionContext.subscribe extension ~disposable:debug;

  let task =
    Vscode.Tasks.registerTaskProvider
      ~type_:Superbol_tasks.type_
      ~provider:Superbol_tasks.provider
  in

  Vscode.ExtensionContext.subscribe extension ~disposable:task;

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

  Superbol_commands.register_all extension instance;
  Superbol_instance.start_language_server instance

let deactivate () =
  match !current_instance with
  | Some instance ->
    Superbol_instance.stop_language_server instance
  | None -> Promise.return ()

(* see {{:https://code.visualstudio.com/api/references/vscode-api#Extension}
   activate() *)
let () =
  Js_of_ocaml.Js.(export "activate" (wrap_callback activate));
  Js_of_ocaml.Js.(export "deactivate" (wrap_callback deactivate))
