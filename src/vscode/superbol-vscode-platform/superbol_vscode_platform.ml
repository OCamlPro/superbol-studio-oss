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

(*
import * as vscode from 'vscode';

function globalFormatSelection(): boolean {
    const config = vscode.workspace.getConfiguration().get('ocpIndent.globalFormatTakesSelection');
    if (config) { return true; }
    else { return false; }
}
function ocpIndentPath(): string {
    const config = vscode.workspace.getConfiguration().get('ocpIndent.path');
    if (config) { return String(config); }
    else { return ""; }
}

function ocpCommand() {
    const path = ocpIndentPath();
    if (path === "") {
        return 'ocp-indent';
    } else {
        return path;
    }
}

function getCwd(document: vscode.TextDocument): string | undefined {
    const uri = document.uri;
    // Check if the document has a file path
    if (uri.scheme === 'file') {
        const path = require("path");
        const filePath = uri.fsPath;
        return path.dirname(filePath);
    } else {
        // Otherwise, we -arbitrarily- use the first workspace root
        const workspaceFolders = vscode.workspace.workspaceFolders;
        if (workspaceFolders && workspaceFolders.length > 0) {
            return workspaceFolders[0].uri.fsPath;
        } else {
            // In the worst case, we give up.
            return undefined;
        }
    }
}

function executeOcpIndent(document: vscode.TextDocument, args: string[]): string | undefined {
    const cmd = ocpCommand();
    const text = document.getText();
    const cwd = getCwd(document);
    const options = {
        input: text,
        encoding: 'utf8',
        cwd: cwd
    };
    const cp = require('child_process');
    const { stdout, stderr, error } = cp.spawnSync(cmd, args, options);
    if (stderr) { vscode.window.showErrorMessage(stderr); }
    if (error) { vscode.window.showErrorMessage(error.message); }
    const output = error || stderr ? undefined : stdout;
    return output;
}

function makeOptionRange({ start, end }: vscode.Range): string[] {
    return ['-l', (start.line + 1) + '-' + (end.line + 1)];
}

function doIndentZone(document: vscode.TextDocument, range: any): string | undefined {
    let optionLines: string[] = [];
    if (range) {
        optionLines = makeOptionRange(range);
    }
    else if (globalFormatSelection()) {
        let editor = vscode.window.activeTextEditor;
        if (editor && editor.selection.start.isBefore(editor.selection.end)) {
            optionLines = makeOptionRange(editor.selection);
        }
    }
    return executeOcpIndent(document, optionLines);
}

function isOcaml(languageId: string) {
    return languageId === 'ocaml' || languageId === 'ocaml.interface';
}

function indentRange(document: vscode.TextDocument, range?: vscode.Range):
    vscode.ProviderResult<vscode.TextEdit[]> {
    if (isOcaml(document.languageId)) {
        const output = doIndentZone(document, range);
        if (output) {
            var firstLine = document.lineAt(0);
            var lastLine = document.lineAt(document.lineCount - 1);
            var fullRange = new vscode.Range(firstLine.range.start, lastLine.range.end);
            return [vscode.TextEdit.replace(fullRange, output)];
        }
    }
    return [];
}

export function activate(context: vscode.ExtensionContext) {
    let providerFull = {
        provideDocumentFormattingEdits(document: vscode.TextDocument) {
            return indentRange(document, undefined);
        }
    };

    vscode.languages.registerDocumentFormattingEditProvider('ocaml', providerFull);
    vscode.languages.registerDocumentFormattingEditProvider('ocaml.interface', providerFull);

    let providerPartial = {
        provideDocumentRangeFormattingEdits(document: vscode.TextDocument, range: vscode.Range) {
            return indentRange(document, range);
        }
    };

    vscode.languages.registerDocumentRangeFormattingEditProvider('ocaml', providerPartial);
    vscode.languages.registerDocumentRangeFormattingEditProvider('ocaml.interface', providerPartial);
}
*)

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

let instance = Superbol_instance.make ()

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

  Superbol_instance.set_bundled_superbol instance
    bundled_superbol;

  Superbol_commands.register_all extension instance;
  Superbol_instance.start_language_server instance

let deactivate () =
    Superbol_instance.stop_language_server instance

(* see {{:https://code.visualstudio.com/api/references/vscode-api#Extension}
   activate() *)
let () =
  Js_of_ocaml.Js.(export "activate" (wrap_callback activate));
  Js_of_ocaml.Js.(export "deactivate" (wrap_callback deactivate))
