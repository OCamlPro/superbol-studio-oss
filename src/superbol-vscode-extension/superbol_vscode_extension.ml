(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

let indentRange
    ~document
    ~options:_
    ~token:_
  =
  
  let start_line, end_line = 
    let editor = Vscode.Window.activeTextEditor () in
    let selection = Option.get editor |> Vscode.TextEditor.selection in 
    let anchor = Vscode.Selection.anchor selection in 
    let start_line = Vscode.Position.line anchor in 
    let active = Vscode.Selection.active selection in     
    let end_line = Vscode.Position.line active in 
    if start_line > end_line then end_line, start_line 
    else
        start_line, end_line
  in
   
  let range = 
    let endCharacter =
        String.length @@ Vscode.TextLine.text @@ Vscode.TextDocument.lineAt document ~line:end_line
    in
    Vscode.Range.makeCoordinates ~startLine:start_line ~startCharacter:0
        ~endLine:end_line ~endCharacter:endCharacter
  in 

  let run () = 
    let command = "/home/weituo/git/superbol/padbol/superbol" in 
    let filename = Vscode.TextDocument.fileName document in 
    let args = [|"x-indent range";"--free";
      filename;
      Int.to_string (start_line + 1);
      Int.to_string (end_line + 1)|] 
    in 
    let childprocess = 
      Node.ChildProcess.spawn command args
    in 

    let open Promise.Syntax in 
    let open Node in 
    let+ (result:ChildProcess.return) = 
        childprocess in 
        if result.exitCode = 0 then 
          Ok result.stdout 
        else 
          Error (Printf.sprintf "Command failed with error: \n%s" result.stderr)
  in 

  let output = run () in

  let promise =
    let open Promise.Syntax in
    let+ output in
     match output with 
     | Ok output ->
       Some [ Vscode.TextEdit.replace ~range ~newText:output ]
     | Error msg -> failwith msg 
  in 

  `Promise promise

(* use Cobol_identation as library
in dune(of whole folder), add padbol into dirs
in src/superbol-vscode-extension/package.toml, add Cobol_indentation
*)
(* let indentRange
    ~document
    ~options:_
    ~token:_
  =
  
    let editor = Vscode.Window.activeTextEditor () in
    let selection = Option.get editor |> Vscode.TextEditor.selection in 
    let anchor = Vscode.Selection.anchor selection in 
    let start_line = Vscode.Position.line anchor in 
    let active = Vscode.Selection.active selection in     

    let end_line = Vscode.Position.line active in 
    let start_line, end_line = 
        if start_line > end_line then end_line, start_line 
        else
            start_line, end_line
    in 
    let start_pos = Vscode.Position.make ~line:start_line ~character:0 in 
    let endCharacter =
        String.length @@ Vscode.TextLine.text @@ Vscode.TextDocument.lineAt document ~line:end_line
    in
    let end_pos = Vscode.Position.make ~line:end_line ~character:endCharacter in 
    let range =  Vscode.Range.makePositions ~start:start_pos ~end_:end_pos in
    
    let (myrange:Cobol_indentation.Indenter.range option) = 
        let start_line = start_line + 1 in 
        let end_line = end_line + 1 in 
        Some {start_line;end_line} 
    in 

    let filename = Vscode.TextDocument.fileName document in 

    let output_text = Cobol_indentation.Indenter.indent_file_free ~file:filename ~range:myrange in

    let promise = Some [ Vscode.TextEdit.replace ~range ~newText:output_text ] in
    `Value promise *)

let activate (extension: Vscode.ExtensionContext.t) =

    let providerFull = 
        Vscode.DocumentFormattingEditProvider.create 
        ~provideDocumentFormattingEdits: (indentRange)
    in 
    
    let disposable = Vscode.Languages.registerDocumentFormattingEditProvider
        ~selector: ( `Filter (Vscode.DocumentFilter.create ~scheme:"file" ~language:"cobol" ()))
        ~provider:providerFull
    in
    
    Vscode.ExtensionContext.subscribe extension ~disposable
    
let deactivate () = ()    
   
let () =
  Js_of_ocaml.Js.(export "activate" (wrap_callback activate));
  Js_of_ocaml.Js.(export "deactivate" (wrap_callback deactivate))