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

let read = ref false

let print_result f file =
  read := true;
  match f file with
  | exception exn ->
    Printf.eprintf "File %s: exception %s\n%!" file
      ( Printexc.to_string exn)
  | [], [] ->
    Printf.eprintf "File %s checked OK\n%!" file
  | warnings, [] ->
    Printf.eprintf "Warnings found in file %s but OK\n%!" file;
    List.iter (fun s -> Printf.eprintf "   %s\n%!" s) warnings
  | warnings, errors ->
    Printf.eprintf "Errors found in file %s:\n%!" file;
    List.iter (fun s -> Printf.eprintf "   %s\n%!" s) errors;
    match warnings with
    | [] -> ()
    | _ ->
      Printf.eprintf "  Warnings also found\n%!";
      List.iter (fun s -> Printf.eprintf "      %s\n%!" s) warnings

type kind =
  | MANIFEST
  | TASKS
  | SNIPPET
  | GRAMMAR
  | LANGUAGE

let () =
  let verbose = ref false in
  let kind = ref MANIFEST in
  let parse file =
    print_result
      (match !kind with
       | TASKS ->
         Vscode_json.Main.check_file
           Vscode_json.Tasks.encoding
           Vscode_json.Tasks.pp
       | SNIPPET ->
         Vscode_json.Main.check_file
           Vscode_json.Snippets.snippets_enc
           Vscode_json.Snippets.pp_snippets
       | GRAMMAR ->
           Vscode_json.Main.check_file
            Vscode_json.Grammar.grammar_enc
            Vscode_json.Grammar.pp_grammar
       | LANGUAGE ->
           Vscode_json.Main.check_file
            Vscode_json.Language.language_enc
            Vscode_json.Language.pp_language
       | MANIFEST ->
         Vscode_json.Main.check_project ~verbose:!verbose
      ) file
  in
  Arg.parse [
    "-v", Arg.Set verbose, "Set verbose mode";
    "--tasks",
    Arg.String (fun file ->
        kind := TASKS;
        parse file
      ),
    "FILE Parse file FILE as a .vscode/tasks.json file";

    "--snippet",
    Arg.String (fun file ->
        kind := SNIPPET ;
        parse file),
    "FILE Parse file FILE as a snippets/*.json file";

    "--grammar",
    Arg.String (fun file ->
        kind := GRAMMAR ;
        parse file
      ),
    "FILE Parse file FILE as a syntaxes/*.json file";

    "--language",
    Arg.String (fun file ->
        kind := LANGUAGE ;
        parse file
      ),
    "FILE Parse file FILE as a language *.json file";

    "--manifest",
    Arg.String (fun file ->
        kind := MANIFEST ;
        parse file
      ),
    "FILE Parse file FILE as a package.json file";

  ]
    parse
    "package-json [FILES]: parse files or generate file" ;

  if not !read then
    Vscode_json.Main.write_file "-" Vscode_json.Manifest.vscode_enc Project.manifest
