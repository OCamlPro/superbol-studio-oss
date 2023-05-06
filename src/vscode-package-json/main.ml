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

let read = ref false

let print_result f file =
  read := true;
  match f file with
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

let () =
  Arg.parse [
    "--tasks",
    Arg.String
      (print_result (Vscode_json.Main.check_file Vscode_json.Tasks.encoding)),
    "FILE Parse file FILE as a tasks.json file";

  ]
    (print_result Vscode_json.Main.check_project)
    "package-json [FILES]: parse files or generate file" ;

  if not !read then
    Vscode_json.Main.write_file "-" Vscode_json.Manifest.vscode_enc Project.manifest
