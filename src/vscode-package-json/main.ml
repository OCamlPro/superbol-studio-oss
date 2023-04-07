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

let () =
  let read = ref false in
  Arg.parse [] (fun file ->
      read := true;
      match Vscode_json.Main.check_project file with
      | [], [] ->
        Printf.eprintf "Project %s checked OK\n%!" file
      | warnings, [] ->
        Printf.eprintf "Warnings found in project %s but OK\n%!" file;
        List.iter (fun s -> Printf.eprintf "   %s\n%!" s) warnings
      | warnings, errors ->
        Printf.eprintf "Errors found in project %s:\n%!" file;
        List.iter (fun s -> Printf.eprintf "   %s\n%!" s) errors;
        match warnings with
        | [] -> ()
        | _ ->
          Printf.eprintf "  Warnings also found\n%!";
          List.iter (fun s -> Printf.eprintf "      %s\n%!" s) warnings;
    ) "package-json [FILES]: parse files or generate file" ;

  if not !read then
    Vscode_json.Main.write_file "-" Vscode_json.Manifest.vscode_enc Project.manifest
