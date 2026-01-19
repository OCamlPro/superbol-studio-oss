(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.V2
open EZCMD.TYPES

let print_result f file =
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

let make_cmd ~extension_manifest =
  let generate = ref None in
  let kind = ref MANIFEST in
  let files = ref [] in
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
         Vscode_json.Main.check_project ~verbose:(!Globals.verbosity>1)
      ) file
  in
  EZCMD.sub
    "json vscode"
    (fun () ->
       match !generate, !files with
       | None, [] ->
         Printf.eprintf "Use either --gen TARGET, or provide files to read\n%!";
         exit 2
       | None, files ->
         List.iter parse files
       | Some file, [] ->
         Vscode_json.Main.write_file file
           Vscode_json.Manifest.vscode_enc extension_manifest
       | Some _, _ ->
         Printf.eprintf
           "Actions --gen TARGET and parse files are exclusive\n%!";
         exit 2
    )
    ~args: ([
        [ "tasks" ], Arg.Unit (fun () -> kind := TASKS),
        EZCMD.info "Parse files as .vscode/tasks.json files";

        [ "snippets" ], Arg.Unit (fun () -> kind := SNIPPET),
        EZCMD.info "Parse files as snippets/*.json files";

        [ "grammar" ], Arg.Unit (fun () -> kind := GRAMMAR),
        EZCMD.info "Parse files as syntaxes/*.json files";

        [ "language" ], Arg.Unit (fun () -> kind := LANGUAGE),
        EZCMD.info "Parse files as language/configuration *.json files";

        [ "manifest" ], Arg.Unit (fun () -> kind := MANIFEST),
        EZCMD.info "Parse files as package.json files";

        [ "gen" ], Arg.String (fun s -> generate := Some s),
        EZCMD.info ~docv:"FILE" "Generate FILE from current configuration";

        [], Arg.Anons (fun list -> files := list),
        EZCMD.info ~docv:"FILES" "JSON Files to parse";
      ])
    ~doc:
      "parse VSODE JSON files or generate package.json"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P ""
      ];
    ]

let cmd =
  make_cmd ~extension_manifest:Vscode_extension.manifest
