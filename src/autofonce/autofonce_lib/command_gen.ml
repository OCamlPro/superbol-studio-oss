(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.0, as described in the LICENSE.md file in the root  *)
(*  directory of this source tree.                                        *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(*
open Ezcmd.V2
open EZCMD.TYPES
open Ez_file.V1

module Misc = Autofonce_misc.Misc
module Parser = Autofonce_core.Parser

let cmd =
  let name = ref "testsuite" in
  let build = ref None in
  let run = ref None in
  let files = ref [] in
  let data = ref [] in

  let args =
    [
      [ "name" ], Arg.String (fun s -> name := s),
      EZCMD.info ~docv:"NAME" "Store name in AT_INIT(...)";

      [ "f"; "file" ], Arg.String (fun s -> data := s :: !data),
      EZCMD.info ~docv:"FILE" "Add file to each test AT_DATA";

      [ "build" ], Arg.String (fun s -> build := Some s),
      EZCMD.info ~docv:"FILE" "Build command";

      [ "run" ], Arg.String (fun s -> run := Some s),
      EZCMD.info ~docv:"FILE" "Run command";

      [], Arg.Anons (fun list -> files := list),
      EZCMD.info "List of files" ;
    ]
  in
  EZCMD.sub
    "gen"
    (fun () ->

       begin match !build, !run with
           None, None -> Misc.error "Missing either --build or --run argument"
         | _ -> ()
       end;

       let top_basename = "tests" in
       let sub_basename = "testsuite.src" in
       let gen_basename = "testsuite.gen" in
       let shared_basename = "shared" in
       let files_basename = "files" in

       let dir = top_basename in
       let sub_dir = Filename.concat dir sub_basename in
       let gen_dir = Filename.concat dir gen_basename in
       let shared_dir = Filename.concat gen_dir shared_basename in
       let files_dir = Filename.concat gen_dir files_basename in

       if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
       if not (Sys.file_exists sub_dir) then Unix.mkdir sub_dir 0o755;
       if not (Sys.file_exists gen_dir) then Unix.mkdir gen_dir 0o755;
       if not (Sys.file_exists shared_dir) then Unix.mkdir shared_dir 0o755;
       if not (Sys.file_exists files_dir) then Unix.mkdir files_dir 0o755;

       let files = List.map (fun file ->
           let basename = Filename.basename file in
           let content = EzFile.read_text_file file in
           EzFile.write_file (Filename.concat files_dir basename) content;
           basename
         ) !files
       in

       let data = List.map (fun file ->
           let basename = Filename.basename file in
           let content = EzFile.read_text_file file in
           EzFile.write_file (Filename.concat shared_dir basename) content;
           basename
         ) (List.rev !data)
       in

       let contents =
         let b = Buffer.create 10000 in

         Printf.bprintf b "# File generated with:\n";
         Printf.bprintf b "#";
         Array.iter (fun s -> Printf.bprintf b " '%s'" s) Sys.argv;
         Printf.bprintf b "\n\n";

         Printf.bprintf b "AT_COPYRIGHT(No copyright given)\n\n";
         Printf.bprintf b "# Name of testsuite\n";
         Printf.bprintf b "AT_INIT(%s)\n\n" (Parser.m4_escape !name);
         Printf.bprintf b "AT_COLOR_TESTS\n\n";
         Printf.bprintf b "# Tools being tested\n";
         Printf.bprintf b "# AT_TESTED([ ... tools ... ])\n\n";

         Printf.bprintf b "# First section\n";
         Printf.bprintf b "AT_BANNER([Main tests])\n\n";

         List.iter (fun basename ->
             Printf.bprintf b "m4_include(%s)\n\n"
               (Parser.m4_escape @@
                (Filename.concat sub_basename @@
                 (Filename.chop_extension basename ^ ".at")))
           ) files;
         Buffer.contents b
       in

       let output = Filename.concat dir "testsuite.at" in
       EzFile.write_file output contents;

       let script = Buffer.create 10000 in
       List.iter (fun basename ->

           Printf.bprintf script "\n# autofonce script to regen %s.at\n\n"
             basename;

           Printf.bprintf script "test: %s\n"
             (Filename.chop_extension basename);
           Printf.bprintf script "# keywords: \n";

           List.iter (fun file ->
               Printf.bprintf script "data: %s/%s\n" shared_basename file
             ) data;

           Printf.bprintf script "data: %s/%s\n" files_basename basename;
           Printf.bprintf script "target: %s\n" basename;

           begin
             match !build with
             | None -> ()
             | Some command ->
                 Printf.bprintf script "command: %s\n" command;
                 Printf.bprintf script "#stdout: file.stdout\n";
                 Printf.bprintf script "#stderr: file.stderr\n";
                 Printf.bprintf script "run: 0\n";
           end;

           begin
             match !run with
             | None -> ()
             | Some command ->
                 Printf.bprintf script "command: %s\n" command;
                 Printf.bprintf script "#stdout: file.stdout\n";
                 Printf.bprintf script "#stderr: file.stderr\n";
                 Printf.bprintf script "run: 0\n";
           end;

           Printf.bprintf script "save: ../%s/%s.at\n\n\n"
             sub_basename (Filename.chop_extension basename) ;

         ) files ;

       let script_file = Filename.concat gen_dir "testsuite.atscript" in
       EzFile.write_file script_file (Buffer.contents script);

       Printf.eprintf "File %S generated\n%!" output;
       Command_regen.regen ();
       Printf.eprintf "1. Use `autofonce init` to initialize autofonce.toml file\n";
       Printf.eprintf "2. Use `autofonce run` to run the generated tests afterwards.\n";
       Printf.eprintf "3. Fix the tests and run `autofonce promote` to run and promote tests.\n";
    )
    ~args
    ~doc: "Generate a testsuite"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P {|Generates a full testsuite in directory tests/ from a set of data files.|}
      ];
    ]
*)
