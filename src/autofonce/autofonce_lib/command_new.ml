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

open Ez_win32.V1
open Ezcmd.V2
open EZCMD.TYPES
open Ez_file.V1
open Ez_call.V1

module Misc = Autofonce_misc.Misc
module Parser = Autofonce_core.Parser

let cmd =
  let files = ref [] in
  let captures = ref [] in
  let keywords = ref [] in
  let command = ref [] in
  let output = ref None in
  let name = ref "" in
  let args =
    [
      [ "name" ], Arg.String (fun s -> name := s),
      EZCMD.info ~docv:"NAME" "Store name in AT_SETUP(...)";

      [ "f" ], Arg.String (fun file -> files := file :: !files),
      EZCMD.info ~docv:"FILE" "Store file in AT_DATA(...)";

      [ "k" ], Arg.String (fun s -> keywords := s :: !keywords),
      EZCMD.info ~docv:"KEYWORD" "Store keyword in AT_KEYWORDS(...)";

      [ "c" ], Arg.String (fun file -> captures := file :: !captures),
      EZCMD.info ~docv:"FILE" "Capture file in AT_CAPTURE_FILE(...)";

      [ "o" ; "output" ], Arg.String (fun s -> output := Some s),
      EZCMD.info ~docv:"FILE" "Name of generated file";

      [], Arg.Anons (fun list -> command := list),
      EZCMD.info "List of arguments" ;
    ]
  in
  EZCMD.sub
    "new"
    (fun () ->

       let files = List.map (fun file ->
           let content = EzFile.read_text_file file in
           (file, content)
         ) !files
       in

       let command = match !command with
         | [] -> Misc.error "Missing command to run"
         | command -> command
       in

       let stdout = EzCall.tmpfile () in
       let stderr = EzCall.tmpfile () in
       let pid = EzCall.create_process ~stdout ~stderr command in
       let retcode = Unix.uninterrupted_waitpid pid in
       let stdout_content = EzFile.read_text_file stdout in
       let stderr_content = EzFile.read_text_file stderr in
       Sys.remove stdout;
       Sys.remove stderr;
       let b = Buffer.create 10000 in

       Printf.bprintf b "AT_SETUP(%s)\n"
         (Parser.m4_escape !name);

       begin
         match !keywords with
         | [] -> ()
         | list ->
             Printf.bprintf b "\nAT_KEYWORDS(%s)\n"
               (Parser.m4_escape (String.concat " " list))
       end;

       begin
         match files with
         | [] -> ()
         | files ->
             Printf.bprintf b "\n";
             List.iter (fun (file, content) ->
                 Printf.bprintf b "AT_DATA(%s,%s)\n"
                   (Parser.m4_escape file) (Parser.m4_escape content)
               ) files;
       end;

       begin
         match !captures with
         | [] -> ()
         | files ->
             Printf.bprintf b "\n";
             List.iter (fun file ->
                 Printf.bprintf b "AT_CAPTURE_FILE(%s)\n"
                   (Parser.m4_escape file)
               ) files;
       end;

       Printf.bprintf b "\nAT_CHECK(%s, [%d], %s, %s)\n"
         (Parser.m4_escape @@ String.concat " " command)
         retcode
         (Parser.m4_escape stdout_content)
         (Parser.m4_escape stderr_content);

       Printf.bprintf b "\nAT_CLEANUP\n";

       let s = Buffer.contents b in
       match !output with
       | None -> Printf.printf "%s%!" s
       | Some file ->
           EzFile.write_text_file file s
    )
    ~args
    ~doc: "Create a new test by running a command"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P {|Runs the command, captures its retcode, stdout and stderr
and generates the corresponding autoconf testsuite file.|}
      ];
    ]
