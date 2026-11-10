(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** Running, pausing, and stopping programs; program output and runtime
    errors; modules run with [cobcrun]. *)

open Harness
open Harness.Syntax

(* 4: DISPLAY "Hello from COBOL".
   5: DISPLAY "second line".
   6: STOP RUN RETURNING 3. *)
let hello = program "hello" [ "hello.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.
       PROCEDURE DIVISION.
           DISPLAY "Hello from COBOL".
           DISPLAY "second line".
           STOP RUN RETURNING 3.
|} ]

(* 8-10: endless loop. *)
let loop = program "loop" [ "loop.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. loop.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 K PIC 9(9) VALUE 0.
       PROCEDURE DIVISION.
           DISPLAY "looping".
           PERFORM UNTIL K > 999999999
              ADD 1 TO K
           END-PERFORM.
           STOP RUN.
|} ]

(* 10: MOVE 1 TO ITEM (IDX), with IDX out of bounds; built with runtime
   checks. *)
let rterr = program "rterr" ~cobc_args:("-x" :: debug_args @ [ "-debug" ]) [ "rterr.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. rterr.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 IDX PIC 99 VALUE 5.
       01 TBL.
          05 ITEM PIC 9 OCCURS 3 TIMES.
       PROCEDURE DIVISION.
           DISPLAY "before".
           MOVE 1 TO ITEM (IDX).
           DISPLAY "after".
           STOP RUN.
|} ]

(* Built as a module, to run with cobcrun. *)
let modul = program "modul" ~cobc_args:("-m" :: debug_args) [ "modul.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. modul.
       PROCEDURE DIVISION.
           DISPLAY "in module".
           GOBACK.
|} ]

let suite = { suite = "Session lifecycle"; tests = [

  expected "a program without breakpoints runs to completion" hello (fun ctx ->
      let* () = launch ctx "hello.cbl" in
      terminated ());

  expected "program output appears in the debug console" hello (fun ctx ->
      let* () = launch ctx "hello.cbl" in
      let* () = terminated () in
      check (contains ~sub:"Hello from COBOL" (program_output ())
             && contains ~sub:"second line" (program_output ()))
        "program output: %S" (program_output ()));

  expected "stopping the debugger ends the session" hello (fun ctx ->
      let* _ = start ctx "hello.cbl" ~bps:[bp "hello.cbl" 4] in
      to_end "stop");

  expected "a module is run with cobcrun, and its breakpoints are hit" modul (fun ctx ->
      let* s = start ctx "modul.cbl" ~bps:[bp "modul.cbl" 4] in
      expect_int ~what:"stop" 4 (line_of s));

  expected "runtime errors are reported" rterr (fun ctx ->
      let* () = launch ctx "rterr.cbl" in
      let* _ = next_event ~deadline:(Node.Date.now () +. event_timeout_ms) in
      let all = program_output () ^ console_output () in
      diagnose "[output and console: %S]" all;
      check (contains ~sub:"ITEM" all) "no mention of ITEM in the output or debug console");

  probe "program output is shown as soon as it is printed" hello (fun ctx ->
      let* _ = start ctx "hello.cbl" ~bps:[bp "hello.cbl" 5] in
      (* Give the output some time to reach the debug console. *)
      let* () = Promise.map Result.ok (sleep 500) in
      check (contains ~sub:"Hello from COBOL" (program_output ()))
        "program output when stopped on line 5: %S" (program_output ()));

  probe "pausing a running program stops it at a COBOL statement" loop (fun ctx ->
      let* () = launch ctx "loop.cbl" in
      let* () = Promise.map Result.ok (sleep 1000) in
      let* s = pause () in
      check (file_of s = Some "loop.cbl" && List.mem (line_of s) [8; 9; 10])
        "paused at %s:%d" (Option.value (file_of s) ~default:"?") (line_of s));

  probe "a runtime error stops the program at the faulty statement" rterr (fun ctx ->
      let* () = launch ctx "rterr.cbl" in
      let* s = next_stop () in
      expect_int ~what:"stop" 10 (line_of s));

  probe "the debug console does not show gdb's internal messages" hello (fun ctx ->
      let* () = launch ctx "hello.cbl" in
      let* () = terminated () in
      let console = console_output () in
      (* What gdb prints depends on its version and configuration. *)
      diagnose "[console: %S]"
        (if String.length console > 300 then String.sub console 0 300 ^ "..." else console);
      check (not (contains ~sub:"shared library event" console
                  || contains ~sub:"debuginfod" console))
        "the debug console shows internal messages of gdb");
] }
