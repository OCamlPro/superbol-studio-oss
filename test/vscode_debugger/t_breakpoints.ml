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

(** Line, conditional, hit-count, and function breakpoints. *)

open Harness
open Harness.Syntax

(*  5: 01 I     PIC 99 VALUE 0.
    9: PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
   10:    ADD I TO TOTAL
   12: (comment line)
   13: PERFORM SHOW.
   14: STOP RUN.
   15: SHOW.  (paragraph header)
   16: DISPLAY "TOTAL=" TOTAL.  (last statement of the program) *)
let bps = program "bps" [ "bps.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. bps.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I     PIC 99 VALUE 0.
       01 TOTAL PIC 999 VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
              ADD I TO TOTAL
           END-PERFORM.
      * a comment line
           PERFORM SHOW.
           STOP RUN.
       SHOW.
           DISPLAY "TOTAL=" TOTAL.
|} ]

let src = "bps.cbl"

let expect_line ~what expected stop = expect_int ~what expected (line_of stop)

(** Value of [I] at [stop], read from the variables view. *)
let loop_index stop =
  let* v = local stop "I" in
  match digits v.value with
  | Some i -> ok i
  | None -> error "cannot read I from %S" v.value

let rec continue_n n = if n = 0 then ok () else let* _ = continue_ () in continue_n (n - 1)

let suite = { suite = "Breakpoints"; tests = [

  expected "a breakpoint in a loop stops on each iteration" bps (fun ctx ->
      let* s = start ctx src ~bps:[bp src 10] in
      let* () = expect_line ~what:"first stop" 10 s in
      let rec iterations n =
        if n = 0 then ok ()
        else let* s = continue_ () in
          let* () = expect_line ~what:"next iteration" 10 s in
          iterations (n - 1) in
      let* () = iterations 4 in
      to_end "continue");

  expected "breakpoint on STOP RUN" bps (fun ctx ->
      let* s = start ctx src ~bps:[bp src 14] in
      expect_line ~what:"stop" 14 s);

  expected "breakpoint on the last statement of the program" bps (fun ctx ->
      let* s = start ctx src ~bps:[bp src 16] in
      expect_line ~what:"stop" 16 s);

  expected "breakpoints added while stopped are hit" bps (fun ctx ->
      let* _ = start ctx src ~bps:[bp src 13] in
      let* () = update_breakpoints (fun () -> add_breakpoints ctx [bp src 16]) in
      let* s = continue_ () in
      expect_line ~what:"stop on the added breakpoint" 16 s);

  expected "removed breakpoints are not hit anymore" bps (fun ctx ->
      let* _ = start ctx src ~bps:[bp src 10; bp src 16] in
      let* () = update_breakpoints remove_all_breakpoints in
      to_end "continue");

  expected "disabled breakpoints are not hit" bps (fun ctx ->
      let* s = start ctx src ~bps:[bp src 10 ~enabled:false; bp src 16] in
      expect_line ~what:"first stop" 16 s);

  expected "breakpoints outside the procedure division are never hit" bps (fun ctx ->
      let* () = launch ctx src ~bps:[bp src 5] in
      terminated ());

  probe "conditional breakpoint with a COBOL condition stops only when it holds" bps
    (fun ctx ->
       let* s = start ctx src ~bps:[bp src 10 ~condition:"I = 3"] in
       let* () = expect_line ~what:"stop" 10 s in
       let* i = loop_index s in
       let* () = expect_int ~what:"I at the first stop" 3 i in
       to_end "continue");

  probe "hit-count breakpoint stops on the given hit" bps (fun ctx ->
      let* s = start ctx src ~bps:[bp src 10 ~hit:"3"] in
      let* () = expect_line ~what:"stop" 10 s in
      let* i = loop_index s in
      expect_int ~what:"I at the first stop" 3 i);

  probe "breakpoint on a comment line stops at the next statement" bps (fun ctx ->
      let* s = start ctx src ~bps:[bp src 12] in
      expect_line ~what:"stop" 13 s);

  probe "breakpoint on a paragraph header stops at the paragraph" bps (fun ctx ->
      let* s = start ctx src ~bps:[bp src 15] in
      check (List.mem (line_of s) [15; 16]) "expected line 15 or 16, got %d" (line_of s));

  probe "function breakpoint on the program name stops in the program" bps (fun ctx ->
      let* s = start ctx src ~bps:[Function "bps"] in
      check (file_of s = Some src && line_of s > 0)
        "stopped outside of %s (line %d)" src (line_of s));

  probe "function breakpoint on a paragraph name stops in the paragraph" bps (fun ctx ->
      let* s = start ctx src ~bps:[Function "SHOW"] in
      check (List.mem (line_of s) [15; 16]) "expected line 15 or 16, got %d" (line_of s));
] }
