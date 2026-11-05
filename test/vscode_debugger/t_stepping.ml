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

(** Stepping through statements and PERFORMed paragraphs. *)

open Harness
open Harness.Syntax

(*  9: DISPLAY "start".
   10: PERFORM PARA-A.
   11: PERFORM PARA-B 2 TIMES.
   12: PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
   13:    ADD 1 TO N
   14:    PERFORM PARA-A
   16: PERFORM PARA-A THRU PARA-B.
   17: DISPLAY "end " N.
   18: STOP RUN.
   20: (PARA-A) ADD 10 TO N.
   21: (PARA-A) PERFORM PARA-B.
   23: (PARA-B) ADD 100 TO N. *)
let performs = program "performs" [ "performs.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. performs.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I PIC 9(4) VALUE 0.
       01 N PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "start".
           PERFORM PARA-A.
           PERFORM PARA-B 2 TIMES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
              ADD 1 TO N
              PERFORM PARA-A
           END-PERFORM.
           PERFORM PARA-A THRU PARA-B.
           DISPLAY "end " N.
           STOP RUN.
       PARA-A.
           ADD 10 TO N.
           PERFORM PARA-B.
       PARA-B.
           ADD 100 TO N.
|} ]

let src = "performs.cbl"

let start_at ctx lines = start ctx src ~bps:(List.map (bp src) lines)

let expect_line ~what expected stop = expect_int ~what expected (line_of stop)

(** Steps over until line [last], and returns the lines of all stops. *)
let rec steps_until ~last ?(limit = 30) acc : int list res =
  if limit = 0 then error "too many steps: %s"
      (String.concat " " (List.rev_map string_of_int acc))
  else
    let* s = step_over () in
    let acc = line_of s :: acc in
    if line_of s = last then ok (List.rev acc)
    else steps_until ~last ~limit:(limit - 1) acc

let suite = { suite = "Stepping"; tests = [

  probe "step over walks the main paragraph without entering performed paragraphs"
    performs (fun ctx ->
        let* s = start_at ctx [9] in
        let* () = expect_line ~what:"first stop" 9 s in
        let* lines = steps_until ~last:18 [] in
        (* Inline PERFORMs may stop several times on line 12 (VARYING, UNTIL). *)
        let collapsed = List.filteri (fun i l -> i = 0 || l <> List.nth lines (i - 1)) lines in
        check (collapsed = [10; 11; 12; 13; 14; 12; 13; 14; 12; 16; 17; 18])
          "unexpected stops: %s" (String.concat " " (List.map string_of_int lines)));

  probe "step over PERFORM ... TIMES skips all iterations" performs (fun ctx ->
      let* s = start_at ctx [11] in
      let* () = expect_line ~what:"first stop" 11 s in
      let* s = step_over () in
      expect_line ~what:"after stepping over" 12 s);

  probe "step into PERFORM enters the paragraph" performs (fun ctx ->
      let* _ = start_at ctx [10] in
      let* s = step_into () in
      check (List.mem (line_of s) [19; 20]) "expected line 19 or 20, got %d" (line_of s));

  probe "step into a statement other than PERFORM steps over it" performs (fun ctx ->
      let* _ = start_at ctx [9] in
      let* s = step_into () in
      expect_line ~what:"after stepping into DISPLAY" 10 s);

  probe "step out of nested paragraphs returns to the main paragraph" performs (fun ctx ->
      (* The first stop on line 23 is from 10 > PARA-A > 21 > PARA-B. *)
      let* _ = start_at ctx [23] in
      let* s = step_out () in
      let* () = expect_line ~what:"after stepping out" 11 s in
      let* s = continue_ () in
      expect_line ~what:"after continuing" 23 s);

  probe "step out of a paragraph reached by breakpoint" performs (fun ctx ->
      let* _ = start_at ctx [20] in
      let* s = step_out () in
      let* () = expect_line ~what:"after stepping out" 11 s in
      let* s = step_over () in
      expect_line ~what:"after stepping over" 12 s);

  probe "step out of a paragraph performed THRU continues with the range" performs
    (fun ctx ->
       (* 6th stop on 23 is via 16 > PARA-A > 21; PARA-A then falls through
          into PARA-B, as the PERFORM on line 16 goes THRU PARA-B. *)
       let* _ = start_at ctx [23] in
       let rec skip n = if n = 0 then ok () else let* _ = continue_ () in skip (n - 1) in
       let* () = skip 5 in
       let* s = step_out () in
       expect_line ~what:"after stepping out" 23 s);

  probe "breakpoints interrupt step over" performs (fun ctx ->
      let* s = start_at ctx [11; 23] in
      let* () = expect_line ~what:"first stop" 23 s in
      let* s = continue_ () in
      let* () = expect_line ~what:"second stop" 11 s in
      let* s = step_over () in
      expect_line ~what:"after stepping over" 23 s);

  expected "step over STOP RUN ends the session" performs (fun ctx ->
      let* _ = start_at ctx [18] in
      to_end "stepOver");

  expected "step out of the main paragraph ends the session" performs (fun ctx ->
      let* _ = start_at ctx [17] in
      to_end "stepOut");
] }
