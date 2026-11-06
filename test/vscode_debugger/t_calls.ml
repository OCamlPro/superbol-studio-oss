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

(** Debugging programs that CALL sub-programs from other source files. *)

open Harness
open Harness.Syntax

(* caller.cbl:
    8: CALL "callee" USING N.     (N = 1)
    9: DISPLAY "after " N.
   10: PERFORM DO-CALL.
   13: (DO-CALL) CALL "callee" USING N.
   14: (DO-CALL) ADD 1 TO N.

   callee.cbl:
    8: ADD 10 TO N.
    9: PERFORM WORK.
   10: GOBACK.
   12: (WORK) ADD 100 TO N. *)
let calls = program "calls" [
    "caller.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. caller.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N PIC 9(4) VALUE 1.
       PROCEDURE DIVISION.
       MAIN.
           CALL "callee" USING N.
           DISPLAY "after " N.
           PERFORM DO-CALL.
           STOP RUN.
       DO-CALL.
           CALL "callee" USING N.
           ADD 1 TO N.
|};
    "callee.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. callee.
       DATA DIVISION.
       LINKAGE SECTION.
       01 N PIC 9(4).
       PROCEDURE DIVISION USING N.
       MAIN.
           ADD 10 TO N.
           PERFORM WORK.
           GOBACK.
       WORK.
           ADD 100 TO N.
|} ]

let caller = "caller.cbl"
let callee = "callee.cbl"

let start_calls ctx bps = start ctx caller ~group:[callee] ~bps

let expect_at ~what file line stop =
  check (file_of stop = Some file && line_of stop = line)
    "%s: expected %s:%d, got %s:%d" what file line
    (Option.value (file_of stop) ~default:"?") (line_of stop)

let frames_to_string frames =
  String.concat ", " (List.map (fun f -> Printf.sprintf "%s@%d" f.frame_name f.line) frames)

let suite = { suite = "CALLed programs"; tests = [

  expected "step over CALL does not enter the called program" calls (fun ctx ->
      let* _ = start_calls ctx [bp caller 8] in
      let* s = step_over () in
      expect_at ~what:"after stepping over CALL" caller 9 s);

  expected "step into CALL enters the called program" calls (fun ctx ->
      let* _ = start_calls ctx [bp caller 8] in
      let* s = step_into () in
      expect_at ~what:"after stepping into CALL" callee 8 s);

  expected "step over GOBACK returns to the calling program" calls (fun ctx ->
      let* _ = start_calls ctx [bp callee 9] in
      let* s = step_over () in
      let* () = expect_at ~what:"after stepping over PERFORM" callee 10 s in
      let* s = step_over () in
      expect_at ~what:"after stepping over GOBACK" caller 9 s);

  expected "step over a PERFORM in a called program stays in that program" calls (fun ctx ->
      let* _ = start_calls ctx [bp callee 9] in
      let* s = step_over () in
      expect_at ~what:"after stepping over PERFORM" callee 10 s);

  expected "the call stack shows the called program above the calling one" calls (fun ctx ->
      let* _ = start_calls ctx [bp caller 8] in
      let* s = step_into () in
      let* frames = stack_trace s in
      let rec index i p = function
        | [] -> None | f :: _ when p f -> Some i | _ :: l -> index (i + 1) p l in
      let starts prefix f =
        String.length f.frame_name >= String.length prefix
        && String.sub f.frame_name 0 (String.length prefix) = prefix in
      check (match index 0 (starts "callee_") frames, index 0 (starts "caller_") frames with
          | Some 0, Some j -> j > 0
          | _ -> false)
        "frames: %s" (frames_to_string frames));

  probe "the call stack shows the CALL statement in the calling program" calls (fun ctx ->
      let* _ = start_calls ctx [bp caller 8] in
      let* s = step_into () in
      let* frames = stack_trace s in
      check (List.exists (fun f -> f.file = Some caller && f.line = 8) frames)
        "frames: %s" (frames_to_string frames));

  probe "breakpoint on GOBACK is hit" calls (fun ctx ->
      let* s = start_calls ctx [bp callee 10] in
      expect_at ~what:"stop" callee 10 s);

  probe "step out of a called program returns to the calling program" calls (fun ctx ->
      let* _ = start_calls ctx [bp caller 8] in
      let* _ = step_into () in
      let* s = step_out () in
      expect_at ~what:"after stepping out" caller 9 s);

  probe "breakpoints in several source files are all hit" calls (fun ctx ->
      let* s = start_calls ctx [bp caller 9; bp callee 12] in
      let* () = expect_at ~what:"first stop" callee 12 s in
      let* s = continue_ () in
      expect_at ~what:"second stop" caller 9 s);

  probe "changing breakpoints in a file keeps those of other files" calls (fun ctx ->
      let* s = start_calls ctx [bp caller 9; bp callee 12] in
      let* () = expect_at ~what:"first stop" callee 12 s in
      (* Makes VS Code send the breakpoints of callee.cbl only. *)
      let* () = update_breakpoints (fun () -> add_breakpoints ctx [bp callee 8]) in
      let* s = continue_ () in
      expect_at ~what:"second stop" caller 9 s);

  probe "LINKAGE items of the called program show the passed data" calls (fun ctx ->
      let* s = start_calls ctx [bp callee 8] in
      let* v = local s "N" in
      check (digits v.value = Some 1) "N (1) shows %S" v.value);

  probe "the call stack only shows COBOL programs" calls (fun ctx ->
      let* _ = start_calls ctx [bp caller 8] in
      let* s = step_into () in
      let* frames = stack_trace s in
      let others = List.filter (fun f -> f.line <= 0 || f.file = None) frames in
      check (others = []) "non-COBOL frames: %s"
        (String.concat ", " (List.map (fun f -> f.frame_name) others)));
] }
