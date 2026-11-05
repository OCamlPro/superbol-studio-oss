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

(** Displaying and changing data items in the variables view. *)

open Harness
open Harness.Syntax

(* 15: ADD 5 TO N.          (initial values)
   16: MOVE 7 TO ITEM (2).
   17: DISPLAY "N=" N " NAME=" NAME.   (N = 5, ITEM (2) = 7) *)
let vars = program "vars" [ "vars.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. vars.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N        PIC 9(4) VALUE 0.
       01 S        PIC S9(3)V99 VALUE -12.5.
       01 NAME     PIC X(8) VALUE "ALICE".
       01 REC.
          05 REC-ID   PIC 9(3) VALUE 42.
          05 REC-TAG  PIC X(4) VALUE "ABCD".
       01 TBL.
          05 ITEM     PIC 99 OCCURS 3 TIMES.
       01 C        PIC 9(5) COMP VALUE 77.
       PROCEDURE DIVISION.
           ADD 5 TO N.
           MOVE 7 TO ITEM (2).
           DISPLAY "N=" N " NAME=" NAME.
           STOP RUN.
|} ]

let src = "vars.cbl"
let initial = 15
let updated = 17

let unquote s =
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2) else s

let has_char c s = String.contains s c

let suite = { suite = "Variables"; tests = [

  expected "the Local scope lists the level-01 items of WORKING-STORAGE" vars (fun ctx ->
      let* s = start ctx src ~bps:[bp src initial] in
      let* _, locals = locals s in
      let names = List.map (fun v -> v.var_name) locals in
      let missing = List.filter (fun n -> not (List.mem n names))
          ["N"; "S"; "NAME"; "REC"; "TBL"; "C"] in
      check (missing = []) "missing items: %s (have %s)"
        (String.concat ", " missing) (String.concat ", " names));

  expected "alphanumeric items show their text" vars (fun ctx ->
      let* s = start ctx src ~bps:[bp src initial] in
      let* v = local s "NAME" in
      check (contains ~sub:"ALICE" v.value) "NAME shows %S" v.value);

  expected "values reflect the statements executed so far" vars (fun ctx ->
      let* s = start ctx src ~bps:[bp src updated] in
      let* v = local s "N" in
      check (digits v.value = Some 5) "N shows %S instead of 5" v.value);

  probe "changing a numeric item stores the number, right-justified" vars (fun ctx ->
      let* s = start ctx src ~bps:[bp src updated] in
      let* reference, _ = locals s in
      let* _ = set_variable ~reference "N" "42" in
      let* () = to_end "continue" in
      check (contains ~sub:"N=0042" (program_output ()))
        "program output: %S" (program_output ()));

  expected "changing an alphanumeric item affects the rest of the execution" vars (fun ctx ->
      let* s = start ctx src ~bps:[bp src updated] in
      let* reference, _ = locals s in
      let* _ = set_variable ~reference "NAME" "ZED" in
      let* () = to_end "continue" in
      check (contains ~sub:"NAME=ZED" (program_output ()))
        "program output: %S" (program_output ()));

  probe "numeric DISPLAY items show a number, not their raw storage" vars (fun ctx ->
      let* s = start ctx src ~bps:[bp src updated] in
      let* v = local s "N" in
      check (not (has_char '"' v.value) && digits v.value = Some 5)
        "N shows %S" v.value);

  probe "signed decimal items show their sign and decimal point" vars (fun ctx ->
      let* s = start ctx src ~bps:[bp src initial] in
      let* v = local s "S" in
      check (float_of_string_opt (String.trim (unquote v.value)) = Some (-12.5))
        "S (-12.5) shows %S" v.value);

  probe "binary (COMP) items show their numeric value" vars (fun ctx ->
      let* s = start ctx src ~bps:[bp src initial] in
      let* v = local s "C" in
      check (not (has_char '\\' v.value) && digits v.value = Some 77)
        "C (77) shows %S" v.value);

  probe "items are typed after their COBOL category" vars (fun ctx ->
      let* s = start ctx src ~bps:[bp src initial] in
      let* n = local s "N" in
      let* name = local s "NAME" in
      check (n.var_type <> Some "group" && name.var_type <> Some "group")
        "N is typed %s, NAME is typed %s"
        (Option.value n.var_type ~default:"?") (Option.value name.var_type ~default:"?"));

  probe "group items expand into their subordinate items" vars (fun ctx ->
      let* s = start ctx src ~bps:[bp src initial] in
      let* rec_ = local s "REC" in
      let* () = check (rec_.reference > 0) "REC cannot be expanded (shows %S)" rec_.value in
      let* children = variables rec_.reference in
      let* id = find_variable "REC-ID" children in
      let* tag = find_variable "REC-TAG" children in
      check (digits id.value = Some 42 && contains ~sub:"ABCD" tag.value)
        "REC-ID shows %S, REC-TAG shows %S" id.value tag.value);

  probe "tables expand into their elements" vars (fun ctx ->
      let* s = start ctx src ~bps:[bp src updated] in
      let* tbl = local s "TBL" in
      let* () = check (tbl.reference > 0) "TBL cannot be expanded (shows %S)" tbl.value in
      let* children = variables tbl.reference in
      let values = List.map (fun v -> v.value) children in
      check (List.length children >= 3 && List.exists (fun v -> digits v = Some 7) values)
        "TBL expands into: %s" (String.concat ", " values));
] }
