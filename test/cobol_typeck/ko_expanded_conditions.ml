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

open Prog_printer

(* Only the diagnostics are of interest here: a condition that fails to expand
   has no expansion to show, and one rebuilt by error recovery would only show
   the dummy nodes that recovery filled it with. *)
let dotest = Typeck_testing.show_diagnostics

(* Same data division as [Ok_expanded_conditions]: A..D are plain data items,
   A-IS-1..A-IS-4 are condition-names. *)
let items = {|
       77 A PIC 9(4).
       88 A-IS-1 VALUE 1.
       88 A-IS-2 VALUE 2.
       88 A-IS-3 VALUE 3.
       88 A-IS-4 VALUE 4.
       77 B PIC 9(4).
       77 C PIC 9(4).
       77 D PIC 9(4).
    |}

let check ?(working_storage = items) cond =
  dotest @@ prog "cond" ~working_storage
    ~procedure:(Pretty.to_string "IF %s CONTINUE END-IF." cond)

(* --- Syntax errors --- *)

let%expect_test "repeated-not" =
  check "NOT NOT A-IS-1";
  [%expect {|
    prog.cob:13.14-13.17:
      10          77 C PIC 9(4).
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13 >        IF NOT NOT A-IS-1 CONTINUE END-IF.
    ----                 ^^^
      14
    >> Error: Invalid syntax |}];;

let%expect_test "repeated-and" =
  check "A-IS-1 AND AND A-IS-2";
  [%expect {|
    prog.cob:13.20:
      10          77 C PIC 9(4).
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13 >        IF A-IS-1 AND AND A-IS-2 CONTINUE END-IF.
    ----                       ^
      14
    >> Hint: Missing <condition>

    prog.cob:13.21-13.24:
      10          77 C PIC 9(4).
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13 >        IF A-IS-1 AND AND A-IS-2 CONTINUE END-IF.
    ----                        ^^^
      14
    >> Error: Invalid syntax |}];;

let%expect_test "repeated-and-under-not" =
  check "NOT (A-IS-1 AND AND A-IS-2)";
  [%expect {|
    prog.cob:13.25:
      10          77 C PIC 9(4).
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13 >        IF NOT (A-IS-1 AND AND A-IS-2) CONTINUE END-IF.
    ----                            ^
      14
    >> Hint: Missing <condition>

    prog.cob:13.26-13.29:
      10          77 C PIC 9(4).
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13 >        IF NOT (A-IS-1 AND AND A-IS-2) CONTINUE END-IF.
    ----                             ^^^
      14
    >> Error: Invalid syntax |}];;

(* --- Abbreviations with no subject to imply --- *)

let%expect_test "relop-without-subject" =
  check "> B";
  [%expect {|
    prog.cob:13.10-13.13:
      10          77 C PIC 9(4).
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13 >        IF > B CONTINUE END-IF.
    ----             ^^^
      14
    >> Error: Missing subject for abbreviated condition. |}];;

let%expect_test "object-without-subject" =
  check "B AND C";
  [%expect {|
    prog.cob:13.10-13.11:
      10          77 C PIC 9(4).
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13 >        IF B AND C CONTINUE END-IF.
    ----             ^
      14
    >> Error: Missing subject for abbreviated condition.

    prog.cob:13.16-13.17:
      10          77 C PIC 9(4).
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13 >        IF B AND C CONTINUE END-IF.
    ----                   ^
      14
    >> Error: Missing subject for abbreviated condition. |}];;

let%expect_test "class-cond-without-subject" =
  check "NUMERIC";
  [%expect {|
    prog.cob:13.10-13.17:
      10          77 C PIC 9(4).
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13 >        IF NUMERIC CONTINUE END-IF.
    ----             ^^^^^^^
      14
    >> Error: Missing subject for class condition. |}];;

let%expect_test "sign-cond-without-subject" =
  check "POSITIVE";
  [%expect {|
    prog.cob:13.10-13.18:
      10          77 C PIC 9(4).
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13 >        IF POSITIVE CONTINUE END-IF.
    ----             ^^^^^^^^
      14
    >> Error: Missing subject for sign condition. |}];;

(* An unresolved name expands to nothing at all, on purpose: reporting it here
   would duplicate the resolution failure reported when typechecking
   references. *)
let%expect_test "unknown-name" =
  check "A = UNDECLARED";
  [%expect {| |}];;
