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

(* GnuCOBOL testsuite, [unused subject or relop in abbreviated condition] in
   tests/testsuite.src/syn_misc.at: when a condition-name follows an
   abbreviation, it is a condition of its own, and the subject and relational
   operator that the abbreviation had set up end up unused. *)

open Prog_printer

let dotest = Typeck_testing.show_diagnostics

let items = {|
       77 A PIC 9(4).
       88 A-IS-1 VALUE 1.
    |}

let check ?(working_storage = items) cond =
  dotest @@ prog "cond" ~working_storage
    ~procedure:(Pretty.to_string "IF %s CONTINUE END-IF." cond)

(* --- Both the subject and the relational operator are dropped --- *)

let%expect_test "unused-subject-n-relop" =
  check "A = A-IS-1";
  [%expect {|
    prog.cob:7.10-7.11:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF A = A-IS-1 CONTINUE END-IF.
    ----             ^
       8
    >> Warning: This abbreviated condition subject is never used.

    prog.cob:7.12-7.13:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF A = A-IS-1 CONTINUE END-IF.
    ----               ^
       8
    >> Warning: This abbreviated condition relation operator is never used. |}];;

let%expect_test "unused-subject-n-relop-under-not" =
  check "NOT A = A-IS-1";
  [%expect {|
    prog.cob:7.14-7.15:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF NOT A = A-IS-1 CONTINUE END-IF.
    ----                 ^
       8
    >> Warning: This abbreviated condition subject is never used.

    prog.cob:7.16-7.17:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF NOT A = A-IS-1 CONTINUE END-IF.
    ----                   ^
       8
    >> Warning: This abbreviated condition relation operator is never used. |}];;

let%expect_test "unused-subject-n-negated-relop" =
  check "A NOT = A-IS-1";
  [%expect {|
    prog.cob:7.10-7.11:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF A NOT = A-IS-1 CONTINUE END-IF.
    ----             ^
       8
    >> Warning: This abbreviated condition subject is never used.

    prog.cob:7.12-7.17:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF A NOT = A-IS-1 CONTINUE END-IF.
    ----               ^^^^^
       8
    >> Warning: This abbreviated condition relation operator is never used. |}];;

let%expect_test "unused-subject-n-relop-in-parens" =
  check "A > (A-IS-1)";
  [%expect {|
    prog.cob:7.10-7.11:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF A > (A-IS-1) CONTINUE END-IF.
    ----             ^
       8
    >> Warning: This abbreviated condition subject is never used.

    prog.cob:7.12-7.13:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF A > (A-IS-1) CONTINUE END-IF.
    ----               ^
       8
    >> Warning: This abbreviated condition relation operator is never used. |}];;

(* The subject set up inside the parentheses is dropped by the condition it was
   written for, and gone for the one that follows. *)
let%expect_test "unused-subject-n-relop-before-further-cond" =
  check "(A = A-IS-1) OR A-IS-1";
  [%expect {|
    prog.cob:7.11-7.12:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF (A = A-IS-1) OR A-IS-1 CONTINUE END-IF.
    ----              ^
       8
    >> Warning: This abbreviated condition subject is never used.

    prog.cob:7.13-7.14:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF (A = A-IS-1) OR A-IS-1 CONTINUE END-IF.
    ----                ^
       8
    >> Warning: This abbreviated condition relation operator is never used. |}];;

(* --- Only the relational operator is dropped --- *)

let%expect_test "unused-relop" =
  check "A = 1 OR > A-IS-1";
  [%expect {|
    prog.cob:7.19-7.20:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF A = 1 OR > A-IS-1 CONTINUE END-IF.
    ----                      ^
       8
    >> Warning: This abbreviated condition relation operator is never used. |}];;

let%expect_test "unused-negated-relop" =
  check "A = 1 OR NOT > A-IS-1";
  [%expect {|
    prog.cob:7.19-7.24:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF A = 1 OR NOT > A-IS-1 CONTINUE END-IF.
    ----                      ^^^^^
       8
    >> Warning: This abbreviated condition relation operator is never used. |}];;

let%expect_test "unused-relop-in-parenthesized-relops" =
  check "A (= 1 OR = A-IS-1)";
  [%expect {|
    prog.cob:7.20-7.21:
       4          77 A PIC 9(4).
       5          88 A-IS-1 VALUE 1.
       6          PROCEDURE DIVISION.
       7 >        IF A (= 1 OR = A-IS-1) CONTINUE END-IF.
    ----                       ^
       8
    >> Warning: This abbreviated condition relation operator is never used. |}];;

(* --- Nothing is dropped here, so nothing is reported --- *)

let%expect_test "used-subject-n-relop-before-parenthesized-cond" =
  check "A = 1 AND (A-IS-1)";
  [%expect {| |}];;

let%expect_test "used-subject-n-relop-before-class-cond" =
  check "A = 1 AND A IS NUMERIC";
  [%expect {| |}];;

let%expect_test "used-subject-n-relop-before-cond" =
  check "A = 1 OR A-IS-1";
  [%expect {| |}];;
