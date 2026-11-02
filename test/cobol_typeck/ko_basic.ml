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

let dotest = Typeck_testing.show_diagnostics

let%expect_test "missing-specs" =
  dotest @@ prog "missing-specs"
    ~working_storage:{|
       01 A.
       01 B OCCURS 7.
         02 FILLER.
    |};
  [%expect {|
    prog.cob:4.7-4.12:
       1          PROGRAM-ID. missing-specs.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 A.
    ----          ^^^^^
       5          01 B OCCURS 7.
       6            02 FILLER.
    >> Error: Missing PICTURE clause for item 'A'

    prog.cob:6.9-6.19:
       3          WORKING-STORAGE SECTION.
       4          01 A.
       5          01 B OCCURS 7.
       6 >          02 FILLER.
    ----            ^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    >> Error: Missing PICTURE clause for FILLER item |}];;

let%expect_test "error-group-with-pic" =
  dotest @@ prog "error-group-with-pic"
    ~working_storage:{|
       01 A PIC 9.
         02 FILLER PIC 9.
    |};
  [%expect {|
    prog.cob:4.12-4.17:
       1          PROGRAM-ID. error-group-with-pic.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 A PIC 9.
    ----               ^^^^^
       5            02 FILLER PIC 9.
       6          PROCEDURE DIVISION.
    >> Error: Unexpected PICTURE clause for group item 'A'
  |}];;

let%expect_test "non-toplevel-proc-arg" =
  dotest @@ prog "non-toplevel-proc-arg"
    ~linkage:{|
       01 A.
         02 A-1 PIC X.
           88 A-OK VALUE "Y".
    |}
    ~using:"A-1 A-OK";
  [%expect {|
    prog.cob:7.32-7.35:
       4          01 A.
       5            02 A-1 PIC X.
       6              88 A-OK VALUE "Y".
       7 >        PROCEDURE DIVISION USING A-1 A-OK.
    ----                                   ^^^
       8
    >> Error: Data name 'A-1' does not reference a 01- or 77-level item in the
              DATA DIVISION

    prog.cob:7.36-7.40:
       4          01 A.
       5            02 A-1 PIC X.
       6              88 A-OK VALUE "Y".
       7 >        PROCEDURE DIVISION USING A-1 A-OK.
    ----                                       ^^^^
       8
    >> Error: Data name 'A-OK' does not reference a 01- or 77-level item in the
              DATA DIVISION |}];;
