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
