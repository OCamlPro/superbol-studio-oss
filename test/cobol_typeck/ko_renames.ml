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

let%expect_test "renames-errors-1" =
  dotest @@ prog "renames-error-1"
    ~working_storage:{|
       01 B.
         02 B-1 PIC 9 OCCURS 5 TIMES.
         02 B-2 PIC 9.
         02 B-4 PIC 9.
         02 B-5 OCCURS 5 TIMES.
           03 FILLER PIC X.
       66 B-R1 RENAMES B-1. *> should error
       66 B-R2 RENAMES B-1 THRU B-2. *> should error
       66 B-R3 RENAMES B-4 THRU B-5. *> should error
    |};
  [%expect {|
    prog.cob:10.23-10.26:
       7            02 B-4 PIC 9.
       8            02 B-5 OCCURS 5 TIMES.
       9              03 FILLER PIC X.
      10 >        66 B-R1 RENAMES B-1. *> should error
    ----                          ^^^
      11          66 B-R2 RENAMES B-1 THRU B-2. *> should error
      12          66 B-R3 RENAMES B-4 THRU B-5. *> should error
    >> Error: RENAMES operand 'B-1 IN B' has or is subordinate to an OCCURS
              clause

    prog.cob:11.23-11.26:
       8            02 B-5 OCCURS 5 TIMES.
       9              03 FILLER PIC X.
      10          66 B-R1 RENAMES B-1. *> should error
      11 >        66 B-R2 RENAMES B-1 THRU B-2. *> should error
    ----                          ^^^
      12          66 B-R3 RENAMES B-4 THRU B-5. *> should error
      13          PROCEDURE DIVISION.
    >> Error: RENAMES operand 'B-1 IN B' has or is subordinate to an OCCURS
              clause

    prog.cob:12.32-12.35:
       9              03 FILLER PIC X.
      10          66 B-R1 RENAMES B-1. *> should error
      11          66 B-R2 RENAMES B-1 THRU B-2. *> should error
      12 >        66 B-R3 RENAMES B-4 THRU B-5. *> should error
    ----                                   ^^^
      13          PROCEDURE DIVISION.
      14
    >> Error: RENAMES operand 'B-5 IN B' has or is subordinate to an OCCURS
              clause
  |}];;

let%expect_test "renames-missing-target" =
  dotest @@ prog "renames-missing-target"
    ~working_storage:{|
       01 A PIC X OCCURS 5 TIMES.
       66 A-R1 RENAMES A.
    |};
  [%expect {|
    prog.cob:5.23-5.24:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 A PIC X OCCURS 5 TIMES.
       5 >        66 A-R1 RENAMES A.
    ----                          ^
       6          PROCEDURE DIVISION.
       7
    >> Error: Item 'A IN A' not found
  |}];;
