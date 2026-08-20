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

let%expect_test "multi-values" =
  dotest @@ prog "multi-values"
    ~working_storage:{|
       78 MISSING.
       78 WRONG PIC 9 VALUE "A".
       78 MISSING-N-WRONG PIC X.
       78 MULT PIC A VALUE "A" "B".
       78 GLOB GLOBAL VALUE "GLOBAL".
    |};
  [%expect {|
    prog.cob:4.7-4.17:
       1          PROGRAM-ID. multi-values.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        78 MISSING.
    ----          ^^^^^^^^^^
       5          78 WRONG PIC 9 VALUE "A".
       6          78 MISSING-N-WRONG PIC X.
    >> Error: Missing value for 78-level data item `MISSING'

    prog.cob:5.16-5.21:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          78 MISSING.
       5 >        78 WRONG PIC 9 VALUE "A".
    ----                   ^^^^^
       6          78 MISSING-N-WRONG PIC X.
       7          78 MULT PIC A VALUE "A" "B".
    >> Error: Unexpected clause for 78-level data item

    prog.cob:6.7-6.31:
       3          WORKING-STORAGE SECTION.
       4          78 MISSING.
       5          78 WRONG PIC 9 VALUE "A".
       6 >        78 MISSING-N-WRONG PIC X.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^
       7          78 MULT PIC A VALUE "A" "B".
       8          78 GLOB GLOBAL VALUE "GLOBAL".
    >> Error: Missing value for 78-level data item `MISSING-N-WRONG'

    prog.cob:6.26-6.31:
       3          WORKING-STORAGE SECTION.
       4          78 MISSING.
       5          78 WRONG PIC 9 VALUE "A".
       6 >        78 MISSING-N-WRONG PIC X.
    ----                             ^^^^^
       7          78 MULT PIC A VALUE "A" "B".
       8          78 GLOB GLOBAL VALUE "GLOBAL".
    >> Error: Unexpected clause for 78-level data item

    prog.cob:7.15-7.20:
       4          78 MISSING.
       5          78 WRONG PIC 9 VALUE "A".
       6          78 MISSING-N-WRONG PIC X.
       7 >        78 MULT PIC A VALUE "A" "B".
    ----                  ^^^^^
       8          78 GLOB GLOBAL VALUE "GLOBAL".
       9          PROCEDURE DIVISION.
    >> Error: Unexpected clause for 78-level data item

    prog.cob:7.21-7.34:
       4          78 MISSING.
       5          78 WRONG PIC 9 VALUE "A".
       6          78 MISSING-N-WRONG PIC X.
       7 >        78 MULT PIC A VALUE "A" "B".
    ----                        ^^^^^^^^^^^^^
       8          78 GLOB GLOBAL VALUE "GLOBAL".
       9          PROCEDURE DIVISION.
    >> Error: Unexpected multiple values for 78-level data item `MULT'

    prog.cob:7.34-7.35:
       4          78 MISSING.
       5          78 WRONG PIC 9 VALUE "A".
       6          78 MISSING-N-WRONG PIC X.
       7 >        78 MULT PIC A VALUE "A" "B".
    ----                                     ^
       8          78 GLOB GLOBAL VALUE "GLOBAL".
       9          PROCEDURE DIVISION.
    >> Error: Invalid syntax

    prog.cob:7.34:
       4          78 MISSING.
       5          78 WRONG PIC 9 VALUE "A".
       6          78 MISSING-N-WRONG PIC X.
       7 >        78 MULT PIC A VALUE "A" "B".
    ----                                     ^
       8          78 GLOB GLOBAL VALUE "GLOBAL".
       9          PROCEDURE DIVISION.
    >> Hint: Missing FROM <subscripts> |}];;
