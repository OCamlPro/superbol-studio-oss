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

let%expect_test "mismatching-subordinate-usage" =
  dotest @@ prog "mismatching-subordinate-usage"
    ~working_storage:{|
       01 FILLER BINARY.
         02 X USAGE DISPLAY.
    |};
  [%expect {|
    prog.cob:5.14-5.27:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 FILLER BINARY.
       5 >          02 X USAGE DISPLAY.
    ----                 ^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    >> Warning: Mismatching USAGE DISPLAY for item 'X', subordinate to a group
                with USAGE BINARY

    prog.cob:5.9-5.28:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 FILLER BINARY.
       5 >          02 X USAGE DISPLAY.
    ----            ^^^^^^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    >> Error: Missing PICTURE clause for item 'X' |}];;


let%expect_test "pic-on-wrong-usage" =
  dotest @@ prog "pic-on-wrong-usage"
    ~working_storage:{|
       77 A PIC 99 INDEX.
       01 FILLER INDEX.
         02 B PIC X(4).
    |};
  [%expect {|
    prog.cob:4.12-4.18:
       1          PROGRAM-ID. pic-on-wrong-usage.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A PIC 99 INDEX.
    ----               ^^^^^^
       5          01 FILLER INDEX.
       6            02 B PIC X(4).
    >> Error: Unexpected PICTURE clause for item 'A' with USAGE INDEX

    prog.cob:6.14-6.22:
       3          WORKING-STORAGE SECTION.
       4          77 A PIC 99 INDEX.
       5          01 FILLER INDEX.
       6 >          02 B PIC X(4).
    ----                 ^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    >> Error: Unexpected PICTURE clause for item 'B' with USAGE INDEX |}];;
