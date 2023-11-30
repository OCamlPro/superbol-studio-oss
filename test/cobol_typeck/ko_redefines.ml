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

let%expect_test "error-group-with-pic" =
  dotest @@ prog "error-group-with-pic"
    ~working_storage:{|
       01 A.
         02 A-2 PIC 9 VALUE 5.
         02 A-2 REDEFINES A-2 PIC 9.
           03 A-3 PIC 9.
    |};
  [%expect {|
    prog.cob:6.30-6.35:
       3          WORKING-STORAGE SECTION.
       4          01 A.
       5            02 A-2 PIC 9 VALUE 5.
       6 >          02 A-2 REDEFINES A-2 PIC 9.
    ----                                 ^^^^^
       7              03 A-3 PIC 9.
       8          PROCEDURE DIVISION.
    >> Error: Unexpected PICTURE clause for group item 'A-2'
  |}];;

let%expect_test "unexpected-redefines-target" =
  dotest @@ prog "unexpected-redefines-target"
    ~working_storage:{|
       01 A.
         02 A-1 PIC X.
         02 A-2 PIC 9 VALUE 5.
         02 A-24 REDEFINES A-3 PIC 9.
    |};
  [%expect {|
    prog.cob:7.27-7.30:
       4          01 A.
       5            02 A-1 PIC X.
       6            02 A-2 PIC 9 VALUE 5.
       7 >          02 A-24 REDEFINES A-3 PIC 9.
    ----                              ^^^
       8          PROCEDURE DIVISION.
       9
    >> Error: Unexpected target name 'A-3' in REDEFINES clause for item 'A-24'; expected 'A-2'
  |}];;
