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

let dotest = Typeck_testing.show_data

let%expect_test "one-77" =
  dotest @@ prog "prog"
    ~working_storage:{|
       78 CONST PIC A VALUE "A".
       77 A     PIC A VALUE CONST.
    |};
  [%expect {|
    prog.cob:4.16-4.21:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        78 CONST PIC A VALUE "A".
    ----                   ^^^^^
       5          77 A     PIC A VALUE CONST.
       6          PROCEDURE DIVISION.
    >> Error: Unexpected clause for 78-level data item

    prog.cob:5.7-5.34:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          78 CONST PIC A VALUE "A".
       5 >        77 A     PIC A VALUE CONST.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    Item definition: {
      qualname: A
      offset: 0
      size: 8
      layout: {
        elementary
        usage: {
          display
          category: ALPHABETIC(1)
        }
        value: "A"
      }
    } |}];;
