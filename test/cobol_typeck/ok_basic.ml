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

let%expect_test "basics" =
  dotest @@ prog "prog"
    ~working_storage:{|
       01 X PIC X.
    |}
    ~local_storage:{|
       01 Y PIC X.
    |};
  [%expect {|
    prog.cob:4.7-4.18:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 X PIC X.
    ----          ^^^^^^^^^^^
       5          LOCAL-STORAGE SECTION.
       6          01 Y PIC X.
    Item definition: {
      qualname: X
      offset: 0
      size: 8
      layout: {
        elementary
        usage: {
          display
          category: ALPHANUMERIC(1)
        }
      }
    }
    prog.cob:6.7-6.18:
       3          WORKING-STORAGE SECTION.
       4          01 X PIC X.
       5          LOCAL-STORAGE SECTION.
       6 >        01 Y PIC X.
    ----          ^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    Item definition: {
      qualname: Y
      offset: 0
      size: 8
      layout: {
        elementary
        usage: {
          display
          category: ALPHANUMERIC(1)
        }
      }
    } |}];;
