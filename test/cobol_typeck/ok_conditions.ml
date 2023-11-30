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

let%expect_test "group-conditions" =
  dotest @@ prog "group-conditions"
    ~working_storage:{|
       01 X.
         88 X-1 VALUE "123".
         02 FILLER PIC X(3) VALUE "0".
       01 W.
         88 Y-1 VALUE "123".
         02 Y OCCURS 3.
           03 FILLER PIC X VALUE "0".
    |}
    ~procedure:{|
           SET X-1 TO TRUE
           DISPLAY X
           SET Y-1 TO TRUE
           DISPLAY Y (2)
           DISPLAY W.
    |};
  [%expect {|
    prog.cob:4.7-6.38:
       1          PROGRAM-ID. group-conditions.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 X.
    ----          ^^^^^
       5 >          88 X-1 VALUE "123".
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6 >          02 FILLER PIC X(3) VALUE "0".
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          01 W.
       8            88 Y-1 VALUE "123".
    Item definition: {
      qualname: X
      offset: 0
      size: 3
      layout: {
        structure
        fields: {
          filler
          offset: 0
          size: 3
          layout: {
            elementary
            usage: {
              display (dev: temporary)
              category: ALPHANUMERIC(3)
            }
          }
        }
      }
    }
    prog.cob:7.7-10.37:
       4          01 X.
       5            88 X-1 VALUE "123".
       6            02 FILLER PIC X(3) VALUE "0".
       7 >        01 W.
    ----          ^^^^^
       8 >          88 Y-1 VALUE "123".
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       9 >          02 Y OCCURS 3.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^
      10 >            03 FILLER PIC X VALUE "0".
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      11          PROCEDURE DIVISION.
      12          SET X-1 TO TRUE
    Item definition: {
      qualname: W
      offset: 0
      size: 1
      layout: {
        structure
        fields: {
          qualname: Y IN W
          offset: 0
          size: 1
          layout: {
            fixed-length table
            length: 3
            items: {
              filler
              offset: 0
              size: 1
              layout: {
                elementary
                usage: {
                  display (dev: temporary)
                  category: ALPHANUMERIC(1)
                }
              }
            }
          }
        }
      }
    } |}];;
