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

let%expect_test "simple-conditions" =
  dotest @@ prog "simple-conditions"
    ~working_storage:{|
       77 X PIC X.
       88 X-IS-A VALUE "A".
    |};
  [%expect {|
    prog.cob:4.7-4.18:
       1          PROGRAM-ID. simple-conditions.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 X PIC X.
    ----          ^^^^^^^^^^^
       5          88 X-IS-A VALUE "A".
       6          PROCEDURE DIVISION.
    Item definition: {
      qualname: X
      offset: 0
      size: 1
      layout: {
        elementary
        usage: {
          display (dev: temporary)
          category: ALPHANUMERIC(1)
        }
      }
      conditions: {
        qualname: X-IS-A IN X
        values: ...
      }
    } |}];;


let%expect_test "qualified-conditions" =
  dotest @@ prog "simple-conditions"
    ~working_storage:{|
       01 W.
         02 X PIC X VALUE "X".
         88 X-IS-A VALUE "A".
         88 X-IS-B VALUE "B".
    |}
    ~procedure:{|
           DISPLAY X
           SET X-IS-A IN X IN W TO TRUE
           DISPLAY X
           SET X-IS-B TO TRUE
           DISPLAY X.
    |};
  [%expect {|
    prog.cob:4.7-5.30:
       1          PROGRAM-ID. simple-conditions.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 W.
    ----          ^^^^^
       5 >          02 X PIC X VALUE "X".
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6            88 X-IS-A VALUE "A".
       7            88 X-IS-B VALUE "B".
    Item definition: {
      qualname: W
      offset: 0
      size: 1
      layout: {
        structure
        fields: {
          qualname: X IN W
          offset: 0
          size: 1
          layout: {
            elementary
            usage: {
              display (dev: temporary)
              category: ALPHANUMERIC(1)
            }
          }
          conditions: {
            qualname: X-IS-A IN X IN W
            values: ...
          }{
            qualname: X-IS-B IN X IN W
            values: ...
          }
        }
      }
    } |}];;


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
      conditions: {
        qualname: X-1 IN X
        values: ...
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
      conditions: {
        qualname: Y-1 IN W
        values: ...
      }
    } |}];;
