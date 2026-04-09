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
    >> Error: Missing PICTURE clause for item 'X'

    prog.cob:4.7-5.28:
       1          PROGRAM-ID. mismatching-subordinate-usage.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 FILLER BINARY.
    ----          ^^^^^^^^^^^^^^^^^
       5 >          02 X USAGE DISPLAY.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    Item definition: {
      filler
      /!\ with_errors /!\
      offset: 0
      size: 8
      layout: {
        structure
        fields: {
          qualname: X
          /!\ with_errors /!\
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
      }
    } |}];;


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
    >> Error: Unexpected PICTURE clause for item 'B' with USAGE INDEX

    prog.cob:4.7-4.25:
       1          PROGRAM-ID. pic-on-wrong-usage.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A PIC 99 INDEX.
    ----          ^^^^^^^^^^^^^^^^^^
       5          01 FILLER INDEX.
       6            02 B PIC X(4).
    Item definition: {
      qualname: A
      /!\ with_errors /!\
      offset: 0
      size: size-of-index
      layout: {
        elementary
        usage: index
      }
    }
    prog.cob:5.7-6.23:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A PIC 99 INDEX.
       5 >        01 FILLER INDEX.
    ----          ^^^^^^^^^^^^^^^^
       6 >          02 B PIC X(4).
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    Item definition: {
      filler
      /!\ with_errors /!\
      offset: 0
      size: size-of-index
      layout: {
        structure
        fields: {
          qualname: B
          /!\ with_errors /!\
          offset: 0
          size: size-of-index
          layout: {
            elementary
            usage: index
          }
        }
      }
    } |}];;


let%expect_test "unsupported-usage" =
  dotest @@ prog "unsupported-usage"
    ~working_storage:{|
       77 A COMP-X.
       01 B.
         02 FILLER COMP-X.
       01 C.
         02 D COMP-0 OCCURS 10.
      *> CHECKME: Here it's unclear whether F's error flag should propagate to E.
       01 E PIC X.
       01 F COMP-X REDEFINES E.
    |};
  [%expect {|
    prog.cob:4.12-4.18:
       1          PROGRAM-ID. unsupported-usage.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A COMP-X.
    ----               ^^^^^^
       5          01 B.
       6            02 FILLER COMP-X.
    >> Warning: Unsupported USAGE COMP-X

    prog.cob:6.19-6.25:
       3          WORKING-STORAGE SECTION.
       4          77 A COMP-X.
       5          01 B.
       6 >          02 FILLER COMP-X.
    ----                      ^^^^^^
       7          01 C.
       8            02 D COMP-0 OCCURS 10.
    >> Warning: Unsupported USAGE COMP-X

    prog.cob:8.14-8.20:
       5          01 B.
       6            02 FILLER COMP-X.
       7          01 C.
       8 >          02 D COMP-0 OCCURS 10.
    ----                 ^^^^^^
       9         *> CHECKME: Here it's unclear whether F's error flag should propagate to E.
      10          01 E PIC X.
    >> Warning: Unsupported USAGE COMP-0

    prog.cob:11.12-11.18:
       8            02 D COMP-0 OCCURS 10.
       9         *> CHECKME: Here it's unclear whether F's error flag should propagate to E.
      10          01 E PIC X.
      11 >        01 F COMP-X REDEFINES E.
    ----               ^^^^^^
      12          PROCEDURE DIVISION.
      13
    >> Warning: Unsupported USAGE COMP-X

    prog.cob:4.7-4.19:
       1          PROGRAM-ID. unsupported-usage.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A COMP-X.
    ----          ^^^^^^^^^^^^
       5          01 B.
       6            02 FILLER COMP-X.
    Item definition: {
      qualname: A
      /!\ with_errors /!\
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
    prog.cob:5.7-6.26:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A COMP-X.
       5 >        01 B.
    ----          ^^^^^
       6 >          02 FILLER COMP-X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          01 C.
       8            02 D COMP-0 OCCURS 10.
    Item definition: {
      qualname: B
      /!\ with_errors /!\
      offset: 0
      size: 8
      layout: {
        structure
        fields: {
          filler
          /!\ with_errors /!\
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
      }
    }
    prog.cob:7.7-8.31:
       4          77 A COMP-X.
       5          01 B.
       6            02 FILLER COMP-X.
       7 >        01 C.
    ----          ^^^^^
       8 >          02 D COMP-0 OCCURS 10.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       9         *> CHECKME: Here it's unclear whether F's error flag should propagate to E.
      10          01 E PIC X.
    Item definition: {
      qualname: C
      offset: 0
      size: 80
      layout: {
        structure
        fields: {
          table
          /!\ with_errors /!\
          offset: 0
          size: 80
          range: {
            span: fixed-length: 10
          }
          field: {
            qualname: D IN C
            /!\ with_errors /!\
            leading ranges: 1
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
        }
      }
    }
    prog.cob:10.7-11.31:
       7          01 C.
       8            02 D COMP-0 OCCURS 10.
       9         *> CHECKME: Here it's unclear whether F's error flag should propagate to E.
      10 >        01 E PIC X.
    ----          ^^^^^^^^^^^
      11 >        01 F COMP-X REDEFINES E.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      12          PROCEDURE DIVISION.
      13
    Item definition: {
      qualname: E
      offset: 0
      size: 8
      layout: {
        elementary
        usage: {
          display
          category: ALPHANUMERIC(1)
        }
      }
      redefs: {
        qualname: F
        /!\ with_errors /!\
        redefines: E
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
    } |}];;
