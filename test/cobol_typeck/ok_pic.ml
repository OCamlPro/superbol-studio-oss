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

let%expect_test "numeric-pic-for-bin-usage" =
  dotest @@ prog "numeric-pic-for-bin-usage"
    ~working_storage:{|
       77 A PIC 99 USAGE BINARY.
       77 B PIC S9(6) USAGE COMP.
       77 C PIC S9(5)V9(2) USAGE PACKED-DECIMAL.
    |};
  [%expect {|
    prog.cob:4.7-4.32:
       1          PROGRAM-ID. numeric-pic-for-bin-usage.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A PIC 99 USAGE BINARY.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^
       5          77 B PIC S9(6) USAGE COMP.
       6          77 C PIC S9(5)V9(2) USAGE PACKED-DECIMAL.
    Item definition: {
      qualname: A
      offset: 0
      size: 2
      layout: {
        elementary
        usage: {
          display (dev: temporary)
          category: NUMERIC(digits = 2, scale = 0, with_sign = false)
        }
      }
    }
    prog.cob:5.7-5.33:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A PIC 99 USAGE BINARY.
       5 >        77 B PIC S9(6) USAGE COMP.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^
       6          77 C PIC S9(5)V9(2) USAGE PACKED-DECIMAL.
       7          PROCEDURE DIVISION.
    Item definition: {
      qualname: B
      offset: 0
      size: 6
      layout: {
        elementary
        usage: {
          display (dev: temporary)
          category: NUMERIC(digits = 6, scale = 0, with_sign = true)
        }
      }
    }
    prog.cob:6.7-6.48:
       3          WORKING-STORAGE SECTION.
       4          77 A PIC 99 USAGE BINARY.
       5          77 B PIC S9(6) USAGE COMP.
       6 >        77 C PIC S9(5)V9(2) USAGE PACKED-DECIMAL.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    Item definition: {
      qualname: C
      offset: 0
      size: 7
      layout: {
        elementary
        usage: {
          display (dev: temporary)
          category: NUMERIC(digits = 7, scale = 2, with_sign = true)
        }
      }
    } |}];;
