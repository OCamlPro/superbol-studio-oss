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
       78 CONST VALUE "A".
       77 A     PIC A VALUE CONST.
    |};
  [%expect {|
    prog.cob:5.7-5.34:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          78 CONST VALUE "A".
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

let%expect_test "boolean-value" =
  dotest @@ prog "prog"
    ~working_storage:{|
       78 F VALUE B"1010".
       77 X VALUE F.
       78 G VALUE B"".
       77 Y VALUE G.
    |};
  [%expect {|
    prog.cob:5.7-5.20:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          78 F VALUE B"1010".
       5 >        77 X VALUE F.
    ----          ^^^^^^^^^^^^^
       6          78 G VALUE B"".
       7          77 Y VALUE G.
    Item definition: {
      qualname: X
      offset: 0
      size: 32
      layout: {
        elementary
        usage: {
          display
          category: BOOLEAN(4)
        }
        value: b"1010"
      }
    }
    prog.cob:7.7-7.20:
       4          78 F VALUE B"1010".
       5          77 X VALUE F.
       6          78 G VALUE B"".
       7 >        77 Y VALUE G.
    ----          ^^^^^^^^^^^^^
       8          PROCEDURE DIVISION.
       9
    Item definition: {
      qualname: Y
      offset: 0
      size: 0
      layout: {
        elementary
        usage: {
          display
          category: BOOLEAN(0)
        }
        value: b""
      }
    } |}];;

let%expect_test "hexadecimal-value" =
  dotest @@ prog "prog"
    ~working_storage:{|
       78 F VALUE  X"68656C6C6F20776F726C64".
       78 G VALUE X"a68656C6C6F20776F726C64".
       77 X VALUE F.
       77 Y VALUE G.
    |};
  [%expect {|
    prog.cob:6.7-6.20:
       3          WORKING-STORAGE SECTION.
       4          78 F VALUE  X"68656C6C6F20776F726C64".
       5          78 G VALUE X"a68656C6C6F20776F726C64".
       6 >        77 X VALUE F.
    ----          ^^^^^^^^^^^^^
       7          77 Y VALUE G.
       8          PROCEDURE DIVISION.
    Item definition: {
      qualname: X
      offset: 0
      size: 88
      layout: {
        elementary
        usage: {
          display
          category: ALPHANUMERIC(11)
        }
        value: X"68656C6C6F20776F726C64"/"hello world"
      }
    }
    prog.cob:7.7-7.20:
       4          78 F VALUE  X"68656C6C6F20776F726C64".
       5          78 G VALUE X"a68656C6C6F20776F726C64".
       6          77 X VALUE F.
       7 >        77 Y VALUE G.
    ----          ^^^^^^^^^^^^^
       8          PROCEDURE DIVISION.
       9
    Item definition: {
      qualname: Y
      offset: 0
      size: 96
      layout: {
        elementary
        usage: {
          display
          category: ALPHANUMERIC(12)
        }
        value: X"a68656C6C6F20776F726C64"/"\nhello world"
      }
    } |}];;
