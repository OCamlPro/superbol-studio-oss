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
       77 A PIC A VALUE "A".
    |};
  [%expect {|
    prog.cob:4.7-4.28:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A PIC A VALUE "A".
    ----          ^^^^^^^^^^^^^^^^^^^^^
       5          PROCEDURE DIVISION.
       6
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

let%expect_test "77-occurs-fixed" =
  dotest @@ prog "prog"
    ~working_storage:{|
       77 X PIC X OCCURS 5 VALUE "X".
    |};
  [%expect {|
    prog.cob:4.7-4.37:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 X PIC X OCCURS 5 VALUE "X".
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       5          PROCEDURE DIVISION.
       6
    Item definition: {
      table
      offset: 0
      size: 40
      range: {
        span: fixed-length: 5
      }
      field: {
        qualname: X
        leading ranges: 1
        offset: 0
        size: 8
        layout: {
          elementary
          usage: {
            display
            category: ALPHANUMERIC(1)
          }
          value: "X"
        }
      }
    } |}];;

let%expect_test "redefines-77" =
  dotest @@ prog "prog"
    ~working_storage:{|
       77 A PIC A VALUE "A".
       77 B REDEFINES A PIC X.
    |}
    ~local_storage:{|
       77 T-LEN PIC 99 VALUE 5.
       77 T-LEN-2 REDEFINES T-LEN PIC 99.
    |};
  [%expect {|
    prog.cob:4.7-5.30:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A PIC A VALUE "A".
    ----          ^^^^^^^^^^^^^^^^^^^^^
       5 >        77 B REDEFINES A PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6          LOCAL-STORAGE SECTION.
       7          77 T-LEN PIC 99 VALUE 5.
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
      redefs: {
        qualname: B
        redefines: A
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
    prog.cob:7.7-8.41:
       4          77 A PIC A VALUE "A".
       5          77 B REDEFINES A PIC X.
       6          LOCAL-STORAGE SECTION.
       7 >        77 T-LEN PIC 99 VALUE 5.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^
       8 >        77 T-LEN-2 REDEFINES T-LEN PIC 99.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       9          PROCEDURE DIVISION.
      10
    Item definition: {
      qualname: T-LEN
      offset: 0
      size: 16
      layout: {
        elementary
        usage: {
          display
          category: NUMERIC(digits = 2, scale = 0, with_sign = false)
        }
        value: 5
      }
      redefs: {
        qualname: T-LEN-2
        redefines: T-LEN
        offset: 0
        size: 16
        layout: {
          elementary
          usage: {
            display
            category: NUMERIC(digits = 2, scale = 0, with_sign = false)
          }
        }
      }
    } |}];;
