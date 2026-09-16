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

let%expect_test "usage-index" =
  dotest @@ prog "usage-index"
    ~working_storage:{|
       77 A INDEX.
       01 FILLER INDEX.
         02 C.
         02 D VALUE 0.
    |};
  [%expect {|
    prog.cob:4.7-4.18:
       1          PROGRAM-ID. usage-index.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A INDEX.
    ----          ^^^^^^^^^^^
       5          01 FILLER INDEX.
       6            02 C.
    Item definition: {
      qualname: A
      offset: 0
      size: size-of-index
      layout: {
        elementary
        usage: index
      }
    }
    prog.cob:5.7-7.22:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A INDEX.
       5 >        01 FILLER INDEX.
    ----          ^^^^^^^^^^^^^^^^
       6 >          02 C.
    ----  ^^^^^^^^^^^^^^^
       7 >          02 D VALUE 0.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^
       8          PROCEDURE DIVISION.
       9
    Item definition: {
      filler
      offset: 0
      size: (* 2 size-of-index)
      layout: {
        structure
        fields: {
          qualname: C
          offset: 0
          size: size-of-index
          layout: {
            elementary
            usage: index
          }
        }{
          qualname: D
          offset: size-of-index
          size: size-of-index
          layout: {
            elementary
            usage: index
            value: 0
          }
        }
      }
    } |}];;

let%expect_test "usage-comps" =
  dotest @@ prog "usage-comps"
    ~working_storage:{|
       77 A1 COMP-1.
       77 A2 COMP-2.
       77 A5 COMP-5 PIC 9.
       77 B5 COMP-5 PIC 9(4).
       77 C5 COMP-5 PIC 9(8).
       77 D5 COMP-5 PIC 9(18).
       77 SA5 COMP-5 PIC S9.
       77 SB5 COMP-5 PIC S9(4).
       77 SC5 COMP-5 PIC S9(8).
       77 SD5 COMP-5 PIC S9(18).
       77 SVB5 COMP-5 PIC S9(2)V9(2).
       77 SVC5 COMP-5 PIC S9(7)V9(1).
       77 SVD5 COMP-5 PIC S9(10)V9(8).
       77 SVD5 COMP-5 PIC S9(1)V9(17).
    |};
  [%expect {|
    prog.cob:4.7-4.20:
       1          PROGRAM-ID. usage-comps.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A1 COMP-1.
    ----          ^^^^^^^^^^^^^
       5          77 A2 COMP-2.
       6          77 A5 COMP-5 PIC 9.
    Item definition: {
      qualname: A1
      offset: 0
      size: size-of-C-float
      layout: {
        elementary
        usage: float-short (float)
      }
    }
    prog.cob:5.7-5.20:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A1 COMP-1.
       5 >        77 A2 COMP-2.
    ----          ^^^^^^^^^^^^^
       6          77 A5 COMP-5 PIC 9.
       7          77 B5 COMP-5 PIC 9(4).
    Item definition: {
      qualname: A2
      offset: 0
      size: size-of-C-double
      layout: {
        elementary
        usage: float-long (double)
      }
    }
    prog.cob:6.7-6.26:
       3          WORKING-STORAGE SECTION.
       4          77 A1 COMP-1.
       5          77 A2 COMP-2.
       6 >        77 A5 COMP-5 PIC 9.
    ----          ^^^^^^^^^^^^^^^^^^^
       7          77 B5 COMP-5 PIC 9(4).
       8          77 C5 COMP-5 PIC 9(8).
    Item definition: {
      qualname: A5
      offset: 0
      size: 16
      layout: {
        elementary
        usage: binary-short(range-extended)
      }
    }
    prog.cob:7.7-7.29:
       4          77 A1 COMP-1.
       5          77 A2 COMP-2.
       6          77 A5 COMP-5 PIC 9.
       7 >        77 B5 COMP-5 PIC 9(4).
    ----          ^^^^^^^^^^^^^^^^^^^^^^
       8          77 C5 COMP-5 PIC 9(8).
       9          77 D5 COMP-5 PIC 9(18).
    Item definition: {
      qualname: B5
      offset: 0
      size: 16
      layout: {
        elementary
        usage: binary-short(range-extended)
      }
    }
    prog.cob:8.7-8.29:
       5          77 A2 COMP-2.
       6          77 A5 COMP-5 PIC 9.
       7          77 B5 COMP-5 PIC 9(4).
       8 >        77 C5 COMP-5 PIC 9(8).
    ----          ^^^^^^^^^^^^^^^^^^^^^^
       9          77 D5 COMP-5 PIC 9(18).
      10          77 SA5 COMP-5 PIC S9.
    Item definition: {
      qualname: C5
      offset: 0
      size: 64
      layout: {
        elementary
        usage: binary-double(range-extended)
      }
    }
    prog.cob:9.7-9.30:
       6          77 A5 COMP-5 PIC 9.
       7          77 B5 COMP-5 PIC 9(4).
       8          77 C5 COMP-5 PIC 9(8).
       9 >        77 D5 COMP-5 PIC 9(18).
    ----          ^^^^^^^^^^^^^^^^^^^^^^^
      10          77 SA5 COMP-5 PIC S9.
      11          77 SB5 COMP-5 PIC S9(4).
    Item definition: {
      qualname: D5
      offset: 0
      size: 64
      layout: {
        elementary
        usage: binary-double(range-extended)
      }
    }
    prog.cob:10.7-10.28:
       7          77 B5 COMP-5 PIC 9(4).
       8          77 C5 COMP-5 PIC 9(8).
       9          77 D5 COMP-5 PIC 9(18).
      10 >        77 SA5 COMP-5 PIC S9.
    ----          ^^^^^^^^^^^^^^^^^^^^^
      11          77 SB5 COMP-5 PIC S9(4).
      12          77 SC5 COMP-5 PIC S9(8).
    Item definition: {
      qualname: SA5
      offset: 0
      size: 16
      layout: {
        elementary
        usage: signed-binary-short(range-extended)
      }
    }
    prog.cob:11.7-11.31:
       8          77 C5 COMP-5 PIC 9(8).
       9          77 D5 COMP-5 PIC 9(18).
      10          77 SA5 COMP-5 PIC S9.
      11 >        77 SB5 COMP-5 PIC S9(4).
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^
      12          77 SC5 COMP-5 PIC S9(8).
      13          77 SD5 COMP-5 PIC S9(18).
    Item definition: {
      qualname: SB5
      offset: 0
      size: 16
      layout: {
        elementary
        usage: signed-binary-short(range-extended)
      }
    }
    prog.cob:12.7-12.31:
       9          77 D5 COMP-5 PIC 9(18).
      10          77 SA5 COMP-5 PIC S9.
      11          77 SB5 COMP-5 PIC S9(4).
      12 >        77 SC5 COMP-5 PIC S9(8).
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^
      13          77 SD5 COMP-5 PIC S9(18).
      14          77 SVB5 COMP-5 PIC S9(2)V9(2).
    Item definition: {
      qualname: SC5
      offset: 0
      size: 64
      layout: {
        elementary
        usage: signed-binary-double(range-extended)
      }
    }
    prog.cob:13.7-13.32:
      10          77 SA5 COMP-5 PIC S9.
      11          77 SB5 COMP-5 PIC S9(4).
      12          77 SC5 COMP-5 PIC S9(8).
      13 >        77 SD5 COMP-5 PIC S9(18).
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^
      14          77 SVB5 COMP-5 PIC S9(2)V9(2).
      15          77 SVC5 COMP-5 PIC S9(7)V9(1).
    Item definition: {
      qualname: SD5
      offset: 0
      size: 64
      layout: {
        elementary
        usage: signed-binary-double(range-extended)
      }
    }
    prog.cob:14.7-14.37:
      11          77 SB5 COMP-5 PIC S9(4).
      12          77 SC5 COMP-5 PIC S9(8).
      13          77 SD5 COMP-5 PIC S9(18).
      14 >        77 SVB5 COMP-5 PIC S9(2)V9(2).
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      15          77 SVC5 COMP-5 PIC S9(7)V9(1).
      16          77 SVD5 COMP-5 PIC S9(10)V9(8).
    Item definition: {
      qualname: SVB5
      offset: 0
      size: 16
      layout: {
        elementary
        usage: signed-binary-short(range-extended)
      }
    }
    prog.cob:15.7-15.37:
      12          77 SC5 COMP-5 PIC S9(8).
      13          77 SD5 COMP-5 PIC S9(18).
      14          77 SVB5 COMP-5 PIC S9(2)V9(2).
      15 >        77 SVC5 COMP-5 PIC S9(7)V9(1).
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      16          77 SVD5 COMP-5 PIC S9(10)V9(8).
      17          77 SVD5 COMP-5 PIC S9(1)V9(17).
    Item definition: {
      qualname: SVC5
      offset: 0
      size: 64
      layout: {
        elementary
        usage: signed-binary-double(range-extended)
      }
    }
    prog.cob:16.7-16.38:
      13          77 SD5 COMP-5 PIC S9(18).
      14          77 SVB5 COMP-5 PIC S9(2)V9(2).
      15          77 SVC5 COMP-5 PIC S9(7)V9(1).
      16 >        77 SVD5 COMP-5 PIC S9(10)V9(8).
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      17          77 SVD5 COMP-5 PIC S9(1)V9(17).
      18          PROCEDURE DIVISION.
    Item definition: {
      qualname: SVD5
      offset: 0
      size: 64
      layout: {
        elementary
        usage: signed-binary-double(range-extended)
      }
    }
    prog.cob:17.7-17.38:
      14          77 SVB5 COMP-5 PIC S9(2)V9(2).
      15          77 SVC5 COMP-5 PIC S9(7)V9(1).
      16          77 SVD5 COMP-5 PIC S9(10)V9(8).
      17 >        77 SVD5 COMP-5 PIC S9(1)V9(17).
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      18          PROCEDURE DIVISION.
      19
    Item definition: {
      qualname: SVD5
      offset: 0
      size: 64
      layout: {
        elementary
        usage: signed-binary-double(range-extended)
      }
    } |}];;
