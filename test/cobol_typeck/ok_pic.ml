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
      size: 8
      layout: {
        elementary
        usage: {
          binary
          category: NUMERIC(digits = 2, scale = 0, sign = unsigned)
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
      size: 32
      layout: {
        elementary
        usage: {
          binary
          category: NUMERIC(digits = 6, scale = 0, sign = trailing nonseparate)
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
      size: 32
      layout: {
        elementary
        usage: {
          packed-decimal
          category: NUMERIC(digits = 7, scale = 2, sign = trailing nonseparate)
        }
      }
    } |}];;

let%expect_test "signed-numeric-sizes" =
  dotest @@ prog "signed-numeric-sizes"
    ~working_storage:{|
       77 A PIC 9(5).
       77 B PIC S9(5).
       77 C PIC S9(5) SIGN IS LEADING.
       77 D PIC S9(5) SIGN IS LEADING SEPARATE.
       77 E PIC S9(5) SIGN IS TRAILING SEPARATE.
    |};
  [%expect {|
    prog.cob:4.7-4.21:
       1          PROGRAM-ID. signed-numeric-sizes.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A PIC 9(5).
    ----          ^^^^^^^^^^^^^^
       5          77 B PIC S9(5).
       6          77 C PIC S9(5) SIGN IS LEADING.
    Item definition: {
      qualname: A
      offset: 0
      size: 40
      layout: {
        elementary
        usage: {
          display
          category: NUMERIC(digits = 5, scale = 0, sign = unsigned)
        }
      }
    }
    prog.cob:5.7-5.22:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A PIC 9(5).
       5 >        77 B PIC S9(5).
    ----          ^^^^^^^^^^^^^^^
       6          77 C PIC S9(5) SIGN IS LEADING.
       7          77 D PIC S9(5) SIGN IS LEADING SEPARATE.
    Item definition: {
      qualname: B
      offset: 0
      size: 40
      layout: {
        elementary
        usage: {
          display
          category: NUMERIC(digits = 5, scale = 0, sign = trailing nonseparate)
        }
      }
    }
    prog.cob:6.7-6.38:
       3          WORKING-STORAGE SECTION.
       4          77 A PIC 9(5).
       5          77 B PIC S9(5).
       6 >        77 C PIC S9(5) SIGN IS LEADING.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          77 D PIC S9(5) SIGN IS LEADING SEPARATE.
       8          77 E PIC S9(5) SIGN IS TRAILING SEPARATE.
    Item definition: {
      qualname: C
      offset: 0
      size: 40
      layout: {
        elementary
        usage: {
          display
          category: NUMERIC(digits = 5, scale = 0, sign = leading nonseparate)
        }
      }
    }
    prog.cob:7.7-7.47:
       4          77 A PIC 9(5).
       5          77 B PIC S9(5).
       6          77 C PIC S9(5) SIGN IS LEADING.
       7 >        77 D PIC S9(5) SIGN IS LEADING SEPARATE.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       8          77 E PIC S9(5) SIGN IS TRAILING SEPARATE.
       9          PROCEDURE DIVISION.
    Item definition: {
      qualname: D
      offset: 0
      size: 48
      layout: {
        elementary
        usage: {
          display
          category: NUMERIC(digits = 5, scale = 0, sign = leading separate)
        }
      }
    }
    prog.cob:8.7-8.48:
       5          77 B PIC S9(5).
       6          77 C PIC S9(5) SIGN IS LEADING.
       7          77 D PIC S9(5) SIGN IS LEADING SEPARATE.
       8 >        77 E PIC S9(5) SIGN IS TRAILING SEPARATE.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       9          PROCEDURE DIVISION.
      10
    Item definition: {
      qualname: E
      offset: 0
      size: 48
      layout: {
        elementary
        usage: {
          display
          category: NUMERIC(digits = 5, scale = 0, sign = trailing separate)
        }
      }
    } |}];;

let%expect_test "pic-with-space-in-parens" =
  dotest @@ prog "pic-with-space-in-parens"
  ~working_storage:{|
            77 DB-REALM-NAME    PIC X( 30 ) IS EXTERNAL.
            77 DB-RECORD-NAME   PIC X( 30 ) IS EXTERNAL.
            77 DB-SET-NAME      PIC X( 30 ) IS EXTERNAL.
            77 DB-KEY-NAME      PIC X( 30 ) IS EXTERNAL.
  |};
[%expect {|
  prog.cob:4.7-4.51:
     1          PROGRAM-ID. pic-with-space-in-parens.
     2          DATA DIVISION.
     3          WORKING-STORAGE SECTION.
     4 >        77 DB-REALM-NAME    PIC X( 30 ) IS EXTERNAL.
  ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     5               77 DB-RECORD-NAME   PIC X( 30 ) IS EXTERNAL.
     6               77 DB-SET-NAME      PIC X( 30 ) IS EXTERNAL.
  Item definition: {
    qualname: DB-REALM-NAME
    offset: 0
    size: 240
    layout: {
      elementary
      usage: {
        display
        category: ALPHANUMERIC(30)
      }
    }
  }
  prog.cob:5.12-5.56:
     2          DATA DIVISION.
     3          WORKING-STORAGE SECTION.
     4          77 DB-REALM-NAME    PIC X( 30 ) IS EXTERNAL.
     5 >             77 DB-RECORD-NAME   PIC X( 30 ) IS EXTERNAL.
  ----               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     6               77 DB-SET-NAME      PIC X( 30 ) IS EXTERNAL.
     7               77 DB-KEY-NAME      PIC X( 30 ) IS EXTERNAL.
  Item definition: {
    qualname: DB-RECORD-NAME
    offset: 0
    size: 240
    layout: {
      elementary
      usage: {
        display
        category: ALPHANUMERIC(30)
      }
    }
  }
  prog.cob:6.12-6.56:
     3          WORKING-STORAGE SECTION.
     4          77 DB-REALM-NAME    PIC X( 30 ) IS EXTERNAL.
     5               77 DB-RECORD-NAME   PIC X( 30 ) IS EXTERNAL.
     6 >             77 DB-SET-NAME      PIC X( 30 ) IS EXTERNAL.
  ----               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     7               77 DB-KEY-NAME      PIC X( 30 ) IS EXTERNAL.
     8          PROCEDURE DIVISION.
  Item definition: {
    qualname: DB-SET-NAME
    offset: 0
    size: 240
    layout: {
      elementary
      usage: {
        display
        category: ALPHANUMERIC(30)
      }
    }
  }
  prog.cob:7.12-7.56:
     4          77 DB-REALM-NAME    PIC X( 30 ) IS EXTERNAL.
     5               77 DB-RECORD-NAME   PIC X( 30 ) IS EXTERNAL.
     6               77 DB-SET-NAME      PIC X( 30 ) IS EXTERNAL.
     7 >             77 DB-KEY-NAME      PIC X( 30 ) IS EXTERNAL.
  ----               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     8          PROCEDURE DIVISION.
     9
  Item definition: {
    qualname: DB-KEY-NAME
    offset: 0
    size: 240
    layout: {
      elementary
      usage: {
        display
        category: ALPHANUMERIC(30)
      }
    }
  } |}]
