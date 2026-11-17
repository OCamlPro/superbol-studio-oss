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

let mf_binary_sizing_options =
  let default_options = Prog_parser.options () in
  { default_options with
    config =
      (module (struct
        include (val default_options.config)
        let binary_size = Cobol_config.Options.binary_size#from_val ~config B_1__8
      end)) }

let%expect_test "usage-binary" =
  dotest @@ prog "usage-binary"
    ~working_storage:{|
       77 A1 PIC 9 BINARY.
       77 AX PIC 9 BINARY-CHAR.
    |};
  [%expect {|
    prog.cob:5.17-5.18:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A1 PIC 9 BINARY.
       5 >        77 AX PIC 9 BINARY-CHAR.
    ----                    ^
       6          PROCEDURE DIVISION.
       7
    >> Warning: Ignored PICTURE clause given for item with USAGE BINARY-CHAR

    prog.cob:4.7-4.26:
       1          PROGRAM-ID. usage-binary.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A1 PIC 9 BINARY.
    ----          ^^^^^^^^^^^^^^^^^^^
       5          77 AX PIC 9 BINARY-CHAR.
       6          PROCEDURE DIVISION.
    Item definition: {
      qualname: A1
      offset: 0
      size: 8
      size-max: 8
      layout: {
        elementary
        usage: {
          binary
          category: NUMERIC(digits = 1, scale = 0, signed = false)
        }
      }
    }
    prog.cob:5.7-5.31:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A1 PIC 9 BINARY.
       5 >        77 AX PIC 9 BINARY-CHAR.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    Item definition: {
      qualname: AX
      /!\ with_errors /!\
      offset: 0
      size: 8
      size-max: 8
      layout: {
        elementary
        usage: {
          binary-char
        }
      }
    } |}];;

let%expect_test "usage-binary-mf" =
  dotest ~parser_options:mf_binary_sizing_options @@ prog "usage-binary-mf"
    ~working_storage:{|
       77 A1 PIC 9 BINARY.
       77 AX PIC 9 BINARY-CHAR.
    |};
  [%expect {|
    prog.cob:5.17-5.18:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A1 PIC 9 BINARY.
       5 >        77 AX PIC 9 BINARY-CHAR.
    ----                    ^
       6          PROCEDURE DIVISION.
       7
    >> Warning: Ignored PICTURE clause given for item with USAGE BINARY-CHAR

    prog.cob:4.7-4.26:
       1          PROGRAM-ID. usage-binary-mf.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A1 PIC 9 BINARY.
    ----          ^^^^^^^^^^^^^^^^^^^
       5          77 AX PIC 9 BINARY-CHAR.
       6          PROCEDURE DIVISION.
    Item definition: {
      qualname: A1
      offset: 0
      size: 8
      size-max: 8
      layout: {
        elementary
        usage: {
          binary
          category: NUMERIC(digits = 1, scale = 0, signed = false)
        }
      }
    }
    prog.cob:5.7-5.31:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A1 PIC 9 BINARY.
       5 >        77 AX PIC 9 BINARY-CHAR.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    Item definition: {
      qualname: AX
      /!\ with_errors /!\
      offset: 0
      size: 8
      size-max: 8
      layout: {
        elementary
        usage: {
          binary-char
        }
      }
    } |}];;

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
      size-max: size-of-index
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
      size-max: (* 2 size-of-index)
      layout: {
        structure
        fields: {
          qualname: C
          offset: 0
          size: size-of-index
          size-max: size-of-index
          layout: {
            elementary
            usage: index
          }
        }{
          qualname: D
          offset: size-of-index
          size: size-of-index
          size-max: size-of-index
          layout: {
            elementary
            usage: index
            value: 0
          }
        }
      }
    } |}];;

let%expect_test "usage-index-mf" =
  dotest ~parser_options:mf_binary_sizing_options @@ prog "usage-index-mf"
    ~working_storage:{|
       77 A INDEX.
       01 FILLER INDEX.
         02 C.
         02 D VALUE 0.
    |};
  [%expect {|
    prog.cob:4.7-4.18:
       1          PROGRAM-ID. usage-index-mf.
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
      size-max: size-of-index
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
      size-max: (* 2 size-of-index)
      layout: {
        structure
        fields: {
          qualname: C
          offset: 0
          size: size-of-index
          size-max: size-of-index
          layout: {
            elementary
            usage: index
          }
        }{
          qualname: D
          offset: size-of-index
          size: size-of-index
          size-max: size-of-index
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
      size-max: size-of-C-float
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
      size-max: size-of-C-double
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
      size: 8
      size-max: 8
      layout: {
        elementary
        usage: {
          binary-char
          category: NUMERIC(digits = 1, scale = 0, signed = false)
        }
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
      size-max: 16
      layout: {
        elementary
        usage: {
          binary-short
          category: NUMERIC(digits = 4, scale = 0, signed = false)
        }
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
      size: 32
      size-max: 32
      layout: {
        elementary
        usage: {
          binary-long
          category: NUMERIC(digits = 8, scale = 0, signed = false)
        }
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
      size-max: 64
      layout: {
        elementary
        usage: {
          binary-double
          category: NUMERIC(digits = 18, scale = 0, signed = false)
        }
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
      size: 8
      size-max: 8
      layout: {
        elementary
        usage: {
          binary-char
          category: NUMERIC(digits = 1, scale = 0, signed = true)
        }
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
      size-max: 16
      layout: {
        elementary
        usage: {
          binary-short
          category: NUMERIC(digits = 4, scale = 0, signed = true)
        }
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
      size: 32
      size-max: 32
      layout: {
        elementary
        usage: {
          binary-long
          category: NUMERIC(digits = 8, scale = 0, signed = true)
        }
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
      size-max: 64
      layout: {
        elementary
        usage: {
          binary-double
          category: NUMERIC(digits = 18, scale = 0, signed = true)
        }
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
      size-max: 16
      layout: {
        elementary
        usage: {
          binary-short
          category: NUMERIC(digits = 4, scale = 2, signed = true)
        }
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
      size: 32
      size-max: 32
      layout: {
        elementary
        usage: {
          binary-long
          category: NUMERIC(digits = 8, scale = 1, signed = true)
        }
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
      size-max: 64
      layout: {
        elementary
        usage: {
          binary-double
          category: NUMERIC(digits = 18, scale = 8, signed = true)
        }
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
      size-max: 64
      layout: {
        elementary
        usage: {
          binary-double
          category: NUMERIC(digits = 18, scale = 17, signed = true)
        }
      }
    } |}];;

let%expect_test "usage-comps-mf" =
  dotest ~parser_options:mf_binary_sizing_options @@ prog "usage-comps-mf"
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
       1          PROGRAM-ID. usage-comps-mf.
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
      size-max: size-of-C-float
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
      size-max: size-of-C-double
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
      size: 8
      size-max: 8
      layout: {
        elementary
        usage: {
          binary-char
          category: NUMERIC(digits = 1, scale = 0, signed = false)
        }
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
      size-max: 16
      layout: {
        elementary
        usage: {
          binary-short
          category: NUMERIC(digits = 4, scale = 0, signed = false)
        }
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
      size: 32
      size-max: 32
      layout: {
        elementary
        usage: {
          binary-long
          category: NUMERIC(digits = 8, scale = 0, signed = false)
        }
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
      size-max: 64
      layout: {
        elementary
        usage: {
          binary-double
          category: NUMERIC(digits = 18, scale = 0, signed = false)
        }
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
      size: 8
      size-max: 8
      layout: {
        elementary
        usage: {
          binary-char
          category: NUMERIC(digits = 1, scale = 0, signed = true)
        }
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
      size-max: 16
      layout: {
        elementary
        usage: {
          binary-short
          category: NUMERIC(digits = 4, scale = 0, signed = true)
        }
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
      size: 32
      size-max: 32
      layout: {
        elementary
        usage: {
          binary-long
          category: NUMERIC(digits = 8, scale = 0, signed = true)
        }
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
      size-max: 64
      layout: {
        elementary
        usage: {
          binary-double
          category: NUMERIC(digits = 18, scale = 0, signed = true)
        }
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
      size-max: 16
      layout: {
        elementary
        usage: {
          binary-short
          category: NUMERIC(digits = 4, scale = 2, signed = true)
        }
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
      size: 32
      size-max: 32
      layout: {
        elementary
        usage: {
          binary-long
          category: NUMERIC(digits = 8, scale = 1, signed = true)
        }
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
      size-max: 64
      layout: {
        elementary
        usage: {
          binary-double
          category: NUMERIC(digits = 18, scale = 8, signed = true)
        }
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
      size-max: 64
      layout: {
        elementary
        usage: {
          binary-double
          category: NUMERIC(digits = 18, scale = 17, signed = true)
        }
      }
    } |}];;

let%expect_test "usage-comp-3-group" =
  dotest @@ prog "usage-comp-3-group"
    ~working_storage:{|
       01 G COMP-3.
          05 A PIC S9(9)V99 VALUE 0.
    |};
  [%expect {|
    prog.cob:4.7-5.36:
       1          PROGRAM-ID. usage-comp-3-group.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 G COMP-3.
    ----          ^^^^^^^^^^^^
       5 >           05 A PIC S9(9)V99 VALUE 0.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    Item definition: {
      qualname: G
      offset: 0
      size: 48
      size-max: 48
      layout: {
        structure
        fields: {
          qualname: A IN G
          offset: 0
          size: 48
          size-max: 48
          layout: {
            elementary
            usage: {
              packed-decimal
              category: NUMERIC(digits = 11, scale = 2, signed = true)
            }
            value: 0
          }
        }
      }
    } |}];;

let native_byteorder_options =
  let default_options = Prog_parser.options () in
  { default_options with
    config =
      (module (struct
        include (val default_options.config)
        let binary_byteorder =
          Cobol_config.Options.binary_byteorder#from_val ~config Native
      end)) }

(** Shows the byte order of each binary item. *)
let show_byte_orders ?parser_options contents =
  Prog_typeck.typeck ?parser_options contents |>
  Cobol_common.Diagnostics.show_n_forget ~set_status:false ~ppf:Fmt.stdout
    ~platform:Prog_common.platform |>
  begin fun Cobol_typeck.Outputs.{ group; _ } ->
    Cobol_unit.Visitor.fold_unit_group object
      inherit [unit] Cobol_unit.Visitor.folder
      method! fold_item_definition' { payload = def; _ } () =
        Cobol_common.Visitor.skip_children @@
        match def with
        | Field { field_qualname = Some name;
                  field_layout = Elementary_field {
                      usage = Binary { byte_order; _ }; _ }; _ } ->
            Pretty.out "%a: %s@." Cobol_ptree.pp_qualname' name
              (match byte_order with
               | Big_endian -> "big-endian"
               | Little_endian -> "little-endian")
        | _ ->
            ()
    end group ()
  end

let byte_order_items = {|
       77 A PIC 9(4) COMP.
       77 B PIC 9(4) BINARY.
       77 C PIC 9(4) COMP-5.
       77 D PIC X(2) COMP-5.
       77 E BINARY-LONG.
       77 F PIC 9(4) COMP-X.
       77 G PIC X(2) COMP-X.
       77 H PIC 9(4) COMP-N.
       77 I PIC X(2) COMP-N.
|}

let%expect_test "byte-order-big-endian" =
  show_byte_orders @@ prog "byte-order" ~working_storage:byte_order_items;
  [%expect {|
    A: big-endian
    B: big-endian
    C: little-endian
    D: little-endian
    E: little-endian
    F: big-endian
    G: big-endian
    H: big-endian
    I: big-endian |}];;

let%expect_test "byte-order-native" =
  show_byte_orders ~parser_options:native_byteorder_options @@
  prog "byte-order" ~working_storage:byte_order_items;
  [%expect {|
    A: little-endian
    B: little-endian
    C: little-endian
    D: little-endian
    E: little-endian
    F: little-endian
    G: little-endian
    H: big-endian
    I: big-endian |}];;

(** Shows the layout of each binary item. *)
let show_binary_sizes ?parser_options contents =
  Prog_typeck.typeck ?parser_options contents |>
  Cobol_common.Diagnostics.show_n_forget ~set_status:false ~ppf:Fmt.stdout
    ~platform:Prog_common.platform |>
  begin fun Cobol_typeck.Outputs.{ group; _ } ->
    Cobol_unit.Visitor.fold_unit_group object
      inherit [unit] Cobol_unit.Visitor.folder
      method! fold_item_definition' { payload = def; _ } () =
        Cobol_common.Visitor.skip_children @@
        match def with
        | Field { field_qualname = Some name;
                  field_layout = Elementary_field {
                      usage = Binary { signed; scaling; byte_size;
                                       truncation; _ }; _ }; _ } ->
            Pretty.out "%a: bytes=%d signed=%b scaling=%d truncation=%s@."
              Cobol_ptree.pp_qualname' name
              (match byte_size with
               | Byte_size -> 1 | Short_size -> 2 | Long_size -> 4
               | Double_size -> 8 | Long_double_size -> 16
               | C_long_size -> -1 | Custom_size n -> n)
              signed scaling
              (match truncation with
               | Truncate_to_digits { digits } -> Fmt.str "digits(%d)" digits
               | Truncate_to_native_size -> "native")
        | _ ->
            ()
    end group ()
  end

let%expect_test "usage-comp-x-n" =
  show_binary_sizes @@ prog "usage-comp-x-n"
    ~working_storage:{|
       77 X1 PIC 9 COMP-X.
       77 X2 PIC 9(2) COMP-X.
       77 X3 PIC 9(3) COMP-X.
       77 X5 PIC 9(5) COMP-X.
       77 X8 PIC 9(8) COMP-X.
       77 X10 PIC 9(10) COMP-X.
       77 X18 PIC 9(18) COMP-X.
       77 X19 PIC 9(19) COMP-X.
       77 X36 PIC 9(36) COMP-X.
       77 SX4 PIC S9(2)V9(2) COMP-X.
       77 AX1 PIC X COMP-X.
       77 AX3 PIC X(3) COMP-X.
       77 AX8 PIC X(8) COMP-X.
       77 AX9 PIC X(9) COMP-X.
       77 N4 PIC 9(4) COMP-N.
       77 AN2 PIC X(2) COMP-N.
    |};
  [%expect {|
    X1: bytes=1 signed=false scaling=0 truncation=digits(1)
    X2: bytes=1 signed=false scaling=0 truncation=digits(2)
    X3: bytes=2 signed=false scaling=0 truncation=digits(3)
    X5: bytes=3 signed=false scaling=0 truncation=digits(5)
    X8: bytes=4 signed=false scaling=0 truncation=digits(8)
    X10: bytes=5 signed=false scaling=0 truncation=digits(10)
    X18: bytes=8 signed=false scaling=0 truncation=digits(18)
    X19: bytes=8 signed=false scaling=0 truncation=digits(19)
    X36: bytes=15 signed=false scaling=0 truncation=digits(36)
    SX4: bytes=2 signed=true scaling=2 truncation=digits(4)
    AX1: bytes=1 signed=false scaling=0 truncation=native
    AX3: bytes=3 signed=false scaling=0 truncation=native
    AX8: bytes=8 signed=false scaling=0 truncation=native
    AX9: bytes=15 signed=false scaling=0 truncation=native
    N4: bytes=2 signed=false scaling=0 truncation=digits(4)
    AN2: bytes=2 signed=false scaling=0 truncation=native |}];;

let%expect_test "usage-comp-x-errors" =
  show_binary_sizes @@ prog "usage-comp-x-errors"
    ~working_storage:{|
       77 X COMP-X.
       77 N PIC N(2) COMP-N.
    |};
  [%expect {|
    prog.cob:4.7-4.19:
       1          PROGRAM-ID. usage-comp-x-errors.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 X COMP-X.
    ----          ^^^^^^^^^^^^
       5          77 N PIC N(2) COMP-N.
       6          PROCEDURE DIVISION.
    >> Error: Missing PICTURE clause for item with USAGE COMP-X

    prog.cob:5.16-5.20:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 X COMP-X.
       5 >        77 N PIC N(2) COMP-N.
    ----                   ^^^^
       6          PROCEDURE DIVISION.
       7
    >> Error: PICTURE of category national is incompatible with USAGE
              COMP-N; expected a PICTURE for numeric or alphanumeric data item

    X: bytes=1 signed=false scaling=0 truncation=native
    N: bytes=4 signed=false scaling=0 truncation=native |}];;

let options_with_dialect_ancestry ancestry =
  let default_options = Prog_parser.options () in
  { default_options with
    config =
      (module (struct
        include (val default_options.config)
        let dialect = List.hd (List.rev ancestry)
        let dialect_ancestry = ancestry
      end)) }

let%expect_test "usage-comp-x-alphanum-mf" =
  show_binary_sizes
    ~parser_options:(options_with_dialect_ancestry
                       Cobol_config.DIALECT.[mf_strict; mf]) @@
  prog "usage-comp-x-alphanum-mf"
    ~working_storage:{|
       77 AX8 PIC X(8) COMP-X.
       77 AX9 PIC X(9) COMP-X.
       77 AN12 PIC X(12) COMP-N.
    |};
  [%expect {|
    prog.cob:5.18-5.22:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 AX8 PIC X(8) COMP-X.
       5 >        77 AX9 PIC X(9) COMP-X.
    ----                     ^^^^
       6          77 AN12 PIC X(12) COMP-N.
       7          PROCEDURE DIVISION.
    >> Warning: PICTURE of 9 characters for item with USAGE COMP-X exceeds the
                limit of 8 characters of dialect mf; its size is 15 bytes, as in
                GnuCOBOL

    prog.cob:6.19-6.24:
       3          WORKING-STORAGE SECTION.
       4          77 AX8 PIC X(8) COMP-X.
       5          77 AX9 PIC X(9) COMP-X.
       6 >        77 AN12 PIC X(12) COMP-N.
    ----                      ^^^^^
       7          PROCEDURE DIVISION.
       8
    >> Warning: PICTURE of 12 characters for item with USAGE COMP-N exceeds the
                limit of 8 characters of dialect mf; its size is 15 bytes, as in
                GnuCOBOL

    AX8: bytes=8 signed=false scaling=0 truncation=native
    AX9: bytes=15 signed=false scaling=0 truncation=native
    AN12: bytes=15 signed=false scaling=0 truncation=native |}];;

let%expect_test "usage-comp-x-alphanum-mf-derived" =
  show_binary_sizes
    ~parser_options:(options_with_dialect_ancestry
                       Cobol_config.DIALECT.[mf; Custom ("my-mf",
                                                         { strict = false })]) @@
  prog "usage-comp-x-alphanum-mf-derived"
    ~working_storage:{|
       77 AX9 PIC X(9) COMP-X.
    |};
  [%expect {|
    prog.cob:4.18-4.22:
       1          PROGRAM-ID. usage-comp-x-alphanum-mf-derived.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 AX9 PIC X(9) COMP-X.
    ----                     ^^^^
       5          PROCEDURE DIVISION.
       6
    >> Warning: PICTURE of 9 characters for item with USAGE COMP-X exceeds the
                limit of 8 characters of dialect mf, from which my-mf derives;
                its size is 15 bytes, as in GnuCOBOL

    AX9: bytes=15 signed=false scaling=0 truncation=native |}];;
