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

let%expect_test "renames-errors-1" =
  dotest @@ prog "renames-error-1"
    ~working_storage:{|
       01 B.
         02 B-1 PIC 9 OCCURS 5 TIMES.
         02 B-2 PIC 9.
         02 B-3 PIC 9.
         02 B-4 OCCURS 5 TIMES.
           03 FILLER PIC X.
       66 B-R1 RENAMES B-1. *> should error
       66 B-R2 RENAMES B-1 THRU B-2. *> should error
       66 B-R3 RENAMES B-3 THRU B-4. *> should error
       66 B-R4 RENAMES B-3 THRU B-2. *> should error
    |};
  [%expect {|
    prog.cob:10.23-10.26:
       7            02 B-3 PIC 9.
       8            02 B-4 OCCURS 5 TIMES.
       9              03 FILLER PIC X.
      10 >        66 B-R1 RENAMES B-1. *> should error
    ----                          ^^^
      11          66 B-R2 RENAMES B-1 THRU B-2. *> should error
      12          66 B-R3 RENAMES B-3 THRU B-4. *> should error
    >> Error: RENAMES operand 'B-1 IN B' has or is subordinate to an OCCURS
              clause

    prog.cob:11.23-11.26:
       8            02 B-4 OCCURS 5 TIMES.
       9              03 FILLER PIC X.
      10          66 B-R1 RENAMES B-1. *> should error
      11 >        66 B-R2 RENAMES B-1 THRU B-2. *> should error
    ----                          ^^^
      12          66 B-R3 RENAMES B-3 THRU B-4. *> should error
      13          66 B-R4 RENAMES B-3 THRU B-2. *> should error
    >> Error: RENAMES operand 'B-1 IN B' has or is subordinate to an OCCURS
              clause

    prog.cob:12.32-12.35:
       9              03 FILLER PIC X.
      10          66 B-R1 RENAMES B-1. *> should error
      11          66 B-R2 RENAMES B-1 THRU B-2. *> should error
      12 >        66 B-R3 RENAMES B-3 THRU B-4. *> should error
    ----                                   ^^^
      13          66 B-R4 RENAMES B-3 THRU B-2. *> should error
      14          PROCEDURE DIVISION.
    >> Error: RENAMES operand 'B-4 IN B' has or is subordinate to an OCCURS
              clause

    prog.cob:13.7-13.36:
      10          66 B-R1 RENAMES B-1. *> should error
      11          66 B-R2 RENAMES B-1 THRU B-2. *> should error
      12          66 B-R3 RENAMES B-3 THRU B-4. *> should error
      13 >        66 B-R4 RENAMES B-3 THRU B-2. *> should error
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      14          PROCEDURE DIVISION.
      15
    >> Error: Invalid pair of RENAMES operands: first field B-3 IN B should
              precede second field B-2 IN B

    prog.cob:4.7-9.27:
       1          PROGRAM-ID. renames-error-1.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 B.
    ----          ^^^^^
       5 >          02 B-1 PIC 9 OCCURS 5 TIMES.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6 >          02 B-2 PIC 9.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^
       7 >          02 B-3 PIC 9.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^
       8 >          02 B-4 OCCURS 5 TIMES.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       9 >            03 FILLER PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      10          66 B-R1 RENAMES B-1. *> should error
      11          66 B-R2 RENAMES B-1 THRU B-2. *> should error
    Item definition: {
      qualname: B
      offset: 0
      size: 96
      layout: {
        structure
        fields: {
          table
          offset: 0
          size: 40
          range: {
            span: fixed-length: 5
          }
          field: {
            qualname: B-1 IN B
            leading ranges: 1
            offset: 0
            size: 8
            layout: {
              elementary
              usage: {
                display
                category: NUMERIC(digits = 1, scale = 0, sign = unsigned)
              }
            }
          }
        }{
          qualname: B-2 IN B
          offset: 40
          size: 8
          layout: {
            elementary
            usage: {
              display
              category: NUMERIC(digits = 1, scale = 0, sign = unsigned)
            }
          }
        }{
          qualname: B-3 IN B
          offset: 48
          size: 8
          layout: {
            elementary
            usage: {
              display
              category: NUMERIC(digits = 1, scale = 0, sign = unsigned)
            }
          }
        }{
          table
          offset: 56
          size: 40
          range: {
            span: fixed-length: 5
          }
          field: {
            qualname: B-4 IN B
            leading ranges: 1
            offset: 56
            size: 8
            layout: {
              structure
              fields: {
                filler
                leading ranges: 1
                offset: 56
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
      }
    }
    prog.cob:10.7-10.27:
       7            02 B-3 PIC 9.
       8            02 B-4 OCCURS 5 TIMES.
       9              03 FILLER PIC X.
      10 >        66 B-R1 RENAMES B-1. *> should error
    ----          ^^^^^^^^^^^^^^^^^^^^
      11          66 B-R2 RENAMES B-1 THRU B-2. *> should error
      12          66 B-R3 RENAMES B-3 THRU B-4. *> should error
    Record renaming: {
      qualname: B-R1 IN B
      from: B-1 IN B
      offset: 0
      size: 8
      layout: {
        elementary
        usage: {
          display
          category: NUMERIC(digits = 1, scale = 0, sign = unsigned)
        }
      }
    }
    prog.cob:11.7-11.36:
       8            02 B-4 OCCURS 5 TIMES.
       9              03 FILLER PIC X.
      10          66 B-R1 RENAMES B-1. *> should error
      11 >        66 B-R2 RENAMES B-1 THRU B-2. *> should error
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      12          66 B-R3 RENAMES B-3 THRU B-4. *> should error
      13          66 B-R4 RENAMES B-3 THRU B-2. *> should error
    Record renaming: {
      qualname: B-R2 IN B
      from: B-1 IN B
      thru: B-2 IN B
      offset: 0
      size: 48
      layout: {
        elementary
        usage: {
          display
          category: ALPHANUMERIC(6)
        }
      }
    }
    prog.cob:12.7-12.36:
       9              03 FILLER PIC X.
      10          66 B-R1 RENAMES B-1. *> should error
      11          66 B-R2 RENAMES B-1 THRU B-2. *> should error
      12 >        66 B-R3 RENAMES B-3 THRU B-4. *> should error
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      13          66 B-R4 RENAMES B-3 THRU B-2. *> should error
      14          PROCEDURE DIVISION.
    Record renaming: {
      qualname: B-R3 IN B
      from: B-3 IN B
      thru: B-4 IN B
      offset: 48
      size: 16
      layout: {
        elementary
        usage: {
          display
          category: ALPHANUMERIC(2)
        }
      }
    }
    prog.cob:13.7-13.36:
      10          66 B-R1 RENAMES B-1. *> should error
      11          66 B-R2 RENAMES B-1 THRU B-2. *> should error
      12          66 B-R3 RENAMES B-3 THRU B-4. *> should error
      13 >        66 B-R4 RENAMES B-3 THRU B-2. *> should error
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      14          PROCEDURE DIVISION.
      15
    Record renaming: {
      qualname: B-R4 IN B
      /!\ with_errors /!\
      from: B-3 IN B
      thru: B-2 IN B
      offset: 48
      size: 0
      layout: {
        elementary
        usage: {
          display
          category: ALPHANUMERIC(0)
        }
      }
    }
  |}];;


let%expect_test "renames-missing-target" =
  dotest @@ prog "renames-missing-target"
    ~working_storage:{|
       01 A PIC X OCCURS 5 TIMES.
       66 A-R1 RENAMES A.
       01 B.
         02 B1 PIC X.
         02 B2 PIC X.
         66 BR RENAMES B1 THRU B3.
    |};
  [%expect {|
    prog.cob:5.23-5.24:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 A PIC X OCCURS 5 TIMES.
       5 >        66 A-R1 RENAMES A.
    ----                          ^
       6          01 B.
       7            02 B1 PIC X.
    >> Error: Item 'A IN A' not found

    prog.cob:9.31-9.33:
       6          01 B.
       7            02 B1 PIC X.
       8            02 B2 PIC X.
       9 >          66 BR RENAMES B1 THRU B3.
    ----                                  ^^
      10          PROCEDURE DIVISION.
      11
    >> Error: Item 'B3 IN B' not found

    prog.cob:4.7-4.33:
       1          PROGRAM-ID. renames-missing-target.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 A PIC X OCCURS 5 TIMES.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^
       5          66 A-R1 RENAMES A.
       6          01 B.
    Item definition: {
      table
      offset: 0
      size: 40
      range: {
        span: fixed-length: 5
      }
      field: {
        qualname: A
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
    prog.cob:6.7-8.21:
       3          WORKING-STORAGE SECTION.
       4          01 A PIC X OCCURS 5 TIMES.
       5          66 A-R1 RENAMES A.
       6 >        01 B.
    ----          ^^^^^
       7 >          02 B1 PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^
       8 >          02 B2 PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^
       9            66 BR RENAMES B1 THRU B3.
      10          PROCEDURE DIVISION.
    Item definition: {
      qualname: B
      offset: 0
      size: 16
      layout: {
        structure
        fields: {
          qualname: B1 IN B
          offset: 0
          size: 8
          layout: {
            elementary
            usage: {
              display
              category: ALPHANUMERIC(1)
            }
          }
        }{
          qualname: B2 IN B
          offset: 8
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
    prog.cob:9.9-9.34:
       6          01 B.
       7            02 B1 PIC X.
       8            02 B2 PIC X.
       9 >          66 BR RENAMES B1 THRU B3.
    ----            ^^^^^^^^^^^^^^^^^^^^^^^^^
      10          PROCEDURE DIVISION.
      11
    Record renaming: {
      qualname: BR IN B
      /!\ with_errors /!\
      from: B1 IN B
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
  |}];;

let%expect_test "renames-errors-invalid-sizes" =
  dotest @@ prog "renames-errors-invalid-sizes"
    ~working_storage:{|
       01 FILLER.
         02 A PIC X.
         02 B PIC 1 USAGE BIT.
         66 C RENAMES A THRU B. *> should error
    |};
  [%expect {|
    prog.cob:7.9-7.31:
       4          01 FILLER.
       5            02 A PIC X.
       6            02 B PIC 1 USAGE BIT.
       7 >          66 C RENAMES A THRU B. *> should error
    ----            ^^^^^^^^^^^^^^^^^^^^^^
       8          PROCEDURE DIVISION.
       9
    >> Error: Invalid range of 9 bits between RENAMES operands

    prog.cob:4.7-6.30:
       1          PROGRAM-ID. renames-errors-invalid-sizes.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 FILLER.
    ----          ^^^^^^^^^^
       5 >          02 A PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^
       6 >          02 B PIC 1 USAGE BIT.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7            66 C RENAMES A THRU B. *> should error
       8          PROCEDURE DIVISION.
    Item definition: {
      filler
      offset: 0
      size: 9
      layout: {
        structure
        fields: {
          qualname: A
          offset: 0
          size: 8
          layout: {
            elementary
            usage: {
              display
              category: ALPHANUMERIC(1)
            }
          }
        }{
          qualname: B
          offset: 8
          size: 1
          layout: {
            elementary
            usage: {
              bit
              category: BOOLEAN(1)
            }
          }
        }
      }
    }
    prog.cob:7.9-7.31:
       4          01 FILLER.
       5            02 A PIC X.
       6            02 B PIC 1 USAGE BIT.
       7 >          66 C RENAMES A THRU B. *> should error
    ----            ^^^^^^^^^^^^^^^^^^^^^^
       8          PROCEDURE DIVISION.
       9
    Record renaming: {
      qualname: C
      /!\ with_errors /!\
      from: A
      thru: B
      offset: 0
      size: 9
      layout: {
        elementary
        usage: {
          display
          category: ALPHANUMERIC(1)
        }
      }
    } |}];;
