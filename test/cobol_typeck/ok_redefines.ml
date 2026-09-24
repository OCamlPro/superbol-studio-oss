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

let dotest = Typeck_testing.show_data ~show_whole_definitions:true

let%expect_test "redefines-occurs" =
  dotest @@ prog "redefines-occurs"
    ~working_storage:{|
       77 A             PIC X(5).
       77 B REDEFINES A PIC X OCCURS 5.
    |};
  [%expect {|
    Whole data defintions:
    prog.cob:4.7-5.39:
       1          PROGRAM-ID. redefines-occurs.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A             PIC X(5).
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^
       5 >        77 B REDEFINES A PIC X OCCURS 5.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    Definition: {
      data field
      record: A
      def: {
        qualname: A
        offset: 0
        size: 40
        layout: {
          elementary
          usage: {
            display
            category: ALPHANUMERIC(5)
          }
        }
        redefs: {
          table
          redefines: A
          offset: 0
          size: 40
          range: {
            span: fixed-length: 5
          }
          field: {
            qualname: B
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
      main-def: (same as def)
    }
    prog.cob:5.7-5.39:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A             PIC X(5).
       5 >        77 B REDEFINES A PIC X OCCURS 5.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    Definition: {
      data field
      record: A
      def: {
        qualname: B
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
      main-def: {
        qualname: A
        offset: 0
        size: 40
        layout: {
          elementary
          usage: {
            display
            category: ALPHANUMERIC(5)
          }
        }
        redefs: {
          table
          redefines: A
          offset: 0
          size: 40
          range: {
            span: fixed-length: 5
          }
          field: {
            qualname: B
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
      table-def: {
        table
        redefines: A
        offset: 0
        size: 40
        range: {
          span: fixed-length: 5
        }
        field: {
          qualname: B
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
    } |}];;

let%expect_test "qualified-redefines-occurs" =
  dotest @@ prog "qualified-redefines-occurs"
    ~working_storage:{|
       01 X.
         02 a             PIC X(5).
         02 B REDEFINES A PIC X OCCURS 5.
    |};
  [%expect {|
    Whole data defintions:
    prog.cob:4.7-6.41:
       1          PROGRAM-ID. qualified-redefines-occurs.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 X.
    ----          ^^^^^
       5 >          02 a             PIC X(5).
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6 >          02 B REDEFINES A PIC X OCCURS 5.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    Definition: {
      data field
      record: X
      def: {
        qualname: X
        offset: 0
        size: 40
        layout: {
          structure
          fields: {
            qualname: a IN X
            offset: 0
            size: 40
            layout: {
              elementary
              usage: {
                display
                category: ALPHANUMERIC(5)
              }
            }
            redefs: {
              table
              redefines: A IN X
              offset: 0
              size: 40
              range: {
                span: fixed-length: 5
              }
              field: {
                qualname: B IN X
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
      }
      main-def: (same as def)
    }
    prog.cob:5.9-6.41:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 X.
       5 >          02 a             PIC X(5).
    ----            ^^^^^^^^^^^^^^^^^^^^^^^^^^
       6 >          02 B REDEFINES A PIC X OCCURS 5.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    Definition: {
      data field
      record: X
      def: {
        qualname: a IN X
        offset: 0
        size: 40
        layout: {
          elementary
          usage: {
            display
            category: ALPHANUMERIC(5)
          }
        }
        redefs: {
          table
          redefines: A IN X
          offset: 0
          size: 40
          range: {
            span: fixed-length: 5
          }
          field: {
            qualname: B IN X
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
      main-def: (same as def)
    }
    prog.cob:6.9-6.41:
       3          WORKING-STORAGE SECTION.
       4          01 X.
       5            02 a             PIC X(5).
       6 >          02 B REDEFINES A PIC X OCCURS 5.
    ----            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    Definition: {
      data field
      record: X
      def: {
        qualname: B IN X
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
      main-def: {
        qualname: a IN X
        offset: 0
        size: 40
        layout: {
          elementary
          usage: {
            display
            category: ALPHANUMERIC(5)
          }
        }
        redefs: {
          table
          redefines: A IN X
          offset: 0
          size: 40
          range: {
            span: fixed-length: 5
          }
          field: {
            qualname: B IN X
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
      table-def: {
        table
        redefines: A IN X
        offset: 0
        size: 40
        range: {
          span: fixed-length: 5
        }
        field: {
          qualname: B IN X
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
    } |}];;

let%expect_test "redefines-with-value" =
  dotest @@ prog "redefines-with-value"
    ~working_storage:{|
       01 X             PIC X(6).
       01 Y REDEFINES X PIC X    OCCURS 6 VALUE "A".
       01 Z REDEFINES X PIC XX   OCCURS 3 VALUE "AA".
    |};
  [%expect {|
    prog.cob:5.42-5.51:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 X             PIC X(6).
       5 >        01 Y REDEFINES X PIC X    OCCURS 6 VALUE "A".
    ----                                             ^^^^^^^^^
       6          01 Z REDEFINES X PIC XX   OCCURS 3 VALUE "AA".
       7          PROCEDURE DIVISION.
    >> Warning: Ignored VALUE clause for item 'Y' with REDEFINES clause

    prog.cob:6.42-6.52:
       3          WORKING-STORAGE SECTION.
       4          01 X             PIC X(6).
       5          01 Y REDEFINES X PIC X    OCCURS 6 VALUE "A".
       6 >        01 Z REDEFINES X PIC XX   OCCURS 3 VALUE "AA".
    ----                                             ^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    >> Warning: Ignored VALUE clause for item 'Z' with REDEFINES clause

    Whole data defintions:
    prog.cob:4.7-6.53:
       1          PROGRAM-ID. redefines-with-value.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 X             PIC X(6).
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^
       5 >        01 Y REDEFINES X PIC X    OCCURS 6 VALUE "A".
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6 >        01 Z REDEFINES X PIC XX   OCCURS 3 VALUE "AA".
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    Definition: {
      data field
      record: X
      def: {
        qualname: X
        offset: 0
        size: 48
        layout: {
          elementary
          usage: {
            display
            category: ALPHANUMERIC(6)
          }
        }
        redefs: {
          table
          redefines: X
          offset: 0
          size: 48
          range: {
            span: fixed-length: 6
          }
          field: {
            qualname: Y
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
        }{
          table
          redefines: X
          offset: 0
          size: 48
          range: {
            span: fixed-length: 3
          }
          field: {
            qualname: Z
            leading ranges: 1
            offset: 0
            size: 16
            layout: {
              elementary
              usage: {
                display
                category: ALPHANUMERIC(2)
              }
            }
          }
        }
      }
      main-def: (same as def)
    }
    prog.cob:5.7-5.52:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 X             PIC X(6).
       5 >        01 Y REDEFINES X PIC X    OCCURS 6 VALUE "A".
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6          01 Z REDEFINES X PIC XX   OCCURS 3 VALUE "AA".
       7          PROCEDURE DIVISION.
    Definition: {
      data field
      record: X
      def: {
        qualname: Y
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
      main-def: {
        qualname: X
        offset: 0
        size: 48
        layout: {
          elementary
          usage: {
            display
            category: ALPHANUMERIC(6)
          }
        }
        redefs: {
          table
          redefines: X
          offset: 0
          size: 48
          range: {
            span: fixed-length: 6
          }
          field: {
            qualname: Y
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
        }{
          table
          redefines: X
          offset: 0
          size: 48
          range: {
            span: fixed-length: 3
          }
          field: {
            qualname: Z
            leading ranges: 1
            offset: 0
            size: 16
            layout: {
              elementary
              usage: {
                display
                category: ALPHANUMERIC(2)
              }
            }
          }
        }
      }
      table-def: {
        table
        redefines: X
        offset: 0
        size: 48
        range: {
          span: fixed-length: 6
        }
        field: {
          qualname: Y
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
    prog.cob:6.7-6.53:
       3          WORKING-STORAGE SECTION.
       4          01 X             PIC X(6).
       5          01 Y REDEFINES X PIC X    OCCURS 6 VALUE "A".
       6 >        01 Z REDEFINES X PIC XX   OCCURS 3 VALUE "AA".
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    Definition: {
      data field
      record: X
      def: {
        qualname: Z
        leading ranges: 1
        offset: 0
        size: 16
        layout: {
          elementary
          usage: {
            display
            category: ALPHANUMERIC(2)
          }
        }
      }
      main-def: {
        qualname: X
        offset: 0
        size: 48
        layout: {
          elementary
          usage: {
            display
            category: ALPHANUMERIC(6)
          }
        }
        redefs: {
          table
          redefines: X
          offset: 0
          size: 48
          range: {
            span: fixed-length: 6
          }
          field: {
            qualname: Y
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
        }{
          table
          redefines: X
          offset: 0
          size: 48
          range: {
            span: fixed-length: 3
          }
          field: {
            qualname: Z
            leading ranges: 1
            offset: 0
            size: 16
            layout: {
              elementary
              usage: {
                display
                category: ALPHANUMERIC(2)
              }
            }
          }
        }
      }
      table-def: {
        table
        redefines: X
        offset: 0
        size: 48
        range: {
          span: fixed-length: 3
        }
        field: {
          qualname: Z
          leading ranges: 1
          offset: 0
          size: 16
          layout: {
            elementary
            usage: {
              display
              category: ALPHANUMERIC(2)
            }
          }
        }
      }
    } |}];;

let%expect_test "occurs-n-redefines-1" =
  dotest @@ prog "occurs-n-redefines-1"
    ~working_storage:{|
       01 W.
         02 A OCCURS 5 TIMES PIC X VALUE "A".
         02 B REDEFINES a PIC X(5).
         02 C PIC 9.
    |}
    ~procedure:{|
           MOVE "BCDEF" TO b
           DISPLAY A (1) "/" B "/" C.
    |};
  [%expect {|
    prog.cob:6.9-6.35:
       3          WORKING-STORAGE SECTION.
       4          01 W.
       5            02 A OCCURS 5 TIMES PIC X VALUE "A".
       6 >          02 B REDEFINES a PIC X(5).
    ----            ^^^^^^^^^^^^^^^^^^^^^^^^^^
       7            02 C PIC 9.
       8          PROCEDURE DIVISION.
    >> Warning: Redefinition of item with OCCURS clause A IN W

    Whole data defintions:
    prog.cob:4.7-7.20:
       1          PROGRAM-ID. occurs-n-redefines-1.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 W.
    ----          ^^^^^
       5 >          02 A OCCURS 5 TIMES PIC X VALUE "A".
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6 >          02 B REDEFINES a PIC X(5).
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7 >          02 C PIC 9.
    ----  ^^^^^^^^^^^^^^^^^^^^^
       8          PROCEDURE DIVISION.
       9          MOVE "BCDEF" TO b
    Definition: {
      data field
      record: W
      def: {
        qualname: W
        offset: 0
        size: 48
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
              qualname: A IN W
              leading ranges: 1
              offset: 0
              size: 8
              layout: {
                elementary
                usage: {
                  display
                  category: ALPHANUMERIC(1)
                }
                value: "A"
              }
            }
            redefs: {
              qualname: B IN W
              redefines: a IN W
              offset: 0
              size: 40
              layout: {
                elementary
                usage: {
                  display
                  category: ALPHANUMERIC(5)
                }
              }
            }
          }{
            qualname: C IN W
            offset: 40
            size: 8
            layout: {
              elementary
              usage: {
                display
                category: NUMERIC(digits = 1, scale = 0, signed = false)
              }
            }
          }
        }
      }
      main-def: (same as def)
    }
    prog.cob:5.9-5.45:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 W.
       5 >          02 A OCCURS 5 TIMES PIC X VALUE "A".
    ----            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6            02 B REDEFINES a PIC X(5).
       7            02 C PIC 9.
    Definition: {
      data field
      record: W
      def: {
        qualname: A IN W
        leading ranges: 1
        offset: 0
        size: 8
        layout: {
          elementary
          usage: {
            display
            category: ALPHANUMERIC(1)
          }
          value: "A"
        }
      }
      main-def: (same as def)
      table-def: {
        table
        offset: 0
        size: 40
        range: {
          span: fixed-length: 5
        }
        field: {
          qualname: A IN W
          leading ranges: 1
          offset: 0
          size: 8
          layout: {
            elementary
            usage: {
              display
              category: ALPHANUMERIC(1)
            }
            value: "A"
          }
        }
        redefs: {
          qualname: B IN W
          redefines: a IN W
          offset: 0
          size: 40
          layout: {
            elementary
            usage: {
              display
              category: ALPHANUMERIC(5)
            }
          }
        }
      }
    }
    prog.cob:6.9-6.35:
       3          WORKING-STORAGE SECTION.
       4          01 W.
       5            02 A OCCURS 5 TIMES PIC X VALUE "A".
       6 >          02 B REDEFINES a PIC X(5).
    ----            ^^^^^^^^^^^^^^^^^^^^^^^^^^
       7            02 C PIC 9.
       8          PROCEDURE DIVISION.
    Definition: {
      data field
      record: W
      def: {
        qualname: B IN W
        redefines: a IN W
        offset: 0
        size: 40
        layout: {
          elementary
          usage: {
            display
            category: ALPHANUMERIC(5)
          }
        }
      }
      main-def: {
        table
        offset: 0
        size: 40
        range: {
          span: fixed-length: 5
        }
        field: {
          qualname: A IN W
          leading ranges: 1
          offset: 0
          size: 8
          layout: {
            elementary
            usage: {
              display
              category: ALPHANUMERIC(1)
            }
            value: "A"
          }
        }
        redefs: {
          qualname: B IN W
          redefines: a IN W
          offset: 0
          size: 40
          layout: {
            elementary
            usage: {
              display
              category: ALPHANUMERIC(5)
            }
          }
        }
      }
    }
    prog.cob:7.9-7.20:
       4          01 W.
       5            02 A OCCURS 5 TIMES PIC X VALUE "A".
       6            02 B REDEFINES a PIC X(5).
       7 >          02 C PIC 9.
    ----            ^^^^^^^^^^^
       8          PROCEDURE DIVISION.
       9          MOVE "BCDEF" TO b
    Definition: {
      data field
      record: W
      def: {
        qualname: C IN W
        offset: 40
        size: 8
        layout: {
          elementary
          usage: {
            display
            category: NUMERIC(digits = 1, scale = 0, signed = false)
          }
        }
      }
      main-def: (same as def)
    } |}];;

let%expect_test "occurs-n-redefines-2" =
  dotest @@ prog "occurs-n-redefines-2"
    ~working_storage:{|
       01 W.
         02 A PIC X OCCURS 2 TIMES.
       01 X REDEFINES W.
         02 B PIC X.
         02 C PIC X.
    |};
  [%expect {|
    Whole data defintions:
    prog.cob:4.7-8.20:
       1          PROGRAM-ID. occurs-n-redefines-2.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 W.
    ----          ^^^^^
       5 >          02 A PIC X OCCURS 2 TIMES.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6 >        01 X REDEFINES W.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^
       7 >          02 B PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^
       8 >          02 C PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^
       9          PROCEDURE DIVISION.
      10
    Definition: {
      data field
      record: W
      def: {
        qualname: W
        offset: 0
        size: 16
        layout: {
          structure
          fields: {
            table
            offset: 0
            size: 16
            range: {
              span: fixed-length: 2
            }
            field: {
              qualname: A IN W
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
        redefs: {
          qualname: X
          redefines: W
          offset: 0
          size: 16
          layout: {
            structure
            fields: {
              qualname: B IN X
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
              qualname: C IN X
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
      }
      main-def: (same as def)
    }
    prog.cob:5.9-5.35:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 W.
       5 >          02 A PIC X OCCURS 2 TIMES.
    ----            ^^^^^^^^^^^^^^^^^^^^^^^^^^
       6          01 X REDEFINES W.
       7            02 B PIC X.
    Definition: {
      data field
      record: W
      def: {
        qualname: A IN W
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
      main-def: {
        qualname: W
        offset: 0
        size: 16
        layout: {
          structure
          fields: {
            table
            offset: 0
            size: 16
            range: {
              span: fixed-length: 2
            }
            field: {
              qualname: A IN W
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
        redefs: {
          qualname: X
          redefines: W
          offset: 0
          size: 16
          layout: {
            structure
            fields: {
              qualname: B IN X
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
              qualname: C IN X
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
      }
      table-def: {
        table
        offset: 0
        size: 16
        range: {
          span: fixed-length: 2
        }
        field: {
          qualname: A IN W
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
    prog.cob:6.7-8.20:
       3          WORKING-STORAGE SECTION.
       4          01 W.
       5            02 A PIC X OCCURS 2 TIMES.
       6 >        01 X REDEFINES W.
    ----          ^^^^^^^^^^^^^^^^^
       7 >          02 B PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^
       8 >          02 C PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^
       9          PROCEDURE DIVISION.
      10
    Definition: {
      data field
      record: W
      def: {
        qualname: X
        redefines: W
        offset: 0
        size: 16
        layout: {
          structure
          fields: {
            qualname: B IN X
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
            qualname: C IN X
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
      main-def: {
        qualname: W
        offset: 0
        size: 16
        layout: {
          structure
          fields: {
            table
            offset: 0
            size: 16
            range: {
              span: fixed-length: 2
            }
            field: {
              qualname: A IN W
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
        redefs: {
          qualname: X
          redefines: W
          offset: 0
          size: 16
          layout: {
            structure
            fields: {
              qualname: B IN X
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
              qualname: C IN X
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
      }
    }
    prog.cob:7.9-7.20:
       4          01 W.
       5            02 A PIC X OCCURS 2 TIMES.
       6          01 X REDEFINES W.
       7 >          02 B PIC X.
    ----            ^^^^^^^^^^^
       8            02 C PIC X.
       9          PROCEDURE DIVISION.
    Definition: {
      data field
      record: W
      def: {
        qualname: B IN X
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
      main-def: {
        qualname: W
        offset: 0
        size: 16
        layout: {
          structure
          fields: {
            table
            offset: 0
            size: 16
            range: {
              span: fixed-length: 2
            }
            field: {
              qualname: A IN W
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
        redefs: {
          qualname: X
          redefines: W
          offset: 0
          size: 16
          layout: {
            structure
            fields: {
              qualname: B IN X
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
              qualname: C IN X
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
      }
    }
    prog.cob:8.9-8.20:
       5            02 A PIC X OCCURS 2 TIMES.
       6          01 X REDEFINES W.
       7            02 B PIC X.
       8 >          02 C PIC X.
    ----            ^^^^^^^^^^^
       9          PROCEDURE DIVISION.
      10
    Definition: {
      data field
      record: W
      def: {
        qualname: C IN X
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
      main-def: {
        qualname: W
        offset: 0
        size: 16
        layout: {
          structure
          fields: {
            table
            offset: 0
            size: 16
            range: {
              span: fixed-length: 2
            }
            field: {
              qualname: A IN W
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
        redefs: {
          qualname: X
          redefines: W
          offset: 0
          size: 16
          layout: {
            structure
            fields: {
              qualname: B IN X
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
              qualname: C IN X
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
      }
    } |}];;


let%expect_test "redefines-index" =
  (* Note: GnuCOBOL accepts this.  But should it really?  *)
  dotest @@ prog "redefines-index"
    ~working_storage:{|
       01 xx USAGE IS INDEX.
         02 x.
         02 y REDEFINES x.
       01 yy REDEFINES xx PIC 9(4).
    |}
    ~procedure:{|
       MAIN.
           DISPLAY x
           DISPLAY yy.
    |};
  [%expect {|
    Whole data defintions:
    prog.cob:4.7-7.35:
       1          PROGRAM-ID. redefines-index.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 xx USAGE IS INDEX.
    ----          ^^^^^^^^^^^^^^^^^^^^^
       5 >          02 x.
    ----  ^^^^^^^^^^^^^^^
       6 >          02 y REDEFINES x.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7 >        01 yy REDEFINES xx PIC 9(4).
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       8          PROCEDURE DIVISION.
       9          MAIN.
    Definition: {
      data field
      record: XX
      def: {
        qualname: xx
        offset: 0
        size: size-of-index
        layout: {
          structure
          fields: {
            qualname: x IN xx
            offset: 0
            size: size-of-index
            layout: {
              elementary
              usage: index
            }
            redefs: {
              qualname: y IN xx
              redefines: x IN xx
              offset: 0
              size: size-of-index
              layout: {
                elementary
                usage: index
              }
            }
          }
        }
        redefs: {
          qualname: yy
          redefines: xx
          offset: 0
          size: 32
          layout: {
            elementary
            usage: {
              display
              category: NUMERIC(digits = 4, scale = 0, signed = false)
            }
          }
        }
      }
      main-def: (same as def)
    }
    prog.cob:5.9-6.26:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 xx USAGE IS INDEX.
       5 >          02 x.
    ----            ^^^^^
       6 >          02 y REDEFINES x.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          01 yy REDEFINES xx PIC 9(4).
       8          PROCEDURE DIVISION.
    Definition: {
      data field
      record: XX
      def: {
        qualname: x IN xx
        offset: 0
        size: size-of-index
        layout: {
          elementary
          usage: index
        }
        redefs: {
          qualname: y IN xx
          redefines: x IN xx
          offset: 0
          size: size-of-index
          layout: {
            elementary
            usage: index
          }
        }
      }
      main-def: (same as def)
    }
    prog.cob:6.9-6.26:
       3          WORKING-STORAGE SECTION.
       4          01 xx USAGE IS INDEX.
       5            02 x.
       6 >          02 y REDEFINES x.
    ----            ^^^^^^^^^^^^^^^^^
       7          01 yy REDEFINES xx PIC 9(4).
       8          PROCEDURE DIVISION.
    Definition: {
      data field
      record: XX
      def: {
        qualname: y IN xx
        redefines: x IN xx
        offset: 0
        size: size-of-index
        layout: {
          elementary
          usage: index
        }
      }
      main-def: {
        qualname: x IN xx
        offset: 0
        size: size-of-index
        layout: {
          elementary
          usage: index
        }
        redefs: {
          qualname: y IN xx
          redefines: x IN xx
          offset: 0
          size: size-of-index
          layout: {
            elementary
            usage: index
          }
        }
      }
    }
    prog.cob:7.7-7.35:
       4          01 xx USAGE IS INDEX.
       5            02 x.
       6            02 y REDEFINES x.
       7 >        01 yy REDEFINES xx PIC 9(4).
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       8          PROCEDURE DIVISION.
       9          MAIN.
    Definition: {
      data field
      record: XX
      def: {
        qualname: yy
        redefines: xx
        offset: 0
        size: 32
        layout: {
          elementary
          usage: {
            display
            category: NUMERIC(digits = 4, scale = 0, signed = false)
          }
        }
      }
      main-def: {
        qualname: xx
        offset: 0
        size: size-of-index
        layout: {
          structure
          fields: {
            qualname: x IN xx
            offset: 0
            size: size-of-index
            layout: {
              elementary
              usage: index
            }
            redefs: {
              qualname: y IN xx
              redefines: x IN xx
              offset: 0
              size: size-of-index
              layout: {
                elementary
                usage: index
              }
            }
          }
        }
        redefs: {
          qualname: yy
          redefines: xx
          offset: 0
          size: 32
          layout: {
            elementary
            usage: {
              display
              category: NUMERIC(digits = 4, scale = 0, signed = false)
            }
          }
        }
      }
    } |}];;
