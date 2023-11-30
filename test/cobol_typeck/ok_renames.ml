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

let%expect_test "renames-with-redefines" =
  dotest @@ prog "renames-with-redefines"
    ~working_storage:{|
       01 A.
         02 A-1 PIC X.
         02 A-A PIC 9.
         02 A-B PIC 9.
         02 A-2 PIC 9 VALUE 5.
         02 A-21 REDEFINES A-2 PIC 9.
         02 A-22 REDEFINES A-2 PIC 9.
       66 A-R1 RENAMES A-1 THROUGH A-B.
       66 A-R2 RENAMES A-A.
       01 B PIC X OCCURS 5 TIMES.
    |};
  [%expect {|
    prog.cob:4.7-10.37:
       1          PROGRAM-ID. renames-with-redefines.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 A.
    ----          ^^^^^
       5 >          02 A-1 PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^
       6 >          02 A-A PIC 9.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^
       7 >          02 A-B PIC 9.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^
       8 >          02 A-2 PIC 9 VALUE 5.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       9 >          02 A-21 REDEFINES A-2 PIC 9.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      10 >          02 A-22 REDEFINES A-2 PIC 9.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      11          66 A-R1 RENAMES A-1 THROUGH A-B.
      12          66 A-R2 RENAMES A-A.
    Item definition: {
      qualname: A
      offset: 0
      size: 4
      layout: {
        structure
        fields: {
          qualname: A-1 IN A
          offset: 0
          size: 1
          layout: {
            elementary
            usage: {
              display (dev: temporary)
              category: ALPHANUMERIC(1)
            }
          }
        }{
          qualname: A-A IN A
          offset: 1
          size: 1
          layout: {
            elementary
            usage: {
              display (dev: temporary)
              category: NUMERIC(digits = 1, scale = 0, with_sign = false)
            }
          }
        }{
          qualname: A-B IN A
          offset: 2
          size: 1
          layout: {
            elementary
            usage: {
              display (dev: temporary)
              category: NUMERIC(digits = 1, scale = 0, with_sign = false)
            }
          }
        }{
          qualname: A-2 IN A
          offset: 3
          size: 1
          layout: {
            elementary
            usage: {
              display (dev: temporary)
              category: NUMERIC(digits = 1, scale = 0, with_sign = false)
            }
          }
          redefs: {
            qualname: A-21 IN A
            redefines: A-2 IN A
            offset: 3
            size: 1
            layout: {
              elementary
              usage: {
                display (dev: temporary)
                category: NUMERIC(digits = 1, scale = 0, with_sign = false)
              }
            }
          }{
            qualname: A-22 IN A
            redefines: A-2 IN A
            offset: 3
            size: 1
            layout: {
              elementary
              usage: {
                display (dev: temporary)
                category: NUMERIC(digits = 1, scale = 0, with_sign = false)
              }
            }
          }
        }
      }
    }
    prog.cob:11.7-11.39:
       8            02 A-2 PIC 9 VALUE 5.
       9            02 A-21 REDEFINES A-2 PIC 9.
      10            02 A-22 REDEFINES A-2 PIC 9.
      11 >        66 A-R1 RENAMES A-1 THROUGH A-B.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      12          66 A-R2 RENAMES A-A.
      13          01 B PIC X OCCURS 5 TIMES.
    Record renaming: {
      qualname: A-R1 IN A
      from: A-1 IN A
      thru: A-B IN A
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
    prog.cob:12.7-12.27:
       9            02 A-21 REDEFINES A-2 PIC 9.
      10            02 A-22 REDEFINES A-2 PIC 9.
      11          66 A-R1 RENAMES A-1 THROUGH A-B.
      12 >        66 A-R2 RENAMES A-A.
    ----          ^^^^^^^^^^^^^^^^^^^^
      13          01 B PIC X OCCURS 5 TIMES.
      14          PROCEDURE DIVISION.
    Record renaming: {
      qualname: A-R2 IN A
      from: A-A IN A
      offset: 1
      size: 1
      layout: {
        elementary
        usage: {
          display (dev: temporary)
          category: NUMERIC(digits = 1, scale = 0, with_sign = false)
        }
      }
    }
    prog.cob:13.7-13.33:
      10            02 A-22 REDEFINES A-2 PIC 9.
      11          66 A-R1 RENAMES A-1 THROUGH A-B.
      12          66 A-R2 RENAMES A-A.
      13 >        01 B PIC X OCCURS 5 TIMES.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^
      14          PROCEDURE DIVISION.
      15
    Item definition: {
      filler
      offset: 0
      size: 5
      layout: {
        fixed-length table
        length: 5
        items: {
          qualname: B
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
    } |}];;

let%expect_test "renames-qualif" =
  dotest @@ prog "renames-qualif"
    ~working_storage:{|
       01 A.
         02 A-T-LEN PIC 99.
         02 A-1 PIC X.
         02 A-T PIC 9 OCCURS 5 TIMES.
         02 A-2 PIC 9 VALUE 5.
         02 A-22 REDEFINES A-2 PIC 9.
       66 A-R1 RENAMES A-1 THROUGH A-22.
       66 A-R2 RENAMES A-22.
       01 YYY PIC 999.
       01 XXX REDEFINES YYY.
        02 FILLER.
        03 ZZZ.
        04 FILLER.
         05 DDD PIC 9.
         05 FFF PIC 9.
         05 EEE PIC 9.
       66 FFF-2 RENAMES FFF THRU EEE.
    |};
  [%expect {|
    prog.cob:4.7-9.37:
       1          PROGRAM-ID. renames-qualif.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 A.
    ----          ^^^^^
       5 >          02 A-T-LEN PIC 99.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6 >          02 A-1 PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^
       7 >          02 A-T PIC 9 OCCURS 5 TIMES.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       8 >          02 A-2 PIC 9 VALUE 5.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       9 >          02 A-22 REDEFINES A-2 PIC 9.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      10          66 A-R1 RENAMES A-1 THROUGH A-22.
      11          66 A-R2 RENAMES A-22.
    Item definition: {
      qualname: A
      offset: 0
      size: 9
      layout: {
        structure
        fields: {
          qualname: A-T-LEN IN A
          offset: 0
          size: 2
          layout: {
            elementary
            usage: {
              display (dev: temporary)
              category: NUMERIC(digits = 2, scale = 0, with_sign = false)
            }
          }
        }{
          qualname: A-1 IN A
          offset: 2
          size: 1
          layout: {
            elementary
            usage: {
              display (dev: temporary)
              category: ALPHANUMERIC(1)
            }
          }
        }{
          filler
          offset: 3
          size: 5
          layout: {
            fixed-length table
            length: 5
            items: {
              qualname: A-T IN A
              offset: 3
              size: 1
              layout: {
                elementary
                usage: {
                  display (dev: temporary)
                  category: NUMERIC(digits = 1, scale = 0, with_sign = false)
                }
              }
            }
          }
        }{
          qualname: A-2 IN A
          offset: 8
          size: 1
          layout: {
            elementary
            usage: {
              display (dev: temporary)
              category: NUMERIC(digits = 1, scale = 0, with_sign = false)
            }
          }
          redefs: {
            qualname: A-22 IN A
            redefines: A-2 IN A
            offset: 8
            size: 1
            layout: {
              elementary
              usage: {
                display (dev: temporary)
                category: NUMERIC(digits = 1, scale = 0, with_sign = false)
              }
            }
          }
        }
      }
    }
    prog.cob:10.7-10.40:
       7            02 A-T PIC 9 OCCURS 5 TIMES.
       8            02 A-2 PIC 9 VALUE 5.
       9            02 A-22 REDEFINES A-2 PIC 9.
      10 >        66 A-R1 RENAMES A-1 THROUGH A-22.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      11          66 A-R2 RENAMES A-22.
      12          01 YYY PIC 999.
    Record renaming: {
      qualname: A-R1 IN A
      from: A-1 IN A
      thru: A-22 IN A
      offset: 2
      size: 7
      layout: {
        elementary
        usage: {
          display (dev: temporary)
          category: ALPHANUMERIC(7)
        }
      }
    }
    prog.cob:11.7-11.28:
       8            02 A-2 PIC 9 VALUE 5.
       9            02 A-22 REDEFINES A-2 PIC 9.
      10          66 A-R1 RENAMES A-1 THROUGH A-22.
      11 >        66 A-R2 RENAMES A-22.
    ----          ^^^^^^^^^^^^^^^^^^^^^
      12          01 YYY PIC 999.
      13          01 XXX REDEFINES YYY.
    Record renaming: {
      qualname: A-R2 IN A
      from: A-22 IN A
      offset: 8
      size: 1
      layout: {
        elementary
        usage: {
          display (dev: temporary)
          category: NUMERIC(digits = 1, scale = 0, with_sign = false)
        }
      }
    }
    prog.cob:12.7-19.22:
       9            02 A-22 REDEFINES A-2 PIC 9.
      10          66 A-R1 RENAMES A-1 THROUGH A-22.
      11          66 A-R2 RENAMES A-22.
      12 >        01 YYY PIC 999.
    ----          ^^^^^^^^^^^^^^^
      13 >        01 XXX REDEFINES YYY.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      14 >         02 FILLER.
    ----  ^^^^^^^^^^^^^^^^^^^
      15 >         03 ZZZ.
    ----  ^^^^^^^^^^^^^^^^
      16 >         04 FILLER.
    ----  ^^^^^^^^^^^^^^^^^^^
      17 >          05 DDD PIC 9.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^
      18 >          05 FFF PIC 9.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^
      19 >          05 EEE PIC 9.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^
      20          66 FFF-2 RENAMES FFF THRU EEE.
      21          PROCEDURE DIVISION.
    Item definition: {
      qualname: YYY
      offset: 0
      size: 3
      layout: {
        elementary
        usage: {
          display (dev: temporary)
          category: NUMERIC(digits = 3, scale = 0, with_sign = false)
        }
      }
      redefs: {
        qualname: XXX
        redefines: YYY
        offset: 0
        size: 3
        layout: {
          structure
          fields: {
            filler
            offset: 0
            size: 3
            layout: {
              structure
              fields: {
                qualname: ZZZ IN XXX
                offset: 0
                size: 3
                layout: {
                  structure
                  fields: {
                    filler
                    offset: 0
                    size: 3
                    layout: {
                      structure
                      fields: {
                        qualname: DDD IN ZZZ IN XXX
                        offset: 0
                        size: 1
                        layout: {
                          elementary
                          usage: {
                            display (dev: temporary)
                            category:
                             NUMERIC(digits = 1, scale = 0, with_sign = false)
                          }
                        }
                      }{
                        qualname: FFF IN ZZZ IN XXX
                        offset: 1
                        size: 1
                        layout: {
                          elementary
                          usage: {
                            display (dev: temporary)
                            category:
                             NUMERIC(digits = 1, scale = 0, with_sign = false)
                          }
                        }
                      }{
                        qualname: EEE IN ZZZ IN XXX
                        offset: 2
                        size: 1
                        layout: {
                          elementary
                          usage: {
                            display (dev: temporary)
                            category:
                             NUMERIC(digits = 1, scale = 0, with_sign = false)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    prog.cob:20.7-20.37:
      17            05 DDD PIC 9.
      18            05 FFF PIC 9.
      19            05 EEE PIC 9.
      20 >        66 FFF-2 RENAMES FFF THRU EEE.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      21          PROCEDURE DIVISION.
      22
    Record renaming: {
      qualname: FFF-2 IN XXX
      from: FFF IN XXX
      thru: EEE IN XXX
      offset: 1
      size: 2
      layout: {
        elementary
        usage: {
          display (dev: temporary)
          category: ALPHANUMERIC(2)
        }
      }
    }
  |}];;
