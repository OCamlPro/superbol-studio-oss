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

let%expect_test "occurs-fixed-1" =
  dotest @@ prog "prog"
    ~working_storage:{|
       01 B PIC X OCCURS 5 TIMES.
    |};
  [%expect {|
    prog.cob:4.7-4.33:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 B PIC X OCCURS 5 TIMES.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^
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
    } |}];;

let%expect_test "occurs-fixed-2" =
  dotest @@ prog "prog"
    ~working_storage:{|
       01 B OCCURS 5.
         05 FILLER PIC X.
    |};
  [%expect {|
    prog.cob:4.7-5.25:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 B OCCURS 5.
    ----          ^^^^^^^^^^^^^^
       5 >          05 FILLER PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    Item definition: {
      table
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
          structure
          fields: {
            filler
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
    } |}];;

let%expect_test "occurs-fixed-3" =
  dotest @@ prog "prog"
    ~working_storage:{|
       01 TAB OCCURS 42.
         05 FILLER PIC X/X.
         05 FILLER PIC 9V9.
    |};
  [%expect {|
    prog.cob:4.7-6.27:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 TAB OCCURS 42.
    ----          ^^^^^^^^^^^^^^^^^
       5 >          05 FILLER PIC X/X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6 >          05 FILLER PIC 9V9.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    Item definition: {
      table
      offset: 0
      size: 1344
      range: {
        span: fixed-length: 42
      }
      field: {
        qualname: TAB
        leading ranges: 1
        offset: 0
        size: 32
        layout: {
          structure
          fields: {
            filler
            leading ranges: 1
            offset: 0
            size: 16
            layout: {
              elementary
              usage: {
                display
                category: ALPHANUMERIC-EDITED(2)
              }
            }
          }{
            filler
            leading ranges: 1
            offset: 16
            size: 16
            layout: {
              elementary
              usage: {
                display
                category: NUMERIC(digits = 2, scale = 1, with_sign = false)
              }
            }
          }
        }
      }
    } |}];;

let%expect_test "occurs-fixed-nested" =
  dotest @@ prog "prog"
    ~working_storage:{|
       01 A OCCURS 1.
         05 B OCCURS 2.
           10 C OCCURS 3.
             15 D OCCURS 4 PIC X.
             15 E OCCURS 5 PIC 9.
           10 F OCCURS 6 PIC A.
    |};
  [%expect {|
    prog.cob:4.7-9.31:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 A OCCURS 1.
    ----          ^^^^^^^^^^^^^^
       5 >          05 B OCCURS 2.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^
       6 >            10 C OCCURS 3.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^
       7 >              15 D OCCURS 4 PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       8 >              15 E OCCURS 5 PIC 9.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       9 >            10 F OCCURS 6 PIC A.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      10          PROCEDURE DIVISION.
      11
    Item definition: {
      table
      offset: 0
      size: 528
      range: {
        span: fixed-length: 1
      }
      field: {
        qualname: A
        leading ranges: 1
        offset: 0
        size: 528
        layout: {
          structure
          fields: {
            table
            offset: 0
            size: 528
            range: {
              span: fixed-length: 2
            }
            field: {
              qualname: B IN A
              leading ranges: 2
              offset: 0
              size: 264
              layout: {
                structure
                fields: {
                  table
                  offset: 0
                  size: 216
                  range: {
                    span: fixed-length: 3
                  }
                  field: {
                    qualname: C IN B IN A
                    leading ranges: 3
                    offset: 0
                    size: 72
                    layout: {
                      structure
                      fields: {
                        table
                        offset: 0
                        size: 32
                        range: {
                          span: fixed-length: 4
                        }
                        field: {
                          qualname: D IN C IN B IN A
                          leading ranges: 4
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
                        offset: 32
                        size: 40
                        range: {
                          span: fixed-length: 5
                        }
                        field: {
                          qualname: E IN C IN B IN A
                          leading ranges: 4
                          offset: 32
                          size: 8
                          layout: {
                            elementary
                            usage: {
                              display
                              category:
                               NUMERIC(digits = 1, scale = 0, with_sign = false)
                            }
                          }
                        }
                      }
                    }
                  }
                }{
                  table
                  offset: 216
                  size: 48
                  range: {
                    span: fixed-length: 6
                  }
                  field: {
                    qualname: F IN B IN A
                    leading ranges: 3
                    offset: 216
                    size: 8
                    layout: {
                      elementary
                      usage: {
                        display
                        category: ALPHABETIC(1)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    } |}];;

let%expect_test "occurs-depending-1" =
  dotest @@ prog "prog"
    ~working_storage:{|
       01 B-LEN PIC 9.
       01 B PIC X OCCURS 1 TO 5 DEPENDING ON B-LEN.
    |};
  [%expect {|
    prog.cob:4.7-4.22:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 B-LEN PIC 9.
    ----          ^^^^^^^^^^^^^^^
       5          01 B PIC X OCCURS 1 TO 5 DEPENDING ON B-LEN.
       6          PROCEDURE DIVISION.
    Item definition: {
      qualname: B-LEN
      offset: 0
      size: 8
      layout: {
        elementary
        usage: {
          display
          category: NUMERIC(digits = 1, scale = 0, with_sign = false)
        }
      }
    }
    prog.cob:5.7-5.51:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 B-LEN PIC 9.
       5 >        01 B PIC X OCCURS 1 TO 5 DEPENDING ON B-LEN.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    Item definition: {
      table
      offset: 0
      size: (* 8 (valof B-LEN))
      range: {
        span: {
          depending-span
          min_occurs: 1
          max_occurs: 5
          depending: B-LEN
        }
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
    } |}];;

let%expect_test "occurs-depending-2" =
  dotest @@ prog "prog"
    ~working_storage:{|
       01 B PIC X OCCURS 1 TO 5 DEPENDING ON B-LEN.
       01 B-LEN PIC 9.
    |};
  [%expect {|
    prog.cob:4.7-4.51:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 B PIC X OCCURS 1 TO 5 DEPENDING ON B-LEN.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       5          01 B-LEN PIC 9.
       6          PROCEDURE DIVISION.
    Item definition: {
      table
      offset: 0
      size: (* 8 (valof B-LEN))
      range: {
        span: {
          depending-span
          min_occurs: 1
          max_occurs: 5
          depending: B-LEN
        }
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
    prog.cob:5.7-5.22:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 B PIC X OCCURS 1 TO 5 DEPENDING ON B-LEN.
       5 >        01 B-LEN PIC 9.
    ----          ^^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    Item definition: {
      qualname: B-LEN
      offset: 0
      size: 8
      layout: {
        elementary
        usage: {
          display
          category: NUMERIC(digits = 1, scale = 0, with_sign = false)
        }
      }
    } |}];;


let%expect_test "occurs-with-index" =
  dotest @@ prog "prog"
    ~working_storage:{|
       77 V-TAB PIC X OCCURS 5 INDEXED I J.
       01 W.
         02 W-TAB PIC 9 OCCURS 42 INDEXED K L.
    |};
  [%expect {|
    prog.cob:4.7-4.43:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 V-TAB PIC X OCCURS 5 INDEXED I J.
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       5          01 W.
       6            02 W-TAB PIC 9 OCCURS 42 INDEXED K L.
    Item definition: {
      table
      offset: 0
      size: 40
      range: {
        span: fixed-length: 5
        indexes: I IN V-TAB, J IN V-TAB
      }
      field: {
        qualname: V-TAB
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
    prog.cob:5.7-6.46:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 V-TAB PIC X OCCURS 5 INDEXED I J.
       5 >        01 W.
    ----          ^^^^^
       6 >          02 W-TAB PIC 9 OCCURS 42 INDEXED K L.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    Item definition: {
      qualname: W
      offset: 0
      size: 336
      layout: {
        structure
        fields: {
          table
          offset: 0
          size: 336
          range: {
            span: fixed-length: 42
            indexes: K IN W-TAB IN W, L IN W-TAB IN W
          }
          field: {
            qualname: W-TAB IN W
            leading ranges: 1
            offset: 0
            size: 8
            layout: {
              elementary
              usage: {
                display
                category: NUMERIC(digits = 1, scale = 0, with_sign = false)
              }
            }
          }
        }
      }
    } |}];;
