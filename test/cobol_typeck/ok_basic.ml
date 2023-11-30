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

let%expect_test "basics" =
  dotest @@ prog "prog"
    ~working_storage:{|
       01 X PIC X.
    |}
    ~local_storage:{|
       01 Y PIC X.
    |};
  [%expect {|
    prog.cob:4.7-4.18:
       1          PROGRAM-ID. prog.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 X PIC X.
    ----          ^^^^^^^^^^^
       5          LOCAL-STORAGE SECTION.
       6          01 Y PIC X.
    Item definition: {
      qualname: X
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
    prog.cob:6.7-6.18:
       3          WORKING-STORAGE SECTION.
       4          01 X PIC X.
       5          LOCAL-STORAGE SECTION.
       6 >        01 Y PIC X.
    ----          ^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    Item definition: {
      qualname: Y
      offset: 0
      size: 1
      layout: {
        elementary
        usage: {
          display (dev: temporary)
          category: ALPHANUMERIC(1)
        }
      }
    } |}];;


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
      qualname: B
      offset: 0
      size: 1
      layout: {
        fixed-length table
        length: 5
        items: {
          filler
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
      qualname: TAB
      offset: 0
      size: 4
      layout: {
        fixed-length table
        length: 42
        items: {
          filler
          offset: 0
          size: 2
          layout: {
            elementary
            usage: {
              display (dev: temporary)
              category: ALPHANUMERIC-EDITED(2)
            }
          }
        }{
          filler
          offset: 2
          size: 2
          layout: {
            elementary
            usage: {
              display (dev: temporary)
              category: NUMERIC(digits = 2, scale = 1, with_sign = false)
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
      size: 1
      layout: {
        elementary
        usage: {
          display (dev: temporary)
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
      filler
      offset: 0
      size: (valof B-LEN)
      layout: {
        variable-length table
        min_occurs: 1
        max_occurs: 5
        depending: B-LEN
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
      filler
      offset: 0
      size: (valof B-LEN)
      layout: {
        variable-length table
        min_occurs: 1
        max_occurs: 5
        depending: B-LEN
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
      size: 1
      layout: {
        elementary
        usage: {
          display (dev: temporary)
          category: NUMERIC(digits = 1, scale = 0, with_sign = false)
        }
      }
    } |}];;
