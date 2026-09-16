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

let%expect_test "fd" =
  dotest @@ prog "prog"
    ~file_section:{|
       FD file-name.
       01 X PIC X.
       FD file-name-varying
          RECORD VARYING FROM 2 TO 42 CHARACTERS.
       01 FILLER.
         05 C PIC 99 VALUE 0.
         05 Y PIC X OCCURS 0 TO 40 DEPENDING ON C.
    |};
  [%expect {|
    prog.cob:5.7-5.18:
       2          DATA DIVISION.
       3          FILE SECTION.
       4          FD file-name.
       5 >        01 X PIC X.
    ----          ^^^^^^^^^^^
       6          FD file-name-varying
       7             RECORD VARYING FROM 2 TO 42 CHARACTERS.
    File storage: {
      file
      name: file-name
    }
    prog.cob:5.7-5.18:
       2          DATA DIVISION.
       3          FILE SECTION.
       4          FD file-name.
       5 >        01 X PIC X.
    ----          ^^^^^^^^^^^
       6          FD file-name-varying
       7             RECORD VARYING FROM 2 TO 42 CHARACTERS.
    Item definition: {
      qualname: X
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
    prog.cob:8.7-10.50:
       5          01 X PIC X.
       6          FD file-name-varying
       7             RECORD VARYING FROM 2 TO 42 CHARACTERS.
       8 >        01 FILLER.
    ----          ^^^^^^^^^^
       9 >          05 C PIC 99 VALUE 0.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      10 >          05 Y PIC X OCCURS 0 TO 40 DEPENDING ON C.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      11          PROCEDURE DIVISION.
      12
    File storage: {
      file
      name: file-name-varying
      record-size: {
        varying
        min: 2
        max: 42
      }
    }
    prog.cob:8.7-10.50:
       5          01 X PIC X.
       6          FD file-name-varying
       7             RECORD VARYING FROM 2 TO 42 CHARACTERS.
       8 >        01 FILLER.
    ----          ^^^^^^^^^^
       9 >          05 C PIC 99 VALUE 0.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      10 >          05 Y PIC X OCCURS 0 TO 40 DEPENDING ON C.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      11          PROCEDURE DIVISION.
      12
    Item definition: {
      filler
      offset: 0
      size: (+ 16 (* 8 (valof C)))
      layout: {
        structure
        fields: {
          qualname: C
          offset: 0
          size: 16
          layout: {
            elementary
            usage: {
              display
              category: NUMERIC(digits = 2, scale = 0, sign = unsigned)
            }
            value: 0
          }
        }{
          table
          offset: 16
          size: (* 8 (valof C))
          range: {
            span: {
              depending-span
              min_occurs: 0
              max_occurs: 40
              depending: C
            }
          }
          field: {
            qualname: Y
            leading ranges: 1
            offset: 16
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
