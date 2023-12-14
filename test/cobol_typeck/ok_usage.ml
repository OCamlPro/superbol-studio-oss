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
