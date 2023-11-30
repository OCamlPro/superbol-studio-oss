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

let dotest = Typeck_testing.show_diagnostics

let%expect_test "missing-spec" =
  dotest @@ prog "missing-spec"
    ~working_storage:{|
       77 A.
       77 B OCCURS 5.
    |};
  [%expect {|
    prog.cob:4.7-4.12:
       1          PROGRAM-ID. missing-spec.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A.
    ----          ^^^^^
       5          77 B OCCURS 5.
       6          PROCEDURE DIVISION.
    >> Error: Missing PICTURE clause for item 'A'

    prog.cob:5.7-5.21:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A.
       5 >        77 B OCCURS 5.
    ----          ^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    >> Error: Missing PICTURE clause for item 'B' |}];;
