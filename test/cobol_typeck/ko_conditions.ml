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

let%expect_test "misplaced-condition" =
  dotest @@ prog "misplaced-condition"
    ~working_storage:{|
       88 X-IS-A VALUE "A".
       77 X PIC X.
    |};
  [%expect {|
    prog.cob:4.7-4.27:
       1          PROGRAM-ID. misplaced-condition.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        88 X-IS-A VALUE "A".
    ----          ^^^^^^^^^^^^^^^^^^^^
       5          77 X PIC X.
       6          PROCEDURE DIVISION.
    >> Error: Misplaced condition-name entry following no definition |}];;
