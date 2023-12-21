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

let%expect_test "ambiguous-proc-names" =
  dotest @@ prog "ambiguous-proc-names"
    ~procedure:{|
       MAIN SECTION.
          PERFORM SUB-1.
       MAIN-1 SECTION.
       SUB-1.
          DISPLAY 1.
       MAIN-2 SECTION.
       SUB-1.
          DISPLAY 2.
    |};
  [%expect {|
    prog.cob:5.18-5.23:
       2          DATA DIVISION.
       3          PROCEDURE DIVISION.
       4          MAIN SECTION.
       5 >           PERFORM SUB-1.
    ----                     ^^^^^
       6          MAIN-1 SECTION.
       7          SUB-1.
    >> Error: Ambiguous procedure-name 'SUB-1'; known matching names are 'SUB-1
              IN MAIN-2', 'SUB-1 IN MAIN-1' |}];;
