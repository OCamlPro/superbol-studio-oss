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
       MAIN-1 SECTION.
       SUB-1.
          DISPLAY 1.
       MAIN-2 SECTION.
       SUB-1.
          PERFORM SUB-1.
    |};
  [%expect {|
    prog.cob:9.18-9.23:
       6             DISPLAY 1.
       7          MAIN-2 SECTION.
       8          SUB-1.
       9 >           PERFORM SUB-1.
    ----                     ^^^^^
      10
    >> Error: Ambiguous procedure-name 'SUB-1'; known matching names are 'SUB-1
              IN MAIN-2', 'SUB-1 IN MAIN-1' |}];;
