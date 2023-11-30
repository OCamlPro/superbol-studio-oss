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

let%expect_test "no-numeric-edited-pic-for-bin-usage" =
  dotest @@ prog "no-numeric-edited-pic-for-bin-usage"
    ~working_storage:{|
       77 A PIC 9/9 USAGE BINARY.
       77 B PIC 9,9 USAGE COMP.
       77 C PIC $$99999 USAGE PACKED-DECIMAL.
    |};
  [%expect {| |}];;                                           (* should error *)
