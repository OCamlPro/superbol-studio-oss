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

(* Only the diagnostics are of interest here: a selection subject or object that
   fails to expand has no expansion to show. *)
let dotest = Typeck_testing.show_diagnostics

(* Same data division as [Ok_expanded_conditions]: A..D are plain data items,
   A-IS-1..A-IS-4 are condition-names. *)
let items = {|
       77 A PIC 9(4).
       88 A-IS-1 VALUE 1.
       88 A-IS-2 VALUE 2.
       88 A-IS-3 VALUE 3.
       88 A-IS-4 VALUE 4.
       77 B PIC 9(4).
       77 C PIC 9(4).
       77 D PIC 9(4).
    |}

(** [check subject whens] evaluates [subject] with one {v WHEN v} branch per
    element of [whens]. *)
let check ?(working_storage = items) subject whens =
  let lines =
    ("EVALUATE " ^ subject) ::
    List.map (fun objects -> "WHEN " ^ objects ^ " CONTINUE") whens @
    ["END-EVALUATE."]
  in
  dotest @@ prog "eval" ~working_storage
    ~procedure:(String.concat "\n           " lines)

let%expect_test "range-without-value-subject" =
  check "TRUE" ["1 THROUGH 3"];
  [%expect {|
    prog.cob:14.16-14.27:
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13          EVALUATE TRUE
      14 >            WHEN 1 THROUGH 3 CONTINUE
    ----                   ^^^^^^^^^^^
      15              END-EVALUATE.
      16
    >> Error: Invalid range in WHEN clause without an expression EVALUATE subject. |}];;

let%expect_test "too-few-objects" =
  check "A ALSO B" ["1"];
  [%expect {|
    prog.cob:14.16-14.17:
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13          EVALUATE A ALSO B
      14 >            WHEN 1 CONTINUE
    ----                   ^
      15              END-EVALUATE.
      16
    >> Error: Invalid number of selection objects (expecting 2). |}];;

let%expect_test "too-many-objects" =
  check "A" ["1 ALSO 2"];
  [%expect {|
    prog.cob:14.16-14.24:
      11          77 D PIC 9(4).
      12          PROCEDURE DIVISION.
      13          EVALUATE A
      14 >            WHEN 1 ALSO 2 CONTINUE
    ----                   ^^^^^^^^
      15              END-EVALUATE.
      16
    >> Error: Invalid number of selection objects (expecting 1). |}];;

(* As in [Ko_expanded_conditions], an unresolved subject is silent, and its
   objects are then left alone rather than reported against a subject that is
   already known to be invalid. *)
let%expect_test "unknown-subject-name" =
  check "UNDECLARED" ["1"];
  [%expect {| |}];;
