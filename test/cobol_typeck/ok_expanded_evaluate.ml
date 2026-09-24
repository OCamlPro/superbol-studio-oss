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

(* Selection objects of an EVALUATE are abbreviated conditions whose implied
   subject and relational operator come from the matching selection subject, so
   they expand through [expand_selection_object] rather than through
   [expand_condition]. *)

open Prog_printer

let dotest = Typeck_testing.show_expanded_conditions

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

let%expect_test "value-subject-with-value-object" =
  check "A" ["1"];
  [%expect {|
    SUBJECT A
    WHEN 1 |}];;

let%expect_test "value-subject-with-negated-value-object" =
  check "A" ["NOT 1"];
  [%expect {|
    SUBJECT A
    WHEN NOT 1 |}];;

let%expect_test "value-subject-with-relop-object" =
  check "A" ["> 2"];
  [%expect {|
    SUBJECT A
    WHEN A > 2 |}];;

let%expect_test "value-subject-with-abbreviated-object" =
  check "A" ["> 2 AND < 4"];
  [%expect {|
    SUBJECT A
    WHEN (A > 2) AND (A < 4) |}];;

let%expect_test "value-subject-with-range-object" =
  check "A" ["1 THROUGH 3"];
  [%expect {|
    SUBJECT A
    WHEN 1 THROUGH 3 |}];;

let%expect_test "value-subject-with-any-object" =
  check "A" ["ANY"];
  [%expect {|
    SUBJECT A
    WHEN ANY |}];;

let%expect_test "true-subject-with-88-cond-object" =
  check "TRUE" ["A-IS-1"];
  [%expect {|
    SUBJECT TRUE
    WHEN A-IS-1 |}];;

let%expect_test "true-subject-with-relation-object" =
  check "TRUE" ["A > B"];
  [%expect {|
    SUBJECT TRUE
    WHEN A > B |}];;

let%expect_test "condition-subject-with-const-objects" =
  check "A > B" ["TRUE"; "FALSE"];
  [%expect {|
    SUBJECT A > B
    WHEN TRUE
    WHEN FALSE |}];;

let%expect_test "88-cond-subject" =
  check "A-IS-1" ["TRUE"];
  [%expect {|
    SUBJECT A-IS-1
    WHEN TRUE |}];;

let%expect_test "also-subjects" =
  check "A ALSO B" ["1 ALSO > 2"; "ANY ALSO ANY"];
  [%expect {|
    SUBJECT A ALSO B
    WHEN 1 ALSO B > 2
    WHEN ANY ALSO ANY |}];;

let%expect_test "several-branches" =
  check "A" ["1"; "< 3"];
  [%expect {|
    SUBJECT A
    WHEN 1
    WHEN A < 3 |}];;

(* --- GnuCOBOL testsuite, [EVALUATE conditions] in
       tests/testsuite.src/run_fundamental.at --- *)

let%expect_test "gc-negated-relop-object" =
  check "A" ["NOT > 2"];
  [%expect {|
    SUBJECT A
    WHEN A <= 2 |}];;

let%expect_test "gc-negated-range-object" =
  check "A" ["NOT 1 THROUGH 3"];
  [%expect {|
    SUBJECT A
    WHEN NOT 1 THROUGH 3 |}];;

let%expect_test "gc-class-object" =
  check "A" ["NUMERIC"];
  [%expect {|
    SUBJECT A
    WHEN A NUMERIC |}];;

let%expect_test "gc-negated-class-object" =
  check "A" ["IS NOT NUMERIC"];
  [%expect {|
    SUBJECT A
    WHEN NOT (A NUMERIC) |}];;

let%expect_test "gc-sign-object" =
  check "A" ["POSITIVE"];
  [%expect {|
    SUBJECT A
    WHEN A POSITIVE |}];;

let%expect_test "gc-negated-sign-object" =
  check "A" ["IS NOT ZERO"];
  [%expect {|
    SUBJECT A
    WHEN NOT (A ZERO) |}];;

let%expect_test "gc-or-of-value-objects" =
  check "A" ["1 OR 3"];
  [%expect {|
    SUBJECT A
    WHEN (A = 1) OR (A = 3) |}];;

let%expect_test "gc-parenthesized-objects" =
  check "A" ["(1 OR 3) AND NOT 4"];
  [%expect {|
    SUBJECT A
    WHEN ((A = 1) OR (A = 3)) AND (NOT (A = 4)) |}];;

let%expect_test "gc-false-subject" =
  check "FALSE" ["A < 2"];
  [%expect {|
    SUBJECT FALSE
    WHEN A < 2 |}];;

let%expect_test "gc-true-subject-with-combined-condition-object" =
  check "TRUE" ["(A = 1 OR 3) AND B = 2"];
  [%expect {|
    SUBJECT TRUE
    WHEN ((A = 1) OR (A = 3)) AND (B = 2) |}];;

let%expect_test "gc-combined-condition-subject" =
  check "(A = 1 OR 3) AND B = 2" ["TRUE"];
  [%expect {|
    SUBJECT ((A = 1) OR (A = 3)) AND (B = 2)
    WHEN TRUE |}];;

let%expect_test "gc-condition-subject-with-condition-object" =
  check "(A = 1 OR 3)" ["B = 2"];
  [%expect {|
    SUBJECT (A = 1) OR (A = 3)
    WHEN B = 2 |}];;

let%expect_test "gc-relation-subject-with-relation-object" =
  check "A = 2" ["B = 3"];
  [%expect {|
    SUBJECT A = 2
    WHEN B = 3 |}];;
