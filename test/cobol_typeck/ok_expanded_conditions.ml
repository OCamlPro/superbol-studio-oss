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

let dotest = Typeck_testing.show_expanded_conditions

(* A..D are plain data items, so they are relation objects; A-IS-1..A-IS-4 are
   condition-names, so they are boolean operands.  Having both in the same data
   division lets every case below share this working-storage. *)
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

let check ?(working_storage = items) cond =
  dotest @@ prog "cond" ~working_storage
    ~procedure:(Pretty.to_string "IF %s CONTINUE END-IF." cond)

(* --- Boolean operands (condition-names) --- *)

let%expect_test "not-88-cond" =
  check "NOT A-IS-1";
  [%expect {| NOT (A-IS-1) |}];;

let%expect_test "parenthesized-not-88-cond" =
  check "(NOT A-IS-1)";
  [%expect {| NOT (A-IS-1) |}];;

let%expect_test "or-of-88-conds" =
  check "A-IS-1 OR A-IS-2";
  [%expect {| (A-IS-1) OR (A-IS-2) |}];;

let%expect_test "and-of-88-conds" =
  check "A-IS-1 AND A-IS-2";
  [%expect {| (A-IS-1) AND (A-IS-2) |}];;

let%expect_test "and-with-negated-88-cond" =
  check "A-IS-1 AND NOT A-IS-2";
  [%expect {| (A-IS-1) AND (NOT (A-IS-2)) |}];;

let%expect_test "not-binds-tighter-than-or" =
  check "NOT A-IS-1 OR A-IS-2";
  [%expect {| (NOT (A-IS-1)) OR (A-IS-2) |}];;

let%expect_test "and-is-left-associative" =
  check "A-IS-1 AND A-IS-2 AND A-IS-3";
  [%expect {| ((A-IS-1) AND (A-IS-2)) AND (A-IS-3) |}];;

let%expect_test "and-binds-tighter-than-or" =
  check "A-IS-1 OR A-IS-2 AND A-IS-3";
  [%expect {| (A-IS-1) OR ((A-IS-2) AND (A-IS-3)) |}];;

let%expect_test "mixed-and-or-precedence" =
  check "A-IS-1 OR A-IS-1 AND A-IS-2 OR A-IS-3";
  [%expect {| ((A-IS-1) OR ((A-IS-1) AND (A-IS-2))) OR (A-IS-3) |}];;

let%expect_test "not-over-parenthesized-and" =
  check "NOT (A-IS-1 AND A-IS-2)";
  [%expect {| NOT ((A-IS-1) AND (A-IS-2)) |}];;

(* --- Relations and abbreviations --- *)

let%expect_test "simple-relation" =
  check "A = 1";
  [%expect {| A = 1 |}];;

let%expect_test "parenthesized-relation" =
  check "(A = 1)";
  [%expect {| A = 1 |}];;

let%expect_test "implied-subject-n-relop-after-not" =
  check "A = 1 OR NOT B";
  [%expect {| (A = 1) OR (NOT (A = B)) |}];;

(* GnuCOBOL and MF do not agree on this example.
   I guess there is a parsing bug in MF where this seems to be 
   interpreted as "(A = 1) OR (NOT (B <> 0))". *)
let%expect_test "implied-subject-in-parenthesized-not" =
  check "A = 1 OR (NOT B)";
  [%expect {| (A = 1) OR (NOT (A = B)) |}];;

let%expect_test "parens-reset-implied-subject" =
  check "(A = 1) AND NOT A-IS-2";
  [%expect {| (A = 1) AND (NOT (A-IS-2)) |}];;

let%expect_test "two-parenthesized-relations" =
  check "(A >= B) AND (A <= C)";
  [%expect {| (A >= B) AND (A <= C) |}];;

let%expect_test "relation-after-parenthesized-relation" =
  check "(A >= B) AND A <= C";
  [%expect {| (A >= B) AND (A <= C) |}];;

let%expect_test "independent-parenthesized-relations" =
  check "(A = 1) AND (B = 2)";
  [%expect {| (A = 1) AND (B = 2) |}];;

let%expect_test "independent-relations" =
  check "A = 1 OR B = 2";
  [%expect {| (A = 1) OR (B = 2) |}];;

let%expect_test "implied-subject-n-relop" =
  check "A EQUAL TO B AND C";
  [%expect {| (A = B) AND (A = C) |}];;

let%expect_test "implied-subject-overridden-by-full-relation" =
  check "A EQUAL TO B AND B EQUAL TO 1";
  [%expect {| (A = B) AND (B = 1) |}];;

let%expect_test "implied-subject-n-relop-with-literal-objects" =
  check "A EQUAL TO 1 OR 2";
  [%expect {| (A = 1) OR (A = 2) |}];;

let%expect_test "implied-parts-reset-by-full-relation" =
  check "A = 1 OR 2 OR 2 = B";
  [%expect {| ((A = 1) OR (A = 2)) OR (2 = B) |}];;

let%expect_test "implied-parts-with-arithmetic-objects" =
  check "A = 1 OR 1 + 1 OR 1 + 1 = B";
  [%expect {| ((A = 1) OR (A = 1 + 1)) OR (1 + 1 = B) |}];;

let%expect_test "class-cond" =
  check "A IS NUMERIC";
  [%expect {| A NUMERIC |}];;

let%expect_test "class-cond-with-implied-subject" =
  check "A = B AND NUMERIC";
  [%expect {| (A = B) AND (A NUMERIC) |}];;

let%expect_test "sign-cond-with-implied-subject" =
  check "A = B AND POSITIVE";
  [%expect {| (A = B) AND (A POSITIVE) |}];;

(* --- ISO COBOL2014 examples --- *)

let%expect_test "cobol2014-implied-subject-with-negated-relop" =
  check "A > B AND NOT < C OR D";
  [%expect {| ((A > B) AND (A >= C)) OR (A >= D) |}];;

let%expect_test "cobol2014-implied-negated-relop" =
  check "A NOT EQUAL B OR C";
  [%expect {| (A <> B) OR (A <> C) |}];;

let%expect_test "cobol2014-not-over-relation" =
  check "NOT A = B OR C";
  [%expect {| (NOT (A = B)) OR (A = C) |}];;

let%expect_test "cobol2014-not-over-parenthesized-abbreviation" =
  check "NOT (A > B OR < C)";
  [%expect {| NOT ((A > B) OR (A < C)) |}];;

let%expect_test "cobol2014-nested-negations" =
  check "NOT (A NOT > B AND C AND NOT D)";
  [%expect {| NOT (((A <= B) AND (A <= C)) AND (NOT (A <= D))) |}];;

(* --- MicroFocus OSVS:
       https://www.microfocus.com/documentation/reuze/60d/lhpdf60q.htm --- *)

let%expect_test "mf-parenthesized-objects" =
  check "A = (1 OR 2)";
  [%expect {| (A = 1) OR (A = 2) |}];;

let%expect_test "mf-implied-subject-in-parenthesized-objects" =
  check "A > B OR (C AND D)";
  [%expect {| (A > B) OR ((A > C) AND (A > D)) |}];;

let%expect_test "mf-implied-parts-survive-parenthesized-objects" =
  check "A > (B OR C) AND D";
  [%expect {| ((A > B) OR (A > C)) AND (A > D) |}];;

let%expect_test "mf-parenthesized-relops" =
  check "A (= B OR > C)";
  [%expect {| (A = B) OR (A > C) |}];;

let%expect_test "mf-implied-subject-with-parenthesized-relops" =
  check "A = B AND (> C OR < D)";
  [%expect {| (A = B) AND ((A > C) OR (A < D)) |}];;

(* --- GnuCOBOL testsuite, tests/testsuite.src/run_fundamental.at --- *)

(* [Abbreviated Expressions] *)

let%expect_test "gc-implied-parts-over-several-objects" =
  check "A > B AND C AND D";
  [%expect {| ((A > B) AND (A > C)) AND (A > D) |}];;

let%expect_test "gc-implied-parts-across-and-or" =
  check "A > B AND C OR D";
  [%expect {| ((A > B) AND (A > C)) OR (A > D) |}];;

let%expect_test "gc-literal-subject-with-implied-parts" =
  check "1 > 2 AND 3 AND 4";
  [%expect {| ((1 > 2) AND (1 > 3)) AND (1 > 4) |}];;

let%expect_test "gc-literal-subject-with-implied-relop-change" =
  check "1 > 2 AND < 3 OR 4";
  [%expect {| ((1 > 2) AND (1 < 3)) OR (1 < 4) |}];;

(* [abbreviated conditions] *)

let%expect_test "gc-parenthesized-and-objects" =
  check "A = (B AND C AND D)";
  [%expect {| ((A = B) AND (A = C)) AND (A = D) |}];;

let%expect_test "gc-negated-relop-with-parenthesized-objects" =
  check "A NOT = (B AND C AND D)";
  [%expect {| ((A <> B) AND (A <> C)) AND (A <> D) |}];;

(* [abbreviated conditions with new subject in parens] *)

let%expect_test "gc-new-subject-inside-parenthesized-objects" =
  check "A = 1 OR (2 AND B = 3)";
  [%expect {| (A = 1) OR ((A = 2) AND (B = 3)) |}];;

(* [AND and OR precedence with parenthesized conditions] *)

let%expect_test "gc-parenthesized-relation-then-abbreviation" =
  check "(A = 1) AND B = 2 OR (A = 3)";
  [%expect {| ((A = 1) AND (B = 2)) OR (A = 3) |}];;

(* [paren before relop in abbreviated conditions] *)

let%expect_test "gc-negated-parenthesized-relops" =
  check "A NOT (< 1 OR > 2)";
  [%expect {| NOT ((A < 1) OR (A > 2)) |}];;

let%expect_test "gc-is-not-before-parenthesized-relop" =
  check "A IS (NOT < 1)";
  [%expect {| A >= 1 |}];;

let%expect_test "gc-nested-parenthesized-relops" =
  check "A ((<= 1) OR (> 2))";
  [%expect {| (A <= 1) OR (A > 2) |}];;

let%expect_test "gc-not-over-parenthesized-relops" =
  check "NOT A (= 1 OR = 2)";
  [%expect {| NOT ((A = 1) OR (A = 2)) |}];;

let%expect_test "gc-not-over-parenthesized-objects" =
  check "NOT A = (1 OR 2)";
  [%expect {| NOT ((A = 1) OR (A = 2)) |}];;

(* [abbreviated class and sign conditions] *)

let%expect_test "gc-sign-cond" =
  check "A POSITIVE";
  [%expect {| A POSITIVE |}];;

let%expect_test "gc-negated-sign-cond" =
  check "A NOT ZERO";
  [%expect {| NOT (A ZERO) |}];;

(* Without IS, ZERO stays an operand and inherits the relational operator... *)
let%expect_test "gc-zero-as-implied-object" =
  check "A > 7 OR ZERO";
  [%expect {| (A > 7) OR (A > ZERO) |}];;

(* ...whereas with IS it is a sign condition on the implied subject. *)
let%expect_test "gc-zero-as-sign-cond" =
  check "A > 7 OR IS ZERO";
  [%expect {| (A > 7) OR (A ZERO) |}];;

let%expect_test "gc-negated-class-cond-with-implied-subject" =
  check "A = 1 AND NOT NUMERIC";
  [%expect {| (A = 1) AND (NOT (A NUMERIC)) |}];;

let%expect_test "gc-not-over-class-cond" =
  check "NOT A NUMERIC";
  [%expect {| NOT (A NUMERIC) |}];;

let%expect_test "gc-new-subject-with-sign-cond" =
  check "A = 1 AND A NOT NEGATIVE";
  [%expect {| (A = 1) AND (NOT (A NEGATIVE)) |}];;
