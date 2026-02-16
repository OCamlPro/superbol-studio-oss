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

open Alcotest

open Cobol_ptree
open Testing_helpers.Make (Cobol_parser.INTERNAL.Dummy.Tags)
open Cobol_parser.Tokens
open Cobol_parser.INTERNAL.Grammar
open Cobol_parser.INTERNAL.Dummy

let condition: condition testable = testable pp_condition (=)
let parse_condition = parse_list_as standalone_condition
let expand_condition = Cobol_ptree.Terms_helpers.expand_every_abbrev_cond
let check_condition toks cond =
  check condition "correct conditions parsing" cond
    (expand_condition @@ parse_condition toks)
and fail_condition toks =
  check_raises "syntax-error" Error
    (fun () -> ignore @@ parse_condition toks)
;;

let test_conditions =
  let alphanum str =
    ALPHANUM { str; quotation = Double_quote; hexadecimal = false; runtime_repr = Native_bytes }
  in
  let chk descr toks cond =
    test_case descr `Quick (fun () -> check_condition toks cond)
  and fail descr toks =
    test_case (descr^" {fail}") `Quick (fun () -> fail_condition toks)
  in
  let a = WORD "A" and b = WORD "B" and c = WORD "C" and d = WORD "D"
  and a_ = alphanum "a" and b_ = alphanum "b"
  and ac = Cond.ident "A" and bc = Cond.ident "B"
  and cc = Cond.ident "C"
  and ae = Term.ident "A" and be = Term.ident "B"
  and ce = Term.ident "C" and de = Term.ident "D"
  and al = Term.strlit "a" and bl = Term.strlit "b" in
  let ae = Atom ae and be = Atom be and ce = Atom ce and de = Atom de
  and al = Atom al and bl = Atom bl (* and cl = Atom cl *)
  and one = Atom (Integer "1")
  and two = Atom (Integer "2") in
  let ( !. ) a = Not a
  and ( &&. ) a b = Logop (a, LAnd, b)
  and ( ||. ) a b = Logop (a, LOr, b)
  and ( ==. ) a b : condition = Relation (a, Eq, b)
  and ( <>. ) a b : condition = Relation (a, Ne, b)
  and ( >. ) a b : condition = Relation (a, Gt, b)
  and ( >=. ) a b : condition = Relation (a, Ge, b)
  and ( <. ) a b : condition = Relation (a, Lt, b)
  and ( <=. ) a b : condition = Relation (a, Le, b)
  and ( +. ) a b = Binop (a, BPlus, b) in
  [
    chk "NOT A"         [NOT; a]             !.ac;
    chk "(NOT A)"       [LPAR; NOT; a; RPAR] !.ac;
    chk "A OR B"        [a; OR; b]           (ac ||. bc);
    chk "A AND B"       [a; AND; b]          (ac &&. bc);
    chk "A AND NOT B"   [a; AND; NOT; b]     (ac &&. !.bc);
    chk "NOT A OR B"    [NOT; a; OR; b]      (!.ac ||. bc);
    chk "A AND B AND C" [a; AND; b; AND; c]  ((ac &&. bc) &&. cc);
    chk "A OR B AND C"  [a; OR; b; AND; c]   (ac ||. (bc &&. cc));
    chk "A = \"a\""     [a; EQ; a_]          (ae ==. al);
    chk "A OR A AND B OR C"
      [a; OR; a; AND; b; OR; c]              ((ac ||. (ac &&. bc)) ||. cc);
    chk "NOT (A AND B)"
      [NOT; LPAR; a; AND; b; RPAR]           !.(ac &&. bc);
    chk "(A = \"a\")"
      [LPAR; a; EQ; a_; RPAR]                (ae ==. al);
    chk "A = \"a\" OR NOT B"
      [a; EQ; a_; OR; NOT; b]                ((ae ==. al) ||. !.(ae ==. be));
    (* GnuCOBOL and MF do not agree on this example.
       We would need a source for an explanation of MF's behaviour. *)
    (*chk "A = \"a\" OR (NOT B)"
      [a; EQ; a_; OR; LPAR; NOT; b; RPAR]    ((ae ==. al) ||. !.(Expr be));*)
    chk "(A = \"a\") AND NOT B"
      [LPAR; a; EQ; a_; RPAR; AND; NOT; b]   ((ae ==. al) &&. !.(Expr be));
    chk "(A >= B) AND (A <= C)"
      [LPAR; a; GE; b; RPAR; AND;
       LPAR; a; LE; c; RPAR]                 ((ae >=. be) &&. (ae <=. ce));
    chk "(A >= B) AND A <= C"
      [LPAR; a; GE; b; RPAR; AND; a; LE; c]  ((ae >=. be) &&. (ae <=. ce));
    chk "(A = \"a\") AND (B = \"b\")"
      [LPAR; a; EQ; a_; RPAR; AND;
       LPAR; b; EQ; b_; RPAR]                ((ae ==. al) &&. (be ==. bl));
    chk "A = \"a\" OR B = \"b\""
      [a; EQ; a_; OR; b; EQ; b_]             ((ae ==. al) ||. (be ==. bl));
    chk "A EQUAL TO B AND C"
      [a; EQUAL; TO; b; AND; c]              ((ae ==. be) &&. (ae ==. ce));
    chk "A EQUAL TO B AND B EQUAL TO 1"
      [a; EQUAL; TO; b; AND;
       b; EQUAL; TO; DIGITS "1"]             ((ae ==. be) &&. (be ==. one));
    chk "A = 1 OR 2"
      [a; EQUAL; TO; DIGITS "1";
       OR; DIGITS "2"]                       ((ae ==. one) ||. (ae ==. two));
    chk "A = 1 OR 2 OR 2 = B"
      [a; EQ; DIGITS "1"; OR;
       DIGITS "2"; OR; DIGITS "2"; EQ; b]    (((ae ==. one) ||. (ae ==. two)) ||.
                                              (two ==. be));
    chk "A = 1 OR 1 + 1 OR 1 + 1 = B"
      [a; EQ; DIGITS "1";
       OR; DIGITS "1"; PLUS_SIGN; DIGITS "1";
       OR; DIGITS "1"; PLUS_SIGN; DIGITS "1";
       EQ; b]                                (((ae ==. one) ||. (ae ==. (one +. one))) ||.
                                               ((one +. one) ==. be));

    fail "NOT NOT A"           [NOT; NOT; a];
    fail "A AND AND B"         [a; AND; AND; b];
    fail "NOT (A AND AND B)"   [NOT; LPAR; a; AND; AND; b; RPAR];

    (* ISO COBOL2014 examples *)
    chk "a > b AND NOT < c OR d"
      [a; GT; b; AND; NOT; LT; c; OR; d]     (((ae >. be) &&. (ae >=. ce)) ||.
                                              (ae >=. de));
    chk "a NOT EQUAL b OR c"
      [a; NOT; EQUAL; b; OR; c]              ((ae <>. be) ||. (ae <>. ce));
    chk "NOT a = b OR c"
      [NOT; a; EQ; b; OR; c]                 (!.(ae ==. be) ||. (ae ==. ce));
    chk "NOT (a > b OR < c)"
      [NOT; LPAR; a; GT; b; OR; LT; c; RPAR] !.((ae >. be) ||. (ae <. ce));
    chk "NOT (a NOT > b AND c AND NOT d)"
      [NOT; LPAR; a; NOT; GT; b;
       AND; c; AND; NOT; d; RPAR]            !.(((ae <=. be) &&. (ae <=. ce)) &&.
                                                !.(ae <=. de));

    (* MicroFocus OSVS: https://www.microfocus.com/documentation/reuze/60d/lhpdf60q.htm *)
    chk "a = (1 OR 2)"
      [a; EQUAL; LPAR; DIGITS "1"; OR; DIGITS "2"; RPAR]
      ((ae ==. one) ||. (ae ==. two));
    chk "a > b OR (c AND d)"
      [a; GT; b; OR; c; AND; d]
      ((ae >. be) ||. ((ae >. ce) &&. (ae >. de)));
    chk "a > (b OR c) AND d"
      [a; GT; LPAR; b; OR; c; RPAR; AND; d]
      (((ae >. be) ||. (ae >. ce)) &&. (ae >. de));
    chk "a (= b OR > c)"
      [a; LPAR_BEFORE_RELOP; EQUAL; b; OR; GT; c; RPAR]
      ((ae ==. be) ||. (ae >. ce));
    chk "a = b AND (> c OR < d)"
      [a; EQUAL; b; AND; LPAR_BEFORE_RELOP; GT; c; OR; LT; d; RPAR]
      ((ae ==. be) &&. ((ae >. ce) ||. (ae <. de)));
  ];;

(* --- *)

run "combined relations parsing"
  [
    "conditions", test_conditions;
  ];;
