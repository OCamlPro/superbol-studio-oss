(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(* Checks that the traversals of conditions and expressions by the parse-tree
   and procedure visitors, that unroll chains of combined conditions and binary
   operations, behave as plain recursive traversals. *)

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX

module P = Cobol_ptree
module T = Cobol_procedure.Types

let id_of loc = string_of_int (Cobol_common.Srcloc.start_pos loc).pos_cnum

(* Logs each handler call and post action in the accumulator; the action taken
   depends on the number of calls so far. *)
let logger ~seed =
  let calls = ref 0 in
  fun event x ->
    incr calls;
    let x = event :: x in
    match (!calls * seed) mod 7 with
    | 0 -> SkipChildren ("skip" :: x)
    | 1 | 2 | 3 -> DoChildrenAndThen (x, List.cons ("post " ^ event))
    | _ -> DoChildren x

let show_logop: P.logop -> _ = function LAnd -> "and" | LOr -> "or"
let gen_logop rand : P.logop = if Random.State.bool rand then LAnd else LOr

let show_binop: P.binop -> _ = function
  | BPlus -> "+" | BMinus -> "-" | BMul -> "*" | BDiv -> "/" | BPow -> "**"
  | BAnd -> "and" | BOr -> "or" | BXor -> "xor"
let gen_binop rand : P.binop =
  P.[| BPlus; BMinus; BMul; BDiv; BPow |].(Random.State.int rand 5)

let show_unop: P.unop -> _ = function
  | UPlus -> "+" | UMinus -> "-" | UNot -> "not"

let gen ~rand ~leaf ~not_ ~paren ~combined ~op depth =
  let id = ref 0 in
  let with_loc c =
    incr id;
    let pos = Lexing.{ dummy_pos with pos_cnum = !id } in
    c &@ Cobol_common.Srcloc.raw (pos, pos)
  in
  let rec gen depth =
    if depth <= 0 then with_loc leaf
    else with_loc @@ match Random.State.int rand 10 with
      | 0 -> not_ (gen (depth - 1))
      | 1 -> paren (gen (depth - 1))
      | 2 -> combined (gen (small depth)) (op rand) (gen (depth - 1))
      | _ -> combined (gen (depth - 1)) (op rand) (gen (small depth))
  and small depth = min 2 (depth - 1) in
  gen depth

let check_equivalence ~gen ~checks =
  let rand = Random.State.make [| 42 |] in
  let runs = ref 0 and mismatches = ref 0 and events = ref 0 in
  for depth = 0 to 30 do
    for seed = 1 to 20 do
      let c = gen ~rand depth in
      List.iter begin fun (fold, reference) ->
        let log = fold ~seed c and ref_log = reference ~seed c in
        incr runs;
        events := !events + List.length log;
        if log <> ref_log then incr mismatches
      end checks
    done
  done;
  Pretty.out "runs: %d, events: %d, mismatches: %d@." !runs !events !mismatches

(* --- *)

let rec ptree_reference_condition v =
  handle v#fold_condition ~continue:begin fun c x -> match c with
    | P.CondCombined (c, l, d) -> x
        >> ptree_reference_condition' v c
        >> P.Terms_visitor.fold_logop v l
        >> ptree_reference_condition' v d
    | CondParen c | CondNot c -> x
        >> ptree_reference_condition' v c
    | CondOmitted -> x
    | _ -> assert false
  end
and ptree_reference_condition' v =
  handle' v#fold_condition' ~fold:ptree_reference_condition v

let ptree_folder ~seed =
  let log = logger ~seed in
  object
    inherit [string list] P.Visitor.folder
    method! fold' { loc; _ } = log ("with_loc " ^ id_of loc)
    method! fold_condition' { loc; _ } = log ("condition' " ^ id_of loc)
    method! fold_condition c = log @@ "condition " ^ match c with
      | CondCombined (_, l, _) -> show_logop l
      | CondParen _ -> "paren"
      | CondNot _ -> "not"
      | CondOmitted -> "omitted"
      | _ -> assert false
    method! fold_logop l = log ("logop " ^ show_logop l)
  end

let%expect_test "ptree-condition-visitor" =
  let fold f ~seed c = f (ptree_folder ~seed) c [] in
  check_equivalence
    ~gen:(gen ~leaf:P.CondOmitted
            ~not_:(fun c -> P.CondNot c) ~paren:(fun c -> P.CondParen c)
            ~combined:(fun c l d -> P.CondCombined (c, l, d)) ~op:gen_logop)
    ~checks:[
      fold P.Terms_visitor.fold_condition',
      fold ptree_reference_condition';
      fold (fun v c -> P.Terms_visitor.fold_condition v ~&c),
      fold (fun v c -> ptree_reference_condition v ~&c);
    ];
  [%expect {| runs: 1240, events: 42670, mismatches: 0 |}]

(* --- *)

let rec procedure_reference_condition v =
  handle v#fold_condition ~continue:begin fun c x -> match c with
    | T.Combined (c, o, c') -> x
        >> procedure_reference_condition_or_error' v c
        >> P.Terms_visitor.fold_logop v o
        >> procedure_reference_condition_or_error' v c'
    | Not c -> x
        >> procedure_reference_condition_or_error' v c
    | _ -> assert false
  end
and procedure_reference_condition' v =
  handle' v#fold_condition' ~fold:procedure_reference_condition v
and procedure_reference_condition_or_error' v =
  handle v#fold_condition_or_error'
    ~continue:(Cobol_procedure.Visitor.fold_or_error' v
                 ~fold:procedure_reference_condition')

let procedure_folder ~seed =
  let log = logger ~seed in
  object
    inherit [string list] Cobol_procedure.Visitor.folder
    method! fold' { loc; _ } = log ("with_loc " ^ id_of loc)
    method! fold_condition' { loc; _ } = log ("condition' " ^ id_of loc)
    method! fold_condition_or_error' { loc; payload } =
      log @@ "condition_or_error " ^ id_of loc ^
             if Result.is_ok payload then " ok" else " error"
    method! fold_condition c = log @@ "condition " ^ match c with
      | Combined (_, l, _) -> show_logop l
      | Not _ -> "not"
      | _ -> assert false
    method! fold_logop l = log ("logop " ^ show_logop l)
  end

let%expect_test "procedure-condition-visitor" =
  let fold f ~seed c = f (procedure_folder ~seed) c []
  and on_ok f v (c: T.condition T.or_error with_loc) x = match ~&c with
    | Ok p -> f v p x
    | Error () -> x
  and on_ok' f v (c: T.condition T.or_error with_loc) x = match ~&c with
    | Ok p -> f v (p &@<- c) x
    | Error () -> x
  in
  check_equivalence
    ~gen:(gen ~leaf:(Error ())
            ~not_:(fun c -> Ok (T.Not c)) ~paren:(fun c -> Ok (T.Not c))
            ~combined:(fun c o c' -> Ok (T.Combined (c, o, c')))
            ~op:gen_logop)
    ~checks:[
      fold Cobol_procedure.Visitor.fold_condition_or_error',
      fold procedure_reference_condition_or_error';
      fold (on_ok Cobol_procedure.Visitor.fold_condition),
      fold (on_ok procedure_reference_condition);
      fold (on_ok' Cobol_procedure.Visitor.fold_condition'),
      fold (on_ok' procedure_reference_condition');
    ];
  [%expect {| runs: 1860, events: 51369, mismatches: 0 |}]

(* --- *)

let rec ptree_reference_expr v =
  handle v#fold_expr ~continue:begin fun e x -> match e with
    | P.Atom a -> x
        >> P.Terms_visitor.fold_scalar v a
    | Unop (o, e) -> x
        >> P.Terms_visitor.fold_unop v o
        >> ptree_reference_expr' v e
    | Binop (e, o, e') -> x
        >> ptree_reference_expr' v e
        >> P.Terms_visitor.fold_binop v o
        >> ptree_reference_expr' v e'
  end
and ptree_reference_expr' v =
  handle' v#fold_expr' ~fold:ptree_reference_expr v

let ptree_expr_folder ~seed =
  let log = logger ~seed in
  object
    inherit [string list] P.Visitor.folder
    method! fold' { loc; _ } = log ("with_loc " ^ id_of loc)
    method! fold_expr' { loc; _ } = log ("expr' " ^ id_of loc)
    method! fold_expr e = log @@ "expr " ^ match e with
      | Binop (_, o, _) -> show_binop o
      | Unop (o, _) -> show_unop o
      | Atom _ -> "atom"
    method! fold_binop o = log ("binop " ^ show_binop o)
    method! fold_unop o = log ("unop " ^ show_unop o)
  end

let%expect_test "ptree-expression-visitor" =
  let fold f ~seed e = f (ptree_expr_folder ~seed) e [] in
  check_equivalence
    ~gen:(gen ~leaf:(P.Atom (Integer "1"))
            ~not_:(fun e -> P.Unop (UMinus, e)) ~paren:(fun e -> P.Unop (UPlus, e))
            ~combined:(fun e o e' -> P.Binop (e, o, e')) ~op:gen_binop)
    ~checks:[
      fold P.Terms_visitor.fold_expr',
      fold ptree_reference_expr';
      fold (fun v e -> P.Terms_visitor.fold_expr v ~&e),
      fold (fun v e -> ptree_reference_expr v ~&e);
    ];
  [%expect {| runs: 1240, events: 42826, mismatches: 0 |}]

(* --- *)

let rec procedure_reference_expr v =
  handle v#fold_expr ~continue:begin fun e x -> match e with
    | T.Atom a -> x
        >> Cobol_procedure.Visitor.fold_scalar v a
    | Unop (o, e) -> x
        >> P.Terms_visitor.fold_unop v o
        >> procedure_reference_expr' v e
    | Binop (e, o, e') -> x
        >> procedure_reference_expr' v e
        >> P.Terms_visitor.fold_binop v o
        >> procedure_reference_expr' v e'
  end
and procedure_reference_expr' v =
  handle' v#fold_expr' ~fold:procedure_reference_expr v

let procedure_expr_folder ~seed =
  let log = logger ~seed in
  object
    inherit [string list] Cobol_procedure.Visitor.folder
    method! fold' { loc; _ } = log ("with_loc " ^ id_of loc)
    method! fold_expr' { loc; _ } = log ("expr' " ^ id_of loc)
    method! fold_expr e = log @@ "expr " ^ match e with
      | Binop (_, o, _) -> show_binop o
      | Unop (o, _) -> show_unop o
      | Atom _ -> "atom"
    method! fold_binop o = log ("binop " ^ show_binop o)
    method! fold_unop o = log ("unop " ^ show_unop o)
  end

let%expect_test "procedure-expression-visitor" =
  let fold f ~seed e = f (procedure_expr_folder ~seed) e [] in
  check_equivalence
    ~gen:(gen ~leaf:(T.Atom (ScalarLiteral (Error ())))
            ~not_:(fun e -> T.Unop (UMinus, e)) ~paren:(fun e -> T.Unop (UPlus, e))
            ~combined:(fun e o e' -> T.Binop (e, o, e')) ~op:gen_binop)
    ~checks:[
      fold Cobol_procedure.Visitor.fold_expr',
      fold procedure_reference_expr';
      fold (fun v e -> Cobol_procedure.Visitor.fold_expr v ~&e),
      fold (fun v e -> procedure_reference_expr v ~&e);
    ];
  [%expect {| runs: 1240, events: 42826, mismatches: 0 |}]
