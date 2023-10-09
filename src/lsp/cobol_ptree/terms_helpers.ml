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

open Terms

let neg_simple_cond ~neg : simple_condition -> condition =
  if not neg then UPCAST.simple_cond else fun c -> Not c
let neg_condition ~neg : condition -> condition =
  if not neg then Fun.id else fun c -> Not c

(** [expand_every_abbrev_cond cond] recursively substitutes every abbreviated
    combined relation condition from [cond] by an equivalent non-abbreviated
    condition (with abbreviated relations replaced with binary relations). *)
let rec expand_every_abbrev_cond
  : type k. k cond -> _ cond = function
  | Expr _ | Relation _ | ClassCond _ | SignCond _ | Omitted _ as c ->
      c
  | Abbrev a ->
      expand_abbrev_cond a
  | Not c ->
      Not (expand_every_abbrev_cond c)
  | Logop (c1, o, c2) ->
      Logop (expand_every_abbrev_cond c1, o, expand_every_abbrev_cond c2)

(** [expand_abbrev_cond abbrev_combined_relation], expands the non-parenthesized
    relation condition encoded by [abbrev_combined_relation] ([= neg,
    relation_condition, logop, flatop]).

    The result is an expression without any abbreviated combined relation
    condition: {i [relation_condition] [logop] abbrev-combined-conditions} (or
    {i NOT [relation_condition] [logop] abbrev-combined-conditions} if [neg]
    holds), where [logop] and {i abbrev-combined-conditions} are given via
    [logop], and [flatop]. *)
and expand_abbrev_cond: abbrev_combined_relation -> condition =

  let rec disambiguate ?cond_prefix flatop sr =
    (* Recursively constructs a valid condition based on the non-parenthesized
       relational combined condition [flatop], assuming [sr] is the most recent
       subject and relation operator (when reading from the left of the
       sentence, canceling out on non-relational conditions).

       If [cond_prefix] is given, places it with a conjunction at the
       bottom-left of the result, i.e, substitutes the bottom-left node [c] with
       [Logop (cond_prefix, LAnd, c)]. *)
    let c, sr = match flatop, sr with
      | FlatAmbiguous (Some rel, e), Some (subj,   _)
      | FlatAmbiguous (None,     e), Some (subj, rel) ->
          UPCAST.simple_cond @@ Relation (subj, rel, e), Some (subj, rel)
      | FlatAmbiguous (_, e), None ->
          Expr e, sr
      | FlatNotExpr e, Some (subj, rel) ->
          Not (UPCAST.simple_cond @@ Relation (subj, rel, e)), sr
      | FlatNotExpr e, None ->
          Not (UPCAST.simple_cond @@ Expr e), sr
      | FlatRel (neg, (e1, rel, e2)), _ ->
          neg_condition ~neg @@ Relation (e1, rel, e2), Some (e1, rel)
      | FlatOther c, _ ->
          expand_every_abbrev_cond c, None
      | FlatComb (f1, logop, f2), sr ->
          let c1, sr = disambiguate ?cond_prefix f1 sr in
          let c2, sr = disambiguate f2 sr in
          Logop (c1, logop, c2), sr
    in
    match flatop, cond_prefix with
    | FlatComb _, _ | _, None -> c, sr
    | _, Some c0 -> Logop (c0, LAnd, c), sr
  in

  fun (neg, (e1, relop, e2), logop, flatop) ->
    let c0 = neg_condition ~neg @@ Relation (e1, relop, e2) in
    match logop with
    | LOr -> Logop (c0, LOr, fst @@ disambiguate flatop (Some (e1, relop)))
    | LAnd -> fst @@ disambiguate ~cond_prefix:c0 flatop (Some (e1, relop))
