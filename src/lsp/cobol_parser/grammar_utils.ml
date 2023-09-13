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

open Cobol_ast

(* Note: we can share the same source overlay manager across several parsers as
   long as we don't parse localized tokens using multiple instances of the
   parser and in parallel.  If that were to change, the manager would need to be
   passed as parameter to the grammar. *)
module Overlay_manager =
  Cobol_preproc.Src_overlay.New_manager (struct
    let name = __MODULE__
  end)

let neg_cond neg : simple_condition -> condition =
  if not neg then UPCAST.simple_cond else fun c -> Not c
and neg_cond' neg : condition -> condition =
  if not neg then Fun.id else fun c -> Not c

(** Suffix of non-parenthesized relational combined conditions, to decypher
    abbreviations *)
type flat_combination_operand =
  | FlatAmbiguous of relop option * expression                    (* relop? e *)
  | FlatNotExpr of expression                                     (* NOT e *)
  | FlatRel of bool * (expression * relop * expression)           (* NOT? rel *)
  | FlatOther of condition            (* extended- or parenthesized condition *)
  | FlatComb of (flat_combination_operand as 'x) * logop * 'x     (* _ AND/OR _ *)

(** [expand_relation_condition neg relation_condition logop_n_flatop] expands
    the non-parenthesized relation condition encoded by:

    - {i [relation_condition]} (or {i NOT [relation_condition]} if [neg] holds)
    if [logop_n_flatop] is [None];

    - {i [relation_condition] [logop] abbrev-combined-conditions} (or {i NOT
    [relation_condition] [logop] abbrev-combined-conditions} if [neg] holds),
    where [logop] and {i abbrev-combined-conditions} are given via
    [logop_n_flatop]. *)
let expand_relation_condition =
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
          neg_cond' neg @@ Relation (e1, rel, e2), Some (e1, rel)
      | FlatOther c, _ ->
          c, None
      | FlatComb (f1, logop, f2), sr ->
          let c1, sr = disambiguate ?cond_prefix f1 sr in
          let c2, sr = disambiguate f2 sr in
          Logop (c1, logop, c2), sr
    in
    match flatop, cond_prefix with
      | FlatComb _, _ | _, None -> c, sr
      | _, Some c0 -> Logop (c0, LAnd, c), sr
  in
  fun neg (e1, relop, e2) ->
  let c0 = neg_cond' neg @@ Relation (e1, relop, e2) in
  function
  | None ->
      c0
  | Some (LOr, flatop) ->
      Logop (c0, LOr, fst @@ disambiguate flatop (Some (e1, relop)))
  | Some (LAnd, flatop) ->
      fst @@ disambiguate ~cond_prefix:c0 flatop (Some (e1, relop))
