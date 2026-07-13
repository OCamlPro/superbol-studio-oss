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
open Cobol_common
open Srcloc.INFIX

let neg_condition ~neg (c: cond with_loc): cond =
  if not neg then ~&c else Not c

(** [abbrev_condition_expansion_state] is used internally by [expand_abbrev_cond]
    to remember the previous subject and relational operator that are omitted
    in the abbreviation. *)
type abbrev_condition_expansion_state =
  | AfterSubject of bool * expr with_loc
  | AfterRelOp of bool * expr with_loc * relop
  | AfterNonAbbrev

(** [AbbrevMissingSubjectAfterNonAbbrev] is raised by [expand_abbrev_cond] on
    invalid conditions such as [a < b OR c IS POSITIVE OR > d] where there is no
    rule to expand [> d] accoding to the COBOL standard. *)
exception AbbrevMissingSubjectAfterNonAbbrev

(** [AbbrevMissingRelOpAfterSubject] is raised by [expand_abbrev_cond] on invalid
    conditions where there is no [AbbrevRelOp] in leftmost position after an
    [AbbrevSubject]. Normally such cases are forbidden by parsing rules so this
    exception is an internal error. *)
exception AbbrevMissingRelOpAfterSubject

(** [expand_every_abbrev_cond cond] recursively substitutes every abbreviated
    combined relation condition from [cond] by an equivalent non-abbreviated
    condition (with abbreviated relations replaced with binary relations). *)
let rec expand_every_abbrev_cond
  : cond -> cond = function
  | Expr _ | Relation _ | ClassCond _ | SignCond _ | Omitted _ as c ->
      c
  | Abbrev a ->
      expand_abbrev_cond a
  | Not c ->
      Not (expand_every_abbrev_condition c)
  | Logop (c1, o, c2) ->
      Logop (expand_every_abbrev_condition c1, o, expand_every_abbrev_condition c2)

and expand_every_abbrev_condition cond = expand_every_abbrev_cond ~&cond &@<- cond

(** [expand_abbrev_cond abbrev_combined_relation], expands the non-parenthesized
    relation condition encoded by [abbrev_combined_relation] ([= neg,
    relation_condition, logop, flatop]).

    The result is an expression without any abbreviated combined relation
    condition: {i [relation_condition] [logop] abbrev-combined-conditions} (or
    {i NOT [relation_condition] [logop] abbrev-combined-conditions} if [neg]
    holds), where [logop] and {i abbrev-combined-conditions} are given via
    [logop], and [flatop]. *)
and expand_abbrev_cond (abbrev : abbrev_combined_relation) : cond =

  let rec disambiguate abbrevop sr =
    (* Recursively constructs a valid condition based on the abbreviated
       relational combined condition [abbrevop], assuming [sr] is the most recent
       subject and relation operator (when reading from the left of the
       sentence, canceling out on non-relational conditions). *)
    match ~&abbrevop, sr with
    | AbbrevRelOp (rel, a), (AfterSubject (neg, subj) | AfterRelOp (neg, subj, _)) ->
        disambiguate a (AfterRelOp (neg, subj, rel))
    | AbbrevRelOp _, AfterNonAbbrev ->
        raise AbbrevMissingSubjectAfterNonAbbrev
    | AbbrevObject (obj_neg, obj), AfterRelOp (subj_neg, subj, rel) ->
        let neg = if subj_neg then not obj_neg else obj_neg in
        let expanded_cond = neg_condition ~neg (Relation (subj, rel, obj) &@<- abbrevop) in
        (* If present a NOT before the subject only applies to the first object *)
        let sr = AfterRelOp (false, subj, rel) in
        expanded_cond &@<- abbrevop, sr
    | AbbrevObject (neg, e), AfterNonAbbrev ->
        neg_condition ~neg (Expr e &@<- abbrevop) &@<- abbrevop, AfterNonAbbrev
    | AbbrevObject _, AfterSubject _
    | AbbrevSubject _, AfterSubject _ ->
        raise AbbrevMissingRelOpAfterSubject
    | AbbrevSubject (neg, subj, a), _ ->
        let c, sr = disambiguate a (AfterSubject (neg, subj)) in
        ~&c &@<- abbrevop, sr
    | AbbrevParen (neg, a), sr ->
        let c, sr = disambiguate a sr in
        neg_condition ~neg c &@<- abbrevop, sr
    | AbbrevOther c, _ ->
        expand_every_abbrev_condition (c &@<- abbrevop), AfterNonAbbrev
    | AbbrevComb (a1, logop, a2), sr ->
        let c1, sr = disambiguate a1 sr in
        let c2, sr = disambiguate a2 sr in
        Logop (c1, logop, c2) &@<- abbrevop, sr
  in

  let neg, subj, abbrevop = abbrev in
  let c, _ = disambiguate abbrevop (AfterSubject (neg, subj)) in
  ~&c
