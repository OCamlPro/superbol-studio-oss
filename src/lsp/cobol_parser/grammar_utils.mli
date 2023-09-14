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

module Overlay_manager: Cobol_preproc.Src_overlay.MANAGER

val neg_cond: bool -> simple_condition -> condition
val neg_cond': bool -> condition -> condition

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
val expand_relation_condition
  : bool
  -> expression * relop * expression
  -> (logop * flat_combination_operand) option
  -> condition
