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

(** Some utilities to construct or rewrite terms (mostly conditions for now) *)

val neg_condition: neg:bool -> 'r Terms.cond Cobol_common.with_loc -> 'r Terms.cond
val cast_no_rel_cond: Terms.no_rel Terms.cond -> 'r Terms.cond

val expand_every_abbrev_cond: Terms.condition -> Terms.expanded_cond
val expand_abbrev_cond: Terms.abbrev_combined_relation -> Terms.expanded_cond
