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

val neg_simple_cond: neg:bool -> Terms.simple_condition -> Terms.condition
val neg_condition: neg:bool -> Terms.condition -> Terms.condition

val expand_every_abbrev_cond: 'k Terms.cond -> Terms.condition
val expand_abbrev_cond: Terms.abbrev_combined_relation -> Terms.condition
