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

(* NOTE: the implementation of this module is automatically generated from the
   token descriptions in `grammar_tokens.mly` by using the
   `keywords/gen_keywords.ml` utility. *)

(** Mapping from keywords to their respective tokens *)
val keywords: (string * Grammar_tokens.token) list

(** Set of {e inhibited} keywords *)
val silenced_keywords: string list

(** Mapping from punctuations to their respective tokens *)
val puncts: (string * Grammar_tokens.token) list
