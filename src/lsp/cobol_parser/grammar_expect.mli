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

open Grammar.MenhirInterpreter

module Completion_entry: sig
  type t =
    | K of token Cobol_common.Basics.NEL.t
    | QualifiedRef
    | ProcedureRef
  val compare: t -> t -> int
  val pp: t Fmt.t
end

type xnonterminal =
  | X: 'a nonterminal -> xnonterminal

val reducible_productions_in: env:_ env -> production list

val nullable_nonterminals_in: env:_ env -> xnonterminal list

val completion_entries_in: env:_ env -> Completion_entry.t list

val eager_completion_entries_in: env:_ env -> Completion_entry.t list

val default_nonterminal_value: 'a nonterminal -> 'a

