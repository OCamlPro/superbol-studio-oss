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

type completion_entry =
  | K of token Cobol_common.Basics.NEL.t
  | QualifiedRef
  | ProcedureRef

val pp_completion_entry: completion_entry Fmt.t

module CompEntrySet : (Set.S with type elt = completion_entry)

val reducible_productions_in: env:_ env -> production list

val acceptable_nullable_nonterminals_in: env:_ env -> xsymbol list

val acceptable_terminals_in: env:_ env -> completion_entry list

val guessed_default_value_of_nullables: 'a nonterminal -> 'a

