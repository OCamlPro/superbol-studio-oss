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

(** Gathers some types used to define options for the parser engine. *)
(* We are only defining types here so an MLI would only be redundant. *)

open Cobol_common.Srcloc.TYPES

(** Switch for the recovery mechanism *)
type recovery =
  | DisableRecovery
  | EnableRecovery of recovery_options
and recovery_options =
  {
    silence_benign_recoveries: bool; (** Whether to silence reports about some
                                         missing tokens (e.g, periods).  *)
  }

type 'a memory =
  | Amnesic: Cobol_common.Behaviors.amnesic memory
  | Eidetic: Cobol_common.Behaviors.eidetic memory

type tokens_with_locs = Grammar_tokens.token with_loc list
type ('a, 'm) output =
  | Only: 'a ->
      ('a, Cobol_common.Behaviors.amnesic) output
  | WithTokens: 'a * tokens_with_locs Lazy.t * Cobol_preproc.rev_log ->
      ('a, Cobol_common.Behaviors.eidetic) output

type ('a, 'm) parsed_result =
  {
    parsed_input: Cobol_preproc.input;
    parsed_diags: Cobol_common.Diagnostics.Set.t;
    parsed_output: ('a, 'm) output;
  }
