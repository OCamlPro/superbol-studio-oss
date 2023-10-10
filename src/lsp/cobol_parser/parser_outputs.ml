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

(** Gathers some types used to define outputs for the parser engine. *)
(* We are only defining types here so an MLI would only be redundant. *)

open Cobol_common.Srcloc.TYPES

type tokens_with_locs = Grammar_tokens.token with_loc list

type artifacts =
  {
    tokens: tokens_with_locs Lazy.t;
    pplog: Cobol_preproc.Trace.log;
    comments: Cobol_preproc.Text.comments;
  }

(** The output of parsing functions depends on its memorization abilities:

    - an amnesic parse (when ['memo = Cobol_common.Behaviors.amnesic]) only
      returns a parsing result (of type ['result]);

    - an eidetic parse (when ['memo = Cobol_common.Behaviors.eidetic])
      additionally returns some parsing artefacts. *)
type ('result, 'memo) output =
  | Only:
      'result             -> ('result, Cobol_common.Behaviors.amnesic) output
  | WithArtifacts:
      'result * artifacts -> ('result, Cobol_common.Behaviors.eidetic) output

type 'm parsed_compilation_group = (PTree.compilation_group option, 'm) output
