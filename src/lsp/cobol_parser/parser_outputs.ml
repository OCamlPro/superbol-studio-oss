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
    rev_comments: Cobol_preproc.Text.comments;
    rev_ignored: lexloc list;
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

type 'm parsed_compilation_group =
  (Cobol_ptree.compilation_group option, 'm) output

(* --- *)

module Diagnostics = struct
  type t =
    {
      preproc_diags: Cobol_preproc.Diagnostics.t;
      parser_diags: Parser_diagnostics.t;
    }
  let none =
    {
      preproc_diags = Cobol_preproc.Diagnostics.none;
      parser_diags = Parser_diagnostics.none;
    }
  let union d1 d2 =
    {
      preproc_diags =
        Cobol_preproc.Diagnostics.union d1.preproc_diags d2.preproc_diags;
      parser_diags =
        Parser_diagnostics.union d1.parser_diags d2.parser_diags;
    }
  let translate { preproc_diags; parser_diags } =
    let pp_diags = Cobol_preproc.Diagnostics.translate preproc_diags in
    let pa_diags = Parser_diagnostics.translate parser_diags in
    Cobol_common.Diagnostics.Set.union pp_diags pa_diags
  let has_errors { preproc_diags; parser_diags } =
    Cobol_preproc.Diagnostics.has_errors preproc_diags ||
    Parser_diagnostics.has_errors parser_diags
end
include Cobol_common.Diagnostics_accumulator.MAKE (Diagnostics)
