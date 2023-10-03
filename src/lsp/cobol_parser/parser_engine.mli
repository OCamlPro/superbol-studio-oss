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

open Parser_options
open Parser_outputs

(** {1 Basic (one-shot) parsing} *)

type 'm simple_parsing
  = ?options:Parser_options.parser_options
  -> ?config:Cobol_config.t
  -> Cobol_preproc.preprocessor
  -> (PTree.compilation_group option, 'm) output
    Cobol_common.Diagnostics.with_diags

val parse
  : memory: 'm memory -> 'm simple_parsing
val parse_simple
  : Cobol_common.Behaviors.amnesic simple_parsing
val parse_with_artifacts
  : Cobol_common.Behaviors.eidetic simple_parsing

(** {1 Rewindable parsing} *)

type 'm rewindable_parsing
  = ?options:parser_options
  -> ?config:Cobol_config.t
  -> Cobol_preproc.preprocessor
  -> (((PTree.compilation_group option, 'm) output as 'x) *
      'x rewinder)
    Cobol_common.Diagnostics.with_diags
and 'x rewinder
and preprocessor_rewind =
  ?new_position:Lexing.position -> (Cobol_preproc.preprocessor as 'r) -> 'r

val rewindable_parse
  : memory:'m memory
  -> 'm rewindable_parsing
val rewindable_parse_simple
  : Cobol_common.Behaviors.amnesic rewindable_parsing
val rewindable_parse_with_artifacts
  : Cobol_common.Behaviors.eidetic rewindable_parsing

val rewind_and_parse
  : 'x rewinder
  -> preprocessor_rewind
  -> position: Lexing.position
  -> (((PTree.compilation_group option, 'm) output as 'x) * 'x rewinder)
    Cobol_common.Diagnostics.with_diags

(** {1 Accessing artifacts} *)

val artifacts
  : (_, Cobol_common.Behaviors.eidetic) output -> artifacts