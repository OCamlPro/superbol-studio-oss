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

(** Parsing functions essentially parse a stream of tokens that is produced by a
    given preprocessor (typically returned by
    {!val:Cobol_preproc.preprocessor}).  The result always consists in a
    (possibly empty) set of diagnostics, along with either:

    - an optional parse-tree [Only (Some ptree)] only;

    - an optional parse-tree along with some artifacts
      [WithArtifacts (Some ptree, artifacts)].

    The set of diagnostics attached to the result of parsing functions
    ([result.diags]) should {e always} be checked for errors upon return ({i
    i.e,} {!Cobol_common.Diagnostics.Set.has_errors} holds).  This is in
    particular the case when the resulting parse-tree is provided and recovery
    is enabled ([options.recovery <> DisableRecovery]), as in such a case, the
    parse-tree returned may contain dummy nodes and source locations produced
    using the recovery mechanism. *)

(** {1 Basic (one-shot) parsing} *)

(** Simple parsing functions traverse the inputs once to produce a result. *)
type 'm simple_parsing
  = ?options:Parser_options.parser_options
  -> ?config:Cobol_config.t
  -> Cobol_preproc.preprocessor
  -> (PTree.compilation_group option, 'm) output
    Cobol_common.Diagnostics.with_diags

(* val parse *)
(*   : memory: 'm memory -> 'm simple_parsing *)

(** Simple parsing function that does not return any artifact. *)
val parse_simple
  : Cobol_common.Behaviors.amnesic simple_parsing

(** Simple parsing function does return some artifacts. *)
val parse_with_artifacts
  : Cobol_common.Behaviors.eidetic simple_parsing

(** {1 Rewindable parsing} *)

(** Rewindable parsing functions extend the behaviors of simple parsing
    functions, with the ability to {i rewind} the parser (and pre-processor)
    before a given lexing position.  To this end, their results include a
    {!rewinder} that may then be given to {!rewind_and_parse}. *)
type 'm rewindable_parsing
  = ?options:parser_options
  -> ?config:Cobol_config.t
  -> Cobol_preproc.preprocessor
  -> (((PTree.compilation_group option, 'm) output as 'x) * 'x rewinder)
    Cobol_common.Diagnostics.with_diags

(** Rewinder for parsing functions that produce results of type ['x] *)
and 'x rewinder

(** Functions for rewinding the pre-processor.  Such a function should return a
    preprocessor in the {e exact same state} as the one given in argument, with
    the only exception that the input text is now read from [new_position].  If
    [new_position] is not given, the text should be read from the very begining
    of the input. *)
and preprocessor_rewind =
  ?new_position:Lexing.position -> (Cobol_preproc.preprocessor as 'r) -> 'r

(* val rewindable_parse *)
(*   : memory:'m memory -> 'm rewindable_parsing *)

(** Rewindable parsing function that does not return any artifact. *)
val rewindable_parse_simple
  : Cobol_common.Behaviors.amnesic rewindable_parsing

(** Rewindable parsing function that does return some artifacts. *)
val rewindable_parse_with_artifacts
  : Cobol_common.Behaviors.eidetic rewindable_parsing

(** Specification of positions to rewind to. *)
type position =
  | Lexing of
      Lexing.position                                 (** raw lexing position *)
  | Indexed of
      {
        line: int;               (** line number (starting at 0) *)
        char: int;               (** character number in line (starting at 0) *)
      }

(** [rewind_and_parse rewinder preprocessor_rewind ~position] uses [rewinder] to
    restart parsing before the given [position].  Note that the [new_position]
    argument that is given to [preprocessor_rewind] may {e not} correspond to
    [position], as the parser typically needs to rewind earlier than
    [position]. *)
val rewind_and_parse
  : 'x rewinder
  -> preprocessor_rewind
  -> position: position
  -> (((PTree.compilation_group option, 'm) output as 'x) * 'x rewinder)
    Cobol_common.Diagnostics.with_diags

(** {1 Accessing artifacts} *)

val artifacts
  : (_, Cobol_common.Behaviors.eidetic) output -> artifacts
