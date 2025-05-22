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
    i.e,} {!Parser_outputs.Diagnostics.has_errors} holds).  This is in
    particular the case when the resulting parse-tree is provided and recovery
    is enabled ([options.recovery <> DisableRecovery]), as in such a case, the
    parse-tree returned may contain dummy nodes and source locations produced by
    the recovery mechanism. *)

(** {1 Basic (one-shot) parsing} *)

(** Simple parsing functions traverse the inputs once to produce a result. *)
type 'm simple_parsing
  = options: parser_options
  -> Cobol_preproc.preprocessor
  -> (Cobol_ptree.compilation_group option, 'm) output with_diags

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
  = options: parser_options
  -> Cobol_preproc.preprocessor
  -> (((Cobol_ptree.compilation_group option, 'm) output as 'x) * 'x rewinder)
    with_diags

(** Rewinder for parsing functions that produce results of type ['x] *)
and 'x rewinder

(** Functions for rewinding the pre-processor.  Such a function should return a
    preprocessor in the {e exact same state} as the one given in (unlabelled)
    argument, with the only exception that the input text is now read from
    [new_position].

    [last_pp] is the state of the pre-processor at the end of the previous call
    to {!rewindable_parse_simple}, {!rewindable_parse_with_artifacts}, or
    {!rewind_and_parse} operation: it may be used to translate positions that
    follow the last piece of input seen by the unlabelled preprocessor
    argument.

    If [new_position] is not given, the text should be read from the very
    beginning of the input. *)
and preprocessor_rewind
  = last_pp: (Cobol_preproc.preprocessor as 'r)
  -> ?new_position:Lexing.position
  -> 'r -> 'r

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
        line: int;             (** line number (starting from 0) *)
        char: int;             (** character number in line (starting from 0) *)
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
  -> (((Cobol_ptree.compilation_group option, 'm) output as 'x) * 'x rewinder)
    with_diags

(** {1 Rewinding for inspection} *)

(** Type of parser states that can be inspected using the interpreter that is
    generated by menhir ({i cf} {!INSPECT}). *)
type inspectable_parser_state =
  | Env: 'a Grammar.MenhirInterpreter.env -> inspectable_parser_state
  | Sink

(** Note: given parser state should not escapre [inspect]. *)
val rewind_for_inspection
  : 'x rewinder
  -> preprocessor_rewind
  -> position: position
  -> inspect: (inspectable_parser_state -> 'a) -> 'a

(** {1 Accessing artifacts} *)

val artifacts
  : (_, Cobol_common.Behaviors.eidetic) output -> artifacts
