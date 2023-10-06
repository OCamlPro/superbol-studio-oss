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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Diagnostics.TYPES
open Text.TYPES

(** {1 Source text lexer} *)

type 'k srclexer = 'k Src_lexing.state * Lexing.lexbuf
and any_srclexer =
  | Plx: 'k srclexer -> any_srclexer                                   [@@unboxed]

(** {2 Source format} *)

val source_format
  : any_srclexer
  -> Src_format.any
val cdir_source_format
  : dialect: Cobol_config.dialect
  -> string with_loc
  -> Preproc_directives.lexing_directive option with_diags
val with_source_format
  : 'k Src_format.source_format with_loc
  -> any_srclexer
  -> any_srclexer

(** {2 Instantiation} *)

val srclex_from_file
  : source_format: Src_format.any
  -> string
  -> any_srclexer
val srclex_from_string
  : ?filename: string
  -> source_format: Src_format.any
  -> string
  -> any_srclexer
val srclex_from_channel
  : ?filename: string
  -> source_format: Src_format.any
  -> in_channel
  -> any_srclexer

(** {2 Resetting the input} *)

(** Note: the functions below assume [position] corresponds to the begining of
    the input.} *)

val srclex_restart_on_file
  : ?position: Lexing.position
  -> string
  -> any_srclexer
  -> any_srclexer
val srclex_restart_on_string
  : ?position: Lexing.position
  -> string
  -> any_srclexer
  -> any_srclexer
val srclex_restart_on_channel
  : ?position: Lexing.position
  -> in_channel
  -> any_srclexer
  -> any_srclexer

(** {2 Queries} *)

val srclex_diags
  : any_srclexer
  -> Cobol_common.Diagnostics.Set.t
val srclex_pos
  : any_srclexer
  -> Lexing.position
val srclex_comments
  : any_srclexer
  -> comments
val srclex_newline_cnums
  : any_srclexer
  -> int list
val next_source_line
  : any_srclexer
  -> any_srclexer * text
val fold_source_lines
  : any_srclexer
  -> (text -> 'a -> 'a)
  -> 'a
  -> 'a
val print_source_lines
  : Format.formatter
  -> any_srclexer
  -> unit

(** {1 Compiler Directives} *)

val replacing
  : ?partial: Preproc_directives.partial_replacing
  -> pseudotext with_loc
  -> pseudotext with_loc
  -> Preproc_directives.replacing option with_diags

type (_, _) repl_attempt =
  | OnPartText: ([`NoReplacement | `MissingText],
                 partial_text_repl_result) repl_attempt
  | OnFullText: ([`NoReplacement],
                 text * Preproc_trace.log) repl_attempt
and partial_text_repl_result =
  (text * Preproc_trace.log,
   [`MissingText of text * Preproc_trace.log * text]) result
val apply_replacing
  : (_, 'a) repl_attempt
  -> Preproc_directives.replacing with_loc list
  -> Preproc_trace.log
  -> text
  -> 'a

(** {1 Preprocessor state}

    This state is used to track some preprocessing-related divisions, like the
    `CONTROL DIVISION` in the GCOS dialect. *)

type state

type preproc_phrase =
  | Copy of phrase
  | Replace of phrase
  | Header of tracked_header * phrase
and phrase =
  {
    prefix: text;
    phrase: text;
    suffix: text;
  }
and tracked_header =
  | ControlDivision
  | SubstitutionSection
  | IdentificationDivision

val initial_state: state
val find_preproc_phrase
  : ?prefix:[ `Rev | `Same ]
  -> state
  -> text
  -> (preproc_phrase * state,
      [> `MissingPeriod | `MissingText | `NoneFound ]) result

(** {1 Parsing statements and directives} *)

module type ENTRY_POINTS = sig
  type 'x entry
  val replace_statement
    : Preproc_directives.replace_statement with_diags with_loc entry
  val lexing_directive
    : Preproc_directives.lexing_directive option with_diags with_loc entry
  val copy_statement
    : Preproc_directives.copy_statement with_diags with_loc entry
end

module type PPPARSER = sig
  exception Error

  (* The incremental API. *)
  module MenhirInterpreter: MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = Preproc_tokens.token

  (* The entry point(s) to the incremental API. *)
  module Incremental: ENTRY_POINTS with type
    'x entry := Lexing.position -> 'x MenhirInterpreter.checkpoint
end
