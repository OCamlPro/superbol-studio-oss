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

type 'k srclexer = 'k Src_lexing.state * Lexing.lexbuf
and any_srclexer =
  | Plx: 'k srclexer -> any_srclexer                                   [@@unboxed]

(* --- Compiler Directives -------------------------------------------------- *)

(* SOURCE FORMAT *)

type lexing_directive =
  | LexDirSource:
      'k Src_lexing.source_format with_loc -> lexing_directive [@@unboxed]

(* COPY/REPLACING *)

type copy_statement =
  | CDirCopy of
      {
        library: library;
        suppress_printing: bool;
        replacing: replacing with_loc list;
      }
and replace_statement =
  | CDirReplace of
      {
        also: bool;
        replacing: replacing with_loc list;
      }
  | CDirReplaceOff of
      {
        last: bool;
      }
and library =
  {
    libname: fileloc with_loc;
    cbkname: fileloc with_loc option;
  }
and fileloc = [`Word | `Alphanum] * string
and replacing

type (_, _) repl_attempt =
  | OnPartText: ([`NoReplacement | `MissingText],
                 partial_text_repl_result) repl_attempt
  | OnFullText: ([`NoReplacement],
                 text * Preproc_trace.log) repl_attempt
and partial_text_repl_result =
  (text * Preproc_trace.log,
   [`MissingText of text * Preproc_trace.log * text]) result

module type ENTRY_POINTS = sig
  type 'x entry
  val replace_statement: replace_statement with_diags with_loc entry
  val lexing_directive: lexing_directive option with_diags with_loc entry
  val copy_statement: copy_statement with_diags with_loc entry
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

type partial_replacing =
  {
    repl_dir: replacing_direction;
    repl_strict: bool;
  }
and replacing_direction = Leading | Trailing

val replacing
  : ?partial:partial_replacing
  -> pseudotext with_loc
  -> pseudotext with_loc
  -> replacing option with_diags
val apply_replacing
  : (_, 'a) repl_attempt
  -> replacing with_loc list
  -> Preproc_trace.log
  -> text
  -> 'a

val srclex_diags
  : any_srclexer
  -> Cobol_common.Diagnostics.Set.t

val cdir_source_format
  : dialect: Cobol_config.dialect
  -> string with_loc
  -> lexing_directive option with_diags
val srclex_source_format
  : any_srclexer
  -> Cobol_config.source_format
val with_source_format
  : 'k Src_lexing.source_format with_loc
  -> any_srclexer
  -> any_srclexer

val srclex_pos
  : any_srclexer
  -> Lexing.position
val srclex_from_file
  : source_format:Cobol_config.source_format
  -> string
  -> any_srclexer
val srclex_from_string
  : ?filename: string
  -> source_format:Cobol_config.source_format
  -> string
  -> any_srclexer
val srclex_from_channel
  : ?filename: string
  -> source_format:Cobol_config.source_format
  -> in_channel
  -> any_srclexer

val next_source_line: any_srclexer -> any_srclexer * text
val fold_source_lines: any_srclexer -> (text -> 'a -> 'a) -> 'a -> 'a
val print_source_lines: Format.formatter -> any_srclexer -> unit

(* --- *)

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
