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

(** Text manipulation *)

open Cobol_common.Srcloc.TYPES
open Cobol_common.Diagnostics.TYPES
open Text.TYPES

(** {1 Compiler directives} *)

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

(** {1 Parsing statements and directives} *)

module type ENTRY_POINTS = sig
  type 'x entry
  val replace_statement
    : Preproc_directives.replace_statement with_diags with_loc entry
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
