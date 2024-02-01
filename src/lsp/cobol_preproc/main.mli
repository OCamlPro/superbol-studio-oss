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

type preprocessor
type t = preprocessor

val preprocessor
  : ?options: Options.preproc_options
  -> Src_input.t
  -> preprocessor
val reset_preprocessor_for_string
  : string
  -> ?new_position: Lexing.position
  -> preprocessor
  -> preprocessor

(* --- *)

val diags: preprocessor -> Cobol_common.Diagnostics.Set.t
val add_diag: preprocessor -> Cobol_common.Diagnostics.t -> preprocessor
val add_diags: preprocessor -> Cobol_common.Diagnostics.Set.t -> preprocessor
val position: preprocessor -> Lexing.position
val position_at: line:int -> char: int -> preprocessor -> Lexing.position
val source_format: preprocessor -> Src_format.any
val rev_log: preprocessor -> Trace.log
val rev_comments: preprocessor -> Text.comments
val rev_ignored: preprocessor -> lexloc list

val next_chunk: preprocessor -> Text.text * preprocessor

(** {2 High-level commands} *)

val lex_input
  : dialect: Cobol_config.Types.dialect
  -> source_format: Cobol_config.Types.source_format_spec
  -> ?ppf:Format.formatter
  -> Src_input.t
  -> unit Cobol_common.Diagnostics.with_diags

val lex_file
  : dialect: Cobol_config.Types.dialect
  -> source_format: Cobol_config.Types.source_format_spec
  -> ?ppf:Format.formatter
  -> string
  -> unit Cobol_common.Diagnostics.with_diags

val lex_lib
  : dialect: Cobol_config.Types.dialect
  -> source_format: Cobol_config.Types.source_format_spec
  -> libpath:string list
  -> ?ppf:Format.formatter
  -> [< `Alphanum | `Word ] * string
  -> unit Cobol_common.Diagnostics.with_diags

(** [fold_source_lines ~dialect ~source_format ~skip_compiler_directives_text
    ~on_compiler_directive ~on_initial_source_format ~f input acc] applies [f
    line_number line acc] for each successive source line [line] of [input].
    [line_number] gives the line number for [line] (starting at [1]).  [line] is
    given empty to [f] if it corresponds to an empty line in the input, or was a
    line continuation in the case of fixed-width reference format.

    When given, [on_compiler_directive] is called {e after} [f] has been fed
    with the text of a compiler directive, with the same line number.

    When given, [on_initial_source_format] is called {e before} either [f] or
    [on_compiler_directive] has been called.

    When set, [skip_compiler_directives_text] ([false] by default) prevents the
    text of compiler directives from being fed to [f].  If given,
    [on_compiler_directive] is called as if the text had been fed to [f].

    Diagnostics resulting from lexing and parsing the input are attached to the
    returned accumulated value. *)
val fold_source_lines
  : dialect: Cobol_config.Types.dialect
  -> source_format: Cobol_config.Types.source_format_spec
  -> ?on_initial_source_format: (Src_format.any -> 'a -> 'a)
  -> ?skip_compiler_directives_text: bool
  -> ?on_compiler_directive
     : (int -> Directives.compiler_directive with_loc -> 'a -> 'a)
  -> f:(int -> Text.text -> 'a -> 'a)
  -> Src_input.t
  -> 'a
  -> 'a Cobol_common.Diagnostics.with_diags

val preprocess_input
  : ?options: Options.preproc_options
  -> ?ppf:Format.formatter
  -> Src_input.t
  -> unit Cobol_common.Diagnostics.with_diags

val preprocess_file
  : ?options: Options.preproc_options
  -> ?ppf:Format.formatter
  -> string
  -> unit Cobol_common.Diagnostics.with_diags

val text_of_file
  : ?options: Options.preproc_options
  -> string
  -> Text.t Cobol_common.Diagnostics.with_diags

val text_of_input
  : ?options: Options.preproc_options
  -> Src_input.t
  -> Text.t Cobol_common.Diagnostics.with_diags
