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

type preprocessor

type input =
  | Filename of string
  | String of { contents: string; filename: string }
  | Channel of { contents: in_channel; filename: string }

val preprocessor
  : ?options: Preproc_options.preproc_options
  -> input
  -> preprocessor
val reset_preprocessor
  : ?new_position:Lexing.position
  -> preprocessor
  -> input
  -> preprocessor
val reset_preprocessor_for_string
  : string
  -> ?new_position:Lexing.position
  -> preprocessor
  -> preprocessor


(* --- *)

val diags: preprocessor -> Cobol_common.Diagnostics.Set.t
val add_diag: preprocessor -> Cobol_common.Diagnostics.t -> preprocessor
val add_diags: preprocessor -> Cobol_common.Diagnostics.Set.t -> preprocessor
val log: preprocessor -> Preproc_trace.log
val comments: preprocessor -> Text.comments
val position: preprocessor -> Lexing.position
val source_format: preprocessor -> Src_format.any
val newline_cnums: preprocessor -> int list

val next_chunk: preprocessor -> Text.text * preprocessor

(** {2 High-level commands} *)

val decide_source_format
  : string
  -> Cobol_config.source_format_spec
  -> Src_format.any Cobol_common.Diagnostics.with_diags

val lex_file
  : source_format: Cobol_config.source_format_spec
  -> ?ppf:Format.formatter
  -> ?epf:Format.formatter
  -> input
  -> unit

val lex_lib
  : source_format: Cobol_config.source_format_spec
  -> libpath:string list
  -> ?ppf:Format.formatter
  -> ?epf:Format.formatter
  -> [< `Alphanum | `Word ] * string
  -> unit

val fold_source_lines
  : source_format: Cobol_config.source_format_spec
  -> ?epf:Format.formatter
  -> f:(int -> Text.text -> 'a -> 'a)
  -> input
  -> 'a
  -> 'a

val preprocess_file
  : ?options: Preproc_options.preproc_options
  -> ?ppf:Format.formatter
  -> ?epf:Format.formatter
  -> string
  -> unit

val text_of_file
  : ?options: Preproc_options.preproc_options
  -> ?epf:Format.formatter
  -> string
  -> Text.text

val text_of_input
  : ?options: Preproc_options.preproc_options
  -> ?epf:Format.formatter
  -> input
  -> Text.text
