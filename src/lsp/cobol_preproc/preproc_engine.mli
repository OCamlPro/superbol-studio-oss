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

type init =
  {
    init_libpath: string list;
    init_config: Cobol_config.t;
    init_source_format: Cobol_config.source_format_spec;
  }

(* --- *)

val diags: preprocessor -> Cobol_common.Diagnostics.Set.t
val add_diag: preprocessor -> Cobol_common.Diagnostics.t -> preprocessor
val add_diags: preprocessor -> Cobol_common.Diagnostics.Set.t -> preprocessor
val log: preprocessor -> Preproc_trace.log
val srclexer: preprocessor -> Preproc.any_srclexer
val position: preprocessor -> Lexing.position
val next_sentence: preprocessor -> Text.text * preprocessor

(** {2 High-level commands} *)

val decide_source_format
  : string
  -> Cobol_config.source_format_spec
  -> Cobol_config.source_format Cobol_common.Diagnostics.with_diags

val preprocessor
  : ?verbose:bool
  -> input
  -> [< `WithLibpath of init ]
  -> preprocessor

val lex_file
  : source_format: Cobol_config.source_format_spec
  -> ?ppf:Format.formatter
  -> ?epf:Format.formatter
  -> input
  -> unit

val fold_text_lines
  : source_format: Cobol_config.source_format_spec
  -> ?epf:Format.formatter
  -> (Text.text -> 'a -> 'a)
  -> input
  -> 'a
  -> 'a

val lex_lib
  : source_format: Cobol_config.source_format_spec
  -> libpath:string list
  -> ?ppf:Format.formatter
  -> ?epf:Format.formatter
  -> [< `Alphanum | `Word ] * string
  -> unit

val preprocess_file
  : source_format: Cobol_config.source_format_spec
  -> ?verbose:bool
  -> ?config:Cobol_config.t
  -> libpath:string list
  -> ?ppf:Format.formatter
  -> ?epf:Format.formatter
  -> string
  -> unit

val text_of_file
  : source_format: Cobol_config.source_format_spec
  -> ?verbose:bool
  -> ?config:Cobol_config.t
  -> libpath:string list
  -> ?epf:Format.formatter
  -> string
  -> Text.text

val text_of_input
  : source_format: Cobol_config.source_format_spec
  -> ?verbose:bool
  -> ?config:Cobol_config.t
  -> libpath:string list
  -> ?epf:Format.formatter
  -> input
  -> Text.text
