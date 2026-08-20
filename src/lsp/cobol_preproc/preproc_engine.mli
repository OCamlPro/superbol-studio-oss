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

open EzCompat                                                    (* StringMap *)
open Cobol_common.Srcloc.TYPES

type preprocessor

val preprocessor
  : options: Preproc_options.preproc_options
  -> Src_input.t
  -> preprocessor
val reset_preprocessor_for_string
  : string
  -> ?new_position: Lexing.position
  -> preprocessor
  -> preprocessor

(* --- *)

val diags: preprocessor -> Preproc_diagnostics.t
val position: preprocessor -> Lexing.position
val position_at: line:int -> char:int -> preprocessor -> Lexing.position
val input_filename: preprocessor -> string option
val source_format: preprocessor -> Src_format.any
val rev_log: preprocessor -> Preproc_trace.log

(** [rev_comments pp] associates filenames read by [pp] (directly or indirectly
    while processing copybooks) with comments read in each respective file, in
    reverse order.

    As a special rule the returned map always includes an association between
    [""] and the file fed to [pp]. *)
val rev_comments: preprocessor -> Text.comments StringMap.t

(** [rev_ignored pp] lists the lexical locations ignored when reading the file
    given to [pp]. *)
val rev_ignored: preprocessor -> lexloc list

val next_chunk: preprocessor -> Text.text * preprocessor

(** {2 Environment access and manipulation} *)

(** Binds 78-level constants; managed by the preprocessor as MF allows use of
    such constants in compilation directives.

    Given [loc] should correspond to the area in the source code where the
    definition occurs. *)
val bind_78_constant
  : preprocessor
  -> loc:srcloc
  -> Cobol_ptree.name with_loc
  -> Cobol_ptree.literal with_loc
  -> preprocessor
val lookup_compilation_variable
  : preprocessor
  -> Preproc_env.var
  -> Preproc_env.compilation_var_definition option
val record_compilation_variable_substitution
  : preprocessor
  -> loc:srcloc
  -> var:Preproc_env.var
  -> def:Preproc_env.compilation_var_definition
  -> preprocessor

(** {2 High-level commands} *)

val lex_input
  : platform: Cobol_common.Platform.TYPES.platform
  -> dialect: Cobol_config.dialect
  -> source_format: Cobol_config.source_format_spec
  -> ?ppf:Format.formatter
  -> Src_input.t
  -> unit Preproc_outputs.with_diags

val lex_file
  : platform: Cobol_common.Platform.TYPES.platform
  -> dialect: Cobol_config.dialect
  -> source_format: Cobol_config.source_format_spec
  -> ?ppf:Format.formatter
  -> string
  -> unit Preproc_outputs.with_diags

val lex_lib
  : platform: Cobol_common.Platform.TYPES.platform
  -> dialect: Cobol_config.dialect
  -> source_format: Cobol_config.source_format_spec
  -> lookup_config: Cobol_common.Copybook.TYPES.lookup_config
  -> ?ppf:Format.formatter
  -> Cobol_common.Copybook.TYPES.fileloc
  -> unit Preproc_outputs.with_diags

(** [fold_source_lines ~dialect ~source_format ~skip_compiler_directives_text
    ~on_compiler_directive ~on_initial_source_format ~f input acc] applies [f
    line_number line acc] for each successive source line [line] of [input].
    [line_number] gives the line number for [line] (starting at [1]).  [line] is
    given empty to [f] if it corresponds to an empty line in the input, or was a
    line continuation in the case of fixed-width reference format.

    When given, [on_compiler_directive] is called {e after} [f] has been fed
    with the text of a compiler directive, with the same line number.

    When given, [on_change_of_source_format] is called each time the source
    format changes: once at the begining {e before} [f] has been called,
    and then once each time a compiler directive that changes the source format
    is encountered (right after the text is fed to [f] but before
    [on_compiler_directive] is called).

    When set, [skip_compiler_directives_text] ([false] by default) prevents the
    text of compiler directives from being fed to [f].  If given,
    [on_compiler_directive] is called as if the text had been fed to [f].

    Diagnostics resulting from lexing and parsing the input are attached to the
    returned accumulated value. *)
val fold_source_lines
  : platform: Cobol_common.Platform.TYPES.platform
  -> dialect: Cobol_config.dialect
  -> source_format: Cobol_config.source_format_spec
  -> ?on_change_of_source_format: (Src_format.any -> 'a -> 'a)
  -> ?skip_compiler_directives_text: bool
  -> ?on_compiler_directive
     : (int -> Preproc_directives.compiler_directive with_loc -> 'a -> 'a)
  -> f:(int -> Text.text -> 'a -> 'a)
  -> Src_input.t
  -> 'a
  -> 'a

val scan_prefix_for_copybook
  : platform: Cobol_common.Platform.TYPES.platform
  -> dialect: Cobol_config.dialect
  -> source_format: Cobol_config.source_format_spec
  -> Src_input.t
  -> [`Program | `Copybook]

val preprocess_input
  : options: Preproc_options.preproc_options
  -> ?ppf:Format.formatter
  -> Src_input.t
  -> unit Preproc_outputs.with_diags

val preprocess_file
  : options: Preproc_options.preproc_options
  -> ?ppf:Format.formatter
  -> string
  -> unit Preproc_outputs.with_diags

val text_of_file
  : options: Preproc_options.preproc_options
  -> string
  -> Text.t Preproc_outputs.with_diags

val text_of_input
  : options: Preproc_options.preproc_options
  -> Src_input.t
  -> Text.t Preproc_outputs.with_diags
