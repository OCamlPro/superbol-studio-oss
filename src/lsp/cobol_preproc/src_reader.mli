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

(** {1 Source text reader} *)

type t

(** {1 Creation} *)

val from
  : ?source_format: Src_format.any -> Src_input.t -> t

(** {1 Queries} *)

val diags: t -> Src_diagnostics.t
val position: t -> Lexing.position
val input_file: t -> string option
val source_format: t -> Src_format.any
val rev_comments: t -> Text.comments
val rev_ignored: t -> lexloc list
val rev_newline_cnums: t -> int list
val next_chunk: t -> t * Text.t

val fold_lines
  : dialect: Cobol_config.dialect
  -> ?skip_compiler_directives_text: bool
  -> ?on_compiler_directive
     : (int -> Preproc_directives.compiler_directive with_loc -> 'a -> 'a)
  -> f:(int -> Text.t -> 'a -> 'a)
  -> t -> 'a -> 'a

val print_lines
  : dialect: Cobol_config.dialect
  -> ?skip_compiler_directives_text: bool
  -> Format.formatter -> t -> unit

val try_compiler_directive
  : dialect: Cobol_config.dialect -> Text.t
  -> ((Text.t * Preproc_directives.compiler_directive with_loc * Text.t) option,
      Text.t * Preproc_diagnostics.error * Text.t) result

(** {1 Change of source format} *)

val with_source_format
  : Src_format.any with_loc -> t -> (t, Preproc_diagnostics.error) result

(** {1 Resetting the input} *)

(** Note: the functions below assume [position] corresponds to the begining of
    the input.} *)

val restart_on_string
  : ?source_format: Src_format.any -> ?position: Lexing.position
  -> string -> t -> t
val restart_on_channel
  : ?source_format: Src_format.any -> ?position: Lexing.position
  -> in_channel -> t -> t
