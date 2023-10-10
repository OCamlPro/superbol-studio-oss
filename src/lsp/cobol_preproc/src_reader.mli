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

val from_file
  : source_format: Src_format.any -> string -> t
val from_string
  : ?filename: string -> source_format: Src_format.any -> string -> t
val from_channel
  : ?filename: string -> source_format: Src_format.any -> in_channel -> t

(** {1 Queries} *)

val diags: t -> Cobol_common.Diagnostics.Set.t
val position: t -> Lexing.position
val comments: t -> Text.comments
val source_format: t -> Src_format.any
val newline_cnums: t -> int list

val next_chunk: t -> t * Text.t
val fold_chunks: t -> (Text.t -> 'a -> 'a) -> 'a -> 'a

val fold_lines: t -> f:(int -> Text.t -> 'a -> 'a) -> 'a -> 'a
val print_lines: Format.formatter -> t -> unit

(** {1 Change of source format} *)

val with_source_format: 'k Src_format.source_format with_loc -> t -> t

(** {1 Resetting the input} *)

(** Note: the functions below assume [position] corresponds to the begining of
    the input.} *)

val restart_on_file: ?position: Lexing.position -> string -> t -> t
val restart_on_string: ?position: Lexing.position -> string -> t -> t
val restart_on_channel: ?position: Lexing.position -> in_channel -> t -> t
