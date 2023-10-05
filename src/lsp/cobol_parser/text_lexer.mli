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

module TYPES: sig
  type token_handle
  type lexing_options =
    {
      decimal_point_is_comma: bool;
    }
end
include module type of TYPES

module TokenHandles: sig
  include Set.S with type elt = token_handle
  val mem_text_token: Grammar_tokens.token -> t -> bool
end

val show_token: Grammar_tokens.token -> string
val show_token_of_handle: token_handle -> string

(* --- *)

val handle_of_token: Grammar_tokens.token -> token_handle
val reserve_words: Cobol_config.words_spec -> Cobol_common.Diagnostics.Set.t
val enable_tokens: TokenHandles.t -> unit
val disable_tokens: TokenHandles.t -> unit

(** Only for debugging *)
val keyword_of_token : (Grammar_tokens.token, string) Hashtbl.t
val punct_of_token : (Grammar_tokens.token, string) Hashtbl.t

(* --- *)

val default_lexing_options: lexing_options

(* --- *)

exception MultiToks of
    (Grammar_tokens.token * int) list         (* with length, except for last *)

(** [token ~options lexbuf] tokenizes a lexing buffer [lexbuf] into a simple
    token; may raise {!MultiToks} is the contents of the buffer is tokenized
    into more than one token. *)
val token
  : options: lexing_options
  -> Lexing.lexbuf
  -> Grammar_tokens.token

(** [token_of_string'] is similar to {!token}, except that it operates on a
    localized string and returns a token with its location.  May also raise
    {!MultiToks}. *)
val token_of_string'
  : options: lexing_options
  -> string Cobol_common.Srcloc.with_loc
  -> Grammar_tokens.token Cobol_common.Srcloc.with_loc

(** [tokens ~options lexbuf'] tokenizes a lexing buffer with location [lexbuf']
    into a list of localized tokens. *)
val tokens
  : options: lexing_options
  -> Lexing.lexbuf Cobol_common.Srcloc.with_loc
  -> Grammar_tokens.token Cobol_common.Srcloc.with_loc list

(** [tokens_of_string'] is similar to {!token}, except that it operates on a
    localized string. *)
val tokens_of_string'
  : options: lexing_options
  -> string Cobol_common.Srcloc.with_loc
  -> Grammar_tokens.token Cobol_common.Srcloc.with_loc list

(** [decode_symbolic_ebcdics' ~quotation s'] decodes the symbolic EBCDIC
    characters from the localized string [s'], and returns the resulting
    {!Grammar_tokens.ALPHANUM} token and a set of diagnostics.  In case of
    errors, the alphanumeric token returned may represent part of the encoded
    input. *)
val decode_symbolic_ebcdics'
  : quotation: Cobol_preproc.Text.quotation
  -> string Cobol_common.Srcloc.with_loc
  -> Grammar_tokens.token Cobol_common.Srcloc.with_loc *
     Cobol_common.Diagnostics.Set.t
