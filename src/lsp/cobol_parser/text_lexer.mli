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
  type keyword_handle
  type intrinsic_handle
  type lexer
end
include module type of TYPES

module TokenHandles: sig
  include Set.S with type elt = keyword_handle
  val mem_text_token: Grammar_tokens.token -> t -> bool
end

module IntrinsicHandles: Set.S with type elt = intrinsic_handle

(* --- *)

val show_token: Grammar_tokens.token -> string
val show_token_of_handle: keyword_handle -> string
val pp_tokens_via_handles: TokenHandles.t Pretty.printer

(** Only for debugging *)
val word_of_token : (Grammar_tokens.token, string) Hashtbl.t
val punct_of_token : (Grammar_tokens.token, string) Hashtbl.t


(* --- *)

val create: ?decimal_point_is_comma:bool -> unit -> lexer
val handle_of_token: lexer -> Grammar_tokens.token -> keyword_handle
val reserve_words: lexer -> Cobol_config.words_spec -> unit
val intrinsic_handles: lexer -> string list -> IntrinsicHandles.t

val token_of_intrinsic: string -> Grammar_tokens.token

val enable_keywords: TokenHandles.t -> unit
val disable_keywords: TokenHandles.t -> unit
val decimal_point_is_comma: lexer -> lexer
val register_intrinsics: lexer -> IntrinsicHandles.t -> unit
val unregister_intrinsics: lexer -> IntrinsicHandles.t -> unit

(* --- *)

(** [tokens ~options lexbuf'] tokenizes a lexing buffer with location [lexbuf']
    into a list of localized tokens. *)
val tokens
  : lexer
  -> Lexing.lexbuf Cobol_common.Srcloc.with_loc
  -> Grammar_tokens.token Cobol_common.Srcloc.with_loc list

(** [tokens_of_string'] is similar to {!token}, except that it operates on a
    localized string. *)
val tokens_of_string'
  : lexer
  -> string Cobol_common.Srcloc.with_loc
  -> Grammar_tokens.token Cobol_common.Srcloc.with_loc list

(* --- *)

(** {1 Alphanumerics with symbolic EBCDIC characters} *)

(** [decode_symbolic_ebcdics' ~quotation s'] decodes the symbolic EBCDIC
    characters from the localized string [s'], and returns the resulting
    {!Grammar_tokens.ALPHANUM} token and a set of diagnostics.  In case of
    errors, the alphanumeric token returned may represent part of the encoded
    input. *)
val decode_symbolic_ebcdics'
  : quotation: Cobol_ptree.alphanum_quote
  -> string Cobol_common.Srcloc.with_loc
  -> Grammar_tokens.token
    Cobol_common.Srcloc.with_loc
    Parser_diagnostics.Accumulator.with_diags
