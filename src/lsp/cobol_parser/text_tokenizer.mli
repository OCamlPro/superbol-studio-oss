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

(** Cobol text tokenization *)

(** {2 Compilation group tokens} *)

(** Tokens passed to {!Parser}; can be obtained via {!tokenize_text}. *)
type token = Grammar_tokens.token Cobol_ptree.with_loc
type tokens = token list
val pp_token: token Pretty.printer
val pp_tokens: tokens Pretty.printer
val pp_tokens': ?fsep:Pretty.simple -> tokens Pretty.printer

(* --- *)

(** A type of memory that may either forget or rememeber all parsed tokens. *)
type 'a memory
val amnesic: Cobol_common.Behaviors.amnesic memory
val eidetic: Cobol_common.Behaviors.eidetic memory

(** State of a tokenizer for compilation group parsing.

    Such a state is needed to deal with token combinations and simple syntax
    rules for picture clauses, as well as to keep track of lexer configurations.

    Contrary to an [eidetic state] that allows to retrieve every previously
    parsed token, an [amnesic state] is a tokenizer state that does not keep
    hold of any such memory. *)
type 'a state

(** Initialization function, based on the desired memory and a given set of
    tokens that are considered context-sensitive.  The latter are {e disabled}
    (i.e, not considered as keywords) upon initialization; calls to
    {!enable_tokens} or {!disable_tokens} are needed to enable or disable them
    when needed.  *)
val init
  : ?verbose:bool
  -> ?show_if_verbose:[> `Tks | `Ctx] list
  -> exec_scanners: Parser_options.exec_scanners
  -> memory:'a memory
  -> Cobol_config.words_spec
  -> 'a state

val diagnostics
  : 'a state
  -> Parser_diagnostics.t

val parsed_tokens
  : Cobol_common.Behaviors.eidetic state
  -> tokens Lazy.t

val tokenize_text
  : source_format: Cobol_preproc.Src_format.any
  -> 'a state
  -> Cobol_preproc.Text.t
  -> (tokens, [>`MissingInputs | `ReachedEOF of tokens]) result * 'a state

val next_token
  : 'a state
  -> tokens
  -> ('a state * token * tokens) option

val put_token_back
  : 'a state
  -> token
  -> tokens
  -> 'a state * tokens

(* --- *)

val decimal_point_is_comma
  : 'a state
  -> token
  -> tokens
  -> 'a state * token * tokens

(* --- *)

val push_contexts
  : 'a state
  -> tokens
  -> Grammar_contexts.context list
  -> 'a state * tokens

val top_context
  : _ state
  -> Grammar_contexts.context option

val pop_context
  : 'a state
  -> tokens
  -> 'a state * tokens

val enable_context_sensitive_tokens: _ state -> unit
val disable_context_sensitive_tokens: _ state -> unit
