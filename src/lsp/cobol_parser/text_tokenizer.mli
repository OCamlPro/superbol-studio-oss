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

module TEXT = Cobol_preproc.Text

(** {2 Compilation group tokens} *)

(** Tokens passed to {!Parser}; can be obtained via {!tokenize_text}. *)
type token = Grammar_tokens.token Cobol_ptree.with_loc
type tokens = token list
val pp_token: token Pretty.printer
val pp_tokens: tokens Pretty.printer

module Make (Config: Cobol_config.T): sig

  (** A type of memory that may either forget or rememeber all parsed tokens. *)
  type 'a memory
  val amnesic: Cobol_common.Behaviors.amnesic memory
  val eidetic: Cobol_common.Behaviors.eidetic memory

  (** State of a tokenizer for compilation group parsing.

      Such a state is needed to deal with token combinations and simple syntax
      rules for picture clauses, as well as to keep track of lexer
      configurations.

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
    : 'a memory
    -> context_sensitive_tokens: Text_lexer.TokenHandles.t
    -> 'a state

  val diagnostics
    : 'a state
    -> Cobol_common.Diagnostics.Set.t

  val parsed_tokens
    : Cobol_common.Behaviors.eidetic state
    -> tokens Lazy.t

  val tokenize_text
    : source_format: Cobol_preproc.Src_format.any
    -> 'a state
    -> TEXT.t
    -> (tokens, [>`MissingInputs | `ReachedEOF of tokens]) result * 'a state

  val next_token
    : 'a state
    -> tokens
    -> ('a state * token * tokens) option

  val enable_tokens
    : 'a state
    -> tokens
    -> Text_lexer.TokenHandles.t
    -> 'a state * tokens

  val disable_tokens
    : 'a state
    -> tokens
    -> Text_lexer.TokenHandles.t
    -> 'a state * tokens

  val put_token_back
    : 'a state
    -> token
    -> tokens
    -> 'a state * tokens

  val decimal_point_is_comma
    : 'a state
    -> token
    -> tokens
    -> 'a state * token * tokens

end
