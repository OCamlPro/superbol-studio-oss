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

open EzCompat

(** Cobol text tokenization *)

(** {2 Compilation group tokens} *)

(** Tokens passed to {!Parser}; can be obtained via {!tokenize_text}. *)
type token' = Grammar_tokens.token Cobol_ptree.with_loc

val pp_token: Grammar_tokens.token Pretty.printer
val pp_token': token' Pretty.printer
val pp_token'_list: token' list Pretty.printer
val pp_tokens_with_loc_info: ?fsep:Pretty.simple -> token' list Pretty.printer

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
  : ?verbose: bool
  -> ?show_if_verbose: [> `Tks | `Ctx] list
  -> exec_scanners: Parser_options.exec_scanners
  -> memory: 'a memory
  -> intrinsics: StringSet.t
  -> ?default_intrinsics: StringSet.t
  -> Cobol_config.words_spec
  -> 'a state

val diagnostics
  : 'a state
  -> Parser_diagnostics.t

val parsed_tokens
  : Cobol_common.Behaviors.eidetic state
  -> token' list Lazy.t

val tokenize_text
  : source_format: Cobol_preproc.Src_format.any
  -> 'a state
  -> Cobol_preproc.Text.t
  -> (token' list, [>`MissingInputs | `ReachedEOF of token' list]) result * 'a state

val next_token
  : 'a state
  -> token' list
  -> ('a state * token' * token' list) option

val put_token_back
  : 'a state
  -> token'
  -> token' list
  -> 'a state * token' list

(* --- *)

val enable_intrinsics
  : 'a state
  -> token'
  -> token' list
  -> 'a state * token' * token' list

val disable_intrinsics
  : 'a state
  -> token'
  -> token' list
  -> 'a state * token' * token' list

val reset_intrinsics
  : 'a state
  -> token'
  -> token' list
  -> 'a state * token' * token' list

val replace_intrinsics
  : 'a state
  -> string Cobol_ptree.with_loc list option
  -> 'a state

val decimal_point_is_comma
  : 'a state
  -> token'
  -> token' list
  -> 'a state * token' * token' list

(* --- *)

val push_contexts
  : 'a state
  -> token' list
  -> Grammar_contexts.context list
  -> 'a state * token' list

val top_context
  : _ state
  -> Grammar_contexts.context option

val pop_context
  : 'a state
  -> token' list
  -> 'a state * token' list

val enable_context_sensitive_tokens: _ state -> unit
val disable_context_sensitive_tokens: _ state -> unit
val save_intrinsics: _ state -> unit
val restore_intrinsics: _ state -> unit
