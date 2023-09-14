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

(** {2 Context(s)} *)

type context = Grammar_contexts.t
val pp_context: context Pretty.printer

type t = context
(** {2 Context stack} *)

type stack

(** {3 Usual operations on context stacks} *)

val empty_stack: stack
val push: context -> stack -> stack
val top: stack -> context option

(** {3 Context-specific operations} *)

val top_tokens: stack -> Text_lexer.TokenHandles.t
val pop: stack -> stack * Text_lexer.TokenHandles.t
