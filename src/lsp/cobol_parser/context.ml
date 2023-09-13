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

(** {1 Context management}

    This module defines syntactical contexts that are used to enable keywords
    that are context sensitive {i w.r.t} grammar rules.  Such rules are
    annotated using [[@context]] attribules, that describe a context which is:

    - entered {e after} the {e first item} on the rules' right-hand side has
      been observed.
      NOTE: for now this item must be a terminal symbol;

    - exited whenever the annotated rule is reduced.
*)

(** {2 Context types}

    We concretely represent a context [ctx] as the set of keywords that are
    sensitive to [ctx].
*)

module TH = Text_lexer.TokenHandles

type context = TH.t
let pp_context ppf c =
  Pretty.list ~fopen:"{@[" ~fclose:"@]}" ~fempty:"{}" begin fun ppf h ->
    Pretty.string ppf (Text_lexer.show_token_of_handle h)
  end ppf (TH.elements c)

type t = context

(** {2 Context stack} *)

(** The type context stacks as managed by the parser. *)
type stack = entry list

(** As the set of context-sensitive keywords may not by disjoint between
    contexts, each element of the stack maintains the difference that a context
    [ctx] brings {i w.r.t} all contexts that were inserted in the stack before
    [ctx]. *)
and entry =
  {
    ctx: context;
    diff: tokens_diff;
  }
and tokens_diff = TH.t

(** {3 Usual operations on context stacks} *)

let empty_stack: stack = []

let push: context -> stack -> stack = fun ctx -> function
  | [] -> [ { ctx; diff = ctx } ]
  | { diff = top; _ } :: _ as t -> { ctx; diff = TH.diff ctx top } :: t

let top: stack -> context option = function
  | { ctx; _ } :: _ -> Some ctx
  | [] -> None

(** {3 Context-specific operations} *)

(** Retrieve the difference between the hidden part of the stack ({i i.e} all
    its elements, minus the top one), and the top element of the stack (if any),
    in terms of a set of context-sensitive keywords; returns an empty set if the
    stack is empty. *)
let top_tokens: stack -> TH.t = function
  | [] -> TH.empty
  | { diff; _ } :: _ -> diff

(** [pop stack] pops the top element [ctx] from [stack]. Retrieves the
    difference between the hidden part of the stack [stack] ({i i.e} all its
    elements, minus [ctx]), and [ctx], in terms of a set of context-sensitive
    keywords.  Raises {!Invalid_argument} if the stack is empty. *)
let pop: stack -> stack * TH.t = function
  | [] -> Pretty.invalid_arg "Unable to pop on an empty context stack"
  | { diff; _ } :: tl -> tl, diff
