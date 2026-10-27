(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Typed terms: mirror of {!Cobol_ptree} terms where references to data items
    are resolved, and conditions are expanded. *)

open Cobol_common.Srcloc.TYPES

(** Outcome of the typing of a part of the tree: [Error ()] means that this part
    is ill-typed (the corresponding diagnostic is already reported), and is not
    analyzed any further. *)
type 'a or_error = ('a, unit) result [@@deriving ord]

(** Resolved reference to some ['a] *)
type ('a, 'ref_name) resolved_reference = {
  resolved_name: 'ref_name;
  resolved: 'a or_error; [@compare fun _ _ -> 0] (* note: skipped in visitor *)
}
[@@deriving ord]

type 'a resolved_name = ('a, Cobol_ptree.name with_loc) resolved_reference
[@@deriving ord]

type 'a resolved_qualname =
  ('a, Cobol_ptree.qualname with_loc) resolved_reference
[@@deriving ord]

(** Reference to a data item (field, renaming, condition-name, or index).
    References are compared by name, as in the parse tree. *)
type data_ref =
  (Cobol_data.Types.data_definition [@compare fun _ _ -> 0])
    resolved_qualname
[@@deriving ord]

(** Same as {!data_ref}, for positions where the parse tree only allows a single
    unqualified name. *)
type data_name_ref =
  (Cobol_data.Types.data_definition [@compare fun _ _ -> 0])
    resolved_name
[@@deriving ord]

type literal = Cobol_data.Types.literal_value
[@@deriving ord]

(** Identifiers without reference modification. Literals are not identifiers:
    they do not refer to any data item, and are represented by their
    {!Cobol_data} value (see {!or_ident}). *)
type base_ident =
  | Address of address
  | Counter of Cobol_ptree.counter
  | InlineCall of inline_call
  | InlineInvoke of inline_invocation
  | ObjectView of object_view
  | ObjectRef of Cobol_ptree.object_ref
  | QualIdent of qualident

(** Identifiers, with a possible reference modification. *)
and ident = {
  ident_base: base_ident;
  ident_refmod: refmod option;
}

(** Either an identifier, or the value of a literal (e.g, [literal or_ident]),
    which is [Error ()] for an invalid literal. *)
and 'lit or_ident =
  | Ident of ident
  | Literal of 'lit or_error

and ident_or_alphanum = Cobol_data.Types.alphanum_value or_ident

and ident_or_intlit = Cobol_data.Types.integer_value or_ident

and ident_or_literal = literal or_ident

and ident_or_nonnum = literal or_ident (* non-numeric literals only *)

and ident_or_strlit = literal or_ident (* string literals only *)

and scalar =
  | ScalarIdent of ident
  | ScalarLiteral of literal or_error
  | LengthOf of ident_or_literal

and expr =
  | Atom of scalar
  | Unop of Cobol_ptree.unop * expr with_loc
  | Binop of expr with_loc * Cobol_ptree.binop * expr with_loc

and inline_call =
  | CallFunc of {
      func: Cobol_ptree.name with_loc;
      args: effective_arg list;
    }
  | CallGenericIntrinsic of {
      func: Cobol_ptree.intrinsic_name with_loc;
      args: effective_arg list;
    }
  | CallTrim of {
      arg: effective_arg;
      tip: Cobol_ptree.trimming_tip option;
    }
  | CallLength of {
      arg: ident_or_nonnum;
      physical: bool;
    }
  | CallNumvalC of ident_or_nonnum list
  | CallLocaleDate of locale_func_args
  | CallLocaleTime of locale_func_args
  | CallLocaleTimeFromSeconds of locale_func_args
  | CallFormattedDatetime of formatted_func_args
  | CallFormattedTime of formatted_func_args

and formatted_func_args = {
  formatted_func_args: effective_arg list;
  formatted_func_system_offset: bool;
}

and locale_func_args = {
  locale_func_args: effective_arg;
  locale_func_locale: Cobol_ptree.qualname option;
}

and effective_arg =
  | ArgExpr of expr with_loc
  | ArgOmitted

and qualident = {
  ident_data_ref: data_ref;
  ident_subscripts: subscript with_loc list;
}

and subscript =
  | SubSAll
  | SubSExpr of expr with_loc
  | SubSIdx of data_name_ref * Cobol_data.Types.integer_value
  (** relative index, with its signed offset *)

and refmod = {
  refmod_left: expr with_loc;
  refmod_length: expr with_loc option;
}

and inline_invocation = {
  invoke_class: ident;
  invoke_meth: literal or_error;
  invoke_args: effective_arg list;
}

and object_view = {
  object_view_ident: ident;
  object_view_spec: Cobol_ptree.object_view_spec;
}

and address =
  | DataAddress of ident
  | ProgAddress of ident_or_literal
[@@deriving ord]

(** Expanded conditions, where all the implied subjects and relational operators
    of abbreviated conditions are made explicit. Sub-conditions are [Error ()]
    when they could not be expanded (e.g, missing subject). *)
type condition =
  | Expr of expr with_loc  (** expression used as a condition *)
  | Relation of expr with_loc * Cobol_ptree.relop * expr with_loc (** {v e <relop> e' v} *)
  | ClassCond of expr with_loc * Cobol_ptree.class_  (** class condition *)
  | SignCond of expr with_loc * Cobol_ptree.signz (** {v e POSITIVE/NEGATIVE/ZERO v} *)
  | Omitted of expr with_loc  (** {v c OMITTED v} *)
  | Not of condition or_error with_loc  (** {v NOT c v} *)
  | Combined of
      condition or_error with_loc
      * Cobol_ptree.logop
      * condition or_error with_loc  (** {v c <AND/OR> c' v} *)
[@@deriving ord]
