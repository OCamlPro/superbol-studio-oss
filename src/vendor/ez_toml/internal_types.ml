(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Types

type op =
  | OpEqual  (* =, set for the first time *)

  (* EXTENSIONS *)
  | OpInit   (* ==, only set if not already set *)
  | OpSet    (* :=, always set *)
  | OpUnset  (* -=, remove value if exists *)

type 'a loc = { txt : 'a ; loc : location }

type string_format =
  | Quoted
  | QuotedMultiline
  | Literal
  | LiteralMultiline

type 'a binding = {
  bind_var : key_path loc ;
  bind_op : op ;
  bind_val : 'a loc ;
}

type inline_value =
  | IBool of bool
  | IString of string_format * string
  | IInt of string
  | IFloat of string
  | IDate of string
  | ITable of inline_value binding list
  | IArray of inline_value loc list

type operation =
  | Set of inline_value binding
  | Table_item of key_path
  | Array_item of key_path
  | Error_item of int

type line = {
  mutable line_comments_before : string list ;
  mutable line_comment_after : string option ;
  line_operation : operation ;
  line_operation_loc : location ;      (* location of only the op *)
  mutable line_global_loc : location ;  (* location including comments *)
}
