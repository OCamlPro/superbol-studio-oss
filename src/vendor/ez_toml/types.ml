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

open EzCompat

type key = string
type key_path = key list

type location = {
  file : string ;
  mutable line_begin : int ;
  mutable line_end : int ;
  mutable char_begin : int ;
  mutable char_end : int ; (* in line *)
}

type context =
  | InsideTable (* this value is inside a block table [x] *)
  | InsideArray (* this value is inside an array x =[ ... ] *)
  | InsideInlineTable (* this value is inside an inline table x = { ... } *)

type format =
  | Any       (* Guess automatically. Usually, use block format *)
  | Inline    (* Put the value on a single line *)
  | Multiline (* Put the value on multiple lines if possible *)
  | Literal   (* Use literal quotes (only for strings) *)

(* Not all errors can be silent, here are the ones that can be:
   * 3 : array access [x.y] for inline array x = []
   * 4 : try to set an existing key (allow override)
   * 8 : array item [[x]] for an existing non-table array
   * 16 : table item [x] already exists
   * 20 : duplicate key in inline table { x = 1, x = 2 }
   * 21 : extensions (root table, extended operators ==, +=, -=)

  Default silent errors are: [ 16 ]
*)
type config = {
  debug : bool ;
  silent_errors : IntSet.t ;
  newline : string ;      (* newline to be used *)
}


type table = node StringMap.t

and node = {
  (* comments on preceeding lines *)
  mutable node_comment_before : string list ;
  (* comment at end of line *)
  mutable node_comment_after : string option ;
  node_loc : location ;
  node_format : format ;
  node_pos : int ;     (* a position *)
  mutable node_value : value ;
  node_name : key_path ;
}

and value =
  | Table of table
  | Array of node array
  | String of string
  | Bool of bool
  | Int of string
  | Float of string
  | Date of string


type error =
  | Parse_error                                   (* error 0 *)
  | Syntax_error of string                        (* error 1 *)
  | Invalid_lookup                                (* error 2 *)
  | Invalid_lookup_in_inline_array                (* error 3 *)
  | Key_already_exists of key_path                (* error 4 *)
  | Invalid_key_set of key                        (* error 5 *)
  | Invalid_table of key_path                     (* error 6 *)
  | Append_item_to_non_array of key_path          (* error 7 *)
  | Append_item_to_non_table_array of key_path    (* error 8 *)
  | Invalid_escaped_unicode of string             (* error 9 *)
  | Expected_error_before_end_of_file of int      (* error 10 *)
  | Expected_error_did_not_happen of int          (* error 11 *)
  | Expected_error_but_another_error of int * location * int * error
  (* error 12 *)
  | Forbidden_escaped_character                   (* error 13 *)
  | Unterminated_string                           (* error 14 *)
  | Control_characters_must_be_escaped of char    (* error 15 *)
  | Duplicate_table_item of key_path              (* error 16 *)
  | Type_mismatch of value * string                (* error 17 *)
  | Bad_convertion of value * string               (* error 18 *)
  | Invalid_lookup_in_empty_array                 (* error 19 *)
  | Key_already_exists_in_inline_table of key_path  (* error 20 *)
  | Invalid_use_of_extension of string            (* error 21 *)

exception Error of location * int * error
