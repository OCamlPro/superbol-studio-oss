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

(* open Cobol_common.Srcloc.TYPES *)
(* open Cobol_common.Srcloc.INFIX *)
(* open Cobol_ast *)

(* TODO: those properties should be re-introduced later when relevant; they are
   commented for now as they do not appear necessary to the classicfication of
   data items. *)

(* type bit_length = *)
(*   | L16 *)
(*   | L32 *)
(*   | L62 [@@deriving show] *)

(* type pointer_length = *)
(*   | L4 *)
(*   | L8 [@@deriving show] *)

(* type numeric_format = { *)
(*   signed: bool; *)
(*   integer_length: int; *)
(*   decimal_length: int; *)
(* } [@@deriving show] *)

(* type numeric_encoding = *)
(*   | Ascii *)
(*   | Bcd *)
(*   | Int of bit_length *)
(*   | Float of bit_length [@@deriving show] *)

(* (\* NOTE: The numeric does not support yet the floating point format of the 2014 standard *\) *)
(* type numeric = *)
(*   (numeric_format * numeric_encoding) [@@deriving show] *)

(* type alphanumeric_category = *)
(*   | Alphanumeric of int *)
(*   | AlphanumericEdited of int [@@deriving show] *)

(* type national_category = (\* UTF-16 *\) *)
(*   | National of int *)
(*   | NationalEdited of int [@@deriving show] *)

type elementary_data_class =
  | Alphabetic
  | Alphanumeric
  | Boolean
  | Index
  | National
  | Numeric
  | Object
  | Pointer
[@@deriving show]

type data_type =
  | Elementary of elementary_data_class leveled pictured
  | Table of table_type leveled
  | Group of data_type Cobol_ptree.with_loc list leveled
[@@deriving show]

and 'a leveled = {                             (* TODO: no need to keep levels *)
  typ: 'a;
  level: int;
} [@@deriving show]

and 'a pictured = 'a * Data_picture.t option [@@deriving show]

and table_type = {                          (* TODO: inline in `Table/OCcurs` *)
  elements_type: data_type Cobol_ptree.with_loc;
  length: table_length;
}

and table_length =
  | Fixed of Cobol_ptree.integer
  | OccursDepending of      (* TODO: get rid of that (duplicate of AST nodes) *)
      {      (* TODO: resolve depending before building the final type repr.  *)
        min_size: Cobol_ptree.integer;
        max_size: Cobol_ptree.integer;
        depending: Cobol_ptree.qualname Cobol_ptree.with_loc;
      } [@@deriving show]

(* let loc_of = function *)
(*   | Elementary typ_loc -> ~@typ_loc *)
(*   | Group fields_loc -> ~@fields_loc *)
(*   | Table elements_loc -> ~@elements_loc *)

(* let level_of = function *)
(*   | Elementary { payload = {level; _}, _; _ } *)
(*   | Group { payload = {level; _}; _ } *)
(*   | Table { payload = {level; _}; _ } -> level *)

(* let pp_cob_data_type_loc fmt data_type = *)
(*   pp_cob_data_type fmt data_type *)
