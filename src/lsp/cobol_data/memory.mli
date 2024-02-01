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

type elementary_size =
  | Size_of_C_double
  | Size_of_C_float
  | Size_of_C_long
  | Size_of_C_long_double
  | Size_of_dynamic_table
  | Size_of_index
  | Size_of_pointer
[@@deriving show, ord]

type symbolic_var = private
  | Valof of Cobol_ptree.Types.qualname
[@@deriving show, ord]

type factor
[@@deriving show]

type size
[@@deriving show]

type offset = size
[@@deriving show]

exception NON_LINEAR of symbolic_var Cobol_common.Basics.NEL.t
exception NOT_SCALAR of [ `Vars of symbolic_var Cobol_common.Basics.NEL.t
                        | `Consts of elementary_size Cobol_common.Basics.NEL.t ]

(* --- *)

val int: int -> factor
val valof: Cobol_ptree.Types.qualname -> factor

val point_size: size                                             (* null-size *)
val const_size: int -> size
val valof_size: Cobol_ptree.Types.qualname -> size
val elementary_size: elementary_size -> size
val bit_size: size
val byte_size: size
val size_of_C_double: size
val size_of_C_float: size
val size_of_C_long: size
val size_of_C_long_double: size
val size_of_dynamic_table: size
val size_of_index: size
val size_of_pointer: size

(** Raises {!NOT_SCALAR} in case of failure. *)
val as_int: size -> int

val add: size -> size -> size
val diff: size -> size -> size
val increase: size -> by:size -> size

(** May raise {!NON_CONST}. *)
val repeat: size -> by:factor -> size

val mult_int: size -> int -> size

(* --- *)

val no_offset: offset
val shift: offset -> by:size -> offset
val size: from:offset -> to_:offset -> size
