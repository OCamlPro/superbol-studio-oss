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

(* type address *)
(* [@@deriving show, ord] *)

(* type reference = *)
(*   | Address of address *)
(* [@@deriving show, ord] *)

type elementary_size =
  | Size_of_dynamic_table
  | Size_of_byte
[@@deriving show, ord]

type symbolic_var = private
  | Valof of Cobol_ptree.qualname
[@@deriving show, ord]

type factor
[@@deriving show]

(** Note: for now, it is possible to represent sizes in an arbitrary unit by
    using {!const_size}, so {!as_int} is usable directly.  Instead, we may want
    to compute every sizes as factors of elementary sizes, and not use the
    implicit arbitrary unit: then some substitutions/(partial-)evaluations would
    be enough to get ints when relevant. *)
type size
[@@deriving show]

type offset = size
[@@deriving show]

exception NON_LINEAR of symbolic_var Cobol_common.Basics.NEL.t
exception NOT_SCALAR of [ `Vars of symbolic_var Cobol_common.Basics.NEL.t
                        | `Consts of elementary_size Cobol_common.Basics.NEL.t ]

(* val address: string -> address *)

(* --- *)

val int: int -> factor
val valof: Cobol_ptree.qualname -> factor

val point_size: size                                             (* null-size *)
val const_size: int -> size
val valof_size: Cobol_ptree.qualname -> size
val elementary_size: elementary_size -> size
val size_of_dynamic_table: size
val size_of_byte: size

(** Raises {!NOT_SCALAR} in case of failure. *)
val as_int: size -> int

val increase: size -> by:size -> size
val diff: size -> size -> size

(** May raise {!NON_CONST}. *)
val repeat: size -> by:factor -> size

val mult_int: size -> int -> size

(* --- *)

val no_offset: offset
val shift: offset -> by:size -> offset
val size: from:offset -> to_:offset -> size
