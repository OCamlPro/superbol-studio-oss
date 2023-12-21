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

(** Size and offsets are affine expressions on symbolic (assumed constant)
    elementary sizes with symbolic variables. *)

type elementary_size =
  | Size_of_C_double
  | Size_of_C_float
  | Size_of_C_long
  | Size_of_C_long_double
  | Size_of_dynamic_table
  | Size_of_index
  | Size_of_pointer
[@@deriving ord]

let pp_elementary_size ppf s =
  Pretty.string ppf @@ match s with
  | Size_of_C_double      -> "size-of-C-double"
  | Size_of_C_float       -> "size-of-C-float"
  | Size_of_C_long        -> "size-of-C-long"
  | Size_of_C_long_double -> "size-of-C-long-double"
  | Size_of_dynamic_table -> "size-of-dynamic-table"
  | Size_of_index         -> "size-of-index"
  | Size_of_pointer       -> "size-of-pointer"
let show_elementary_size = Pretty.to_string "%a" pp_elementary_size

(* --- *)

type symbolic_var =
  | Valof of Cobol_ptree.qualname
[@@deriving ord]

let pp_symbolic_var ppf = function
  | Valof qn ->
      Pretty.print ppf "@[(valof@;<1 2>%a)@]" Cobol_ptree.pp_qualname qn
let show_symbolic_var = Pretty.to_string "%a" pp_symbolic_var

module AE =
  Cobol_common.Symbolic.Linear_exprs
    (struct type t = symbolic_var    [@@deriving show, ord] end)
    (struct type t = elementary_size [@@deriving show, ord] end)

exception NON_LINEAR = AE.NON_LINEAR
exception NOT_SCALAR = AE.NOT_SCALAR

(* higher level operations *)

type factor = AE.factor
[@@deriving show]

type size = AE.linexpr
[@@deriving show]

type offset = size
[@@deriving show]

let int: int -> factor = AE.int
let valof s : factor = AE.var (Valof s)

let point_size: size = AE.zero
let const_size: int -> size = function
  | 0 -> point_size
  | i -> AE.factor (int i)
let valof_size s = AE.factor @@ valof s
let elementary_size: elementary_size -> size = AE.const

let as_int = AE.as_int

let add: size -> size -> size = AE.add
let diff: size -> size -> size = fun a b -> AE.sub a b
let increase: size -> by:size -> size = fun s ~by -> add s by
let repeat: size -> by:factor -> size = AE.mult
let mult_int: size -> int -> size = fun s by -> repeat s ~by:(int by)

let bit_size: size = const_size 1
let byte_size: size = mult_int bit_size 8

let size_of_C_double      = elementary_size Size_of_C_double
let size_of_C_float       = elementary_size Size_of_C_float
let size_of_C_long        = elementary_size Size_of_C_long
let size_of_C_long_double = elementary_size Size_of_C_long_double
let size_of_dynamic_table = elementary_size Size_of_dynamic_table
let size_of_index         = elementary_size Size_of_index
let size_of_pointer       = elementary_size Size_of_pointer

(* --- *)

let no_offset: offset = point_size

let shift o ~by = add o by                                (* alias for offset *)
let size ~from ~to_ = diff to_ from
