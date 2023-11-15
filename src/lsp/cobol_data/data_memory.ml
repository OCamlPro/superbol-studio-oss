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

(* (\** Named address in memory *\) *)
(* type address = string *)
(* [@@deriving show, ord] *)

(** Size and offsets are affine expressions on symbolic (assumed constant)
    elementary sizes with symbolic variables. *)

type elementary_size =
  | Size_of_dynamic_table
  | Size_of_byte
[@@deriving ord]

let pp_elementary_size ppf = function
  | Size_of_dynamic_table ->
      Pretty.string ppf "size-of-dynamic-table"
  | Size_of_byte ->
      Pretty.string ppf "size-of-byte"
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

open AE

(* higher level operations *)

(* let address: string -> address = Fun.id                             (\* for now *\) *)

(* --- *)

exception NON_LINEAR = AE.NON_LINEAR
exception NOT_SCALAR = AE.NOT_SCALAR

type factor = AE.factor
[@@deriving show]

type size = linexpr
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
let size_of_dynamic_table: size = elementary_size Size_of_dynamic_table
let size_of_byte: size = elementary_size Size_of_byte

let as_int = AE.as_int

let increase: size -> by:size -> size = AE.add
let diff: size -> size -> size = fun a b -> AE.sub a ~by:b
let repeat: size -> by:factor -> size = AE.mult
let mult_int: size -> int -> size = fun s by -> repeat s ~by:(int by)

(* --- *)

let no_offset: offset = point_size

let shift = increase                                      (* alias for offset *)
let size ~from ~to_ = diff to_ from
