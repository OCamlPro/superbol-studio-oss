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

(** Representation of COBOL literals *)

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module VAL = Data_value
module OUT = Data_diagnostics

type integer =
  {
    int_literal: Cobol_ptree.integer;                              (* option? *)
    int_value: VAL.integer;
  }

type fixed =
  {
    fixed_literal: Cobol_ptree.fixed;                              (* option? *)
    fixed_value: VAL.fixed;
  }

type floating =
  {
    float_literal: Cobol_ptree.floating;                           (* option? *)
    float_value: VAL.floating;
  }

type alphanum = VAL.alphanum [@@deriving show]

type boolean =
  {
    bool_literal: Cobol_ptree.boolean;                             (* option? *)
    bool_value: VAL.boolean;
  }
[@@deriving show]

(* --- *)

let error diags e = OUT.add_error e diags

let invalid_chars ~loc ~literal_class diags chars =
  VAL.NEL.fold_left ~f:begin fun diags (i, c) ->
    let loc = Cobol_common.Srcloc.trunc_prefix i loc in
    let loc = Cobol_common.Srcloc.prefix 1 loc in
    error diags @@
    Invalid { loc; stuff = Character_in_literal { literal_class; char = c } }
  end diags chars

let with_invalid_chars ~loc ~literal_class diags chars v =
  let diags = invalid_chars ~loc diags chars ~literal_class in
  OUT.result ~diags (v &@ loc)

(* --- *)

let pp_integer ppf x =
  VAL.pp_integer ppf x.int_value

let integer ({ payload = literal; loc }: Cobol_ptree.integer with_loc)
  : integer with_loc OUT.with_diags =
  try
    let int_value = VAL.integer_of_string literal in
    OUT.result ({ int_literal = literal; int_value } &@ loc)
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc OUT.none chars ~literal_class:Integer
      { int_literal = literal; int_value = VAL.integer_zero }

(* --- *)

let pp_fixed ppf x =
  Q.pp_print ppf x.fixed_value

let fixed ({ payload = literal; loc }: Cobol_ptree.fixed with_loc)
  : fixed with_loc OUT.with_diags =
  try
    let fixed_value =
      VAL.fixed_of_strings
        ~integral:literal.fixed_integral
        ~fractional:literal.fixed_fractional
    in
    OUT.result ({ fixed_literal = literal; fixed_value } &@ loc)
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc OUT.none chars ~literal_class:Fixed
      { fixed_literal = literal; fixed_value = VAL.fixed_zero }

(* --- *)

let pp_floating ppf x =
  VAL.pp_floating ppf x.float_value

let floating ({ payload = literal; loc }: Cobol_ptree.floating with_loc)
  : floating with_loc OUT.with_diags =
  try
    let float_value =
      VAL.floating_of_strings
        ~integral:literal.float_significand.fixed_integral
        ~fractional:literal.float_significand.fixed_fractional
        ~exponent:literal.float_exponent
    in
    OUT.result ({ float_literal = literal; float_value } &@ loc)
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc OUT.none chars ~literal_class:Floating
      { float_literal = literal; float_value = VAL.floating_zero }


(* --- *)

let boolean
    (* TODO deal with prefix length? *)
    ?(max_length = 8_191)                         (* as per ISO/IEC 1989:2014 *)
    Cobol_ptree.{ payload = { bool_base = base;
                              bool_value = literal_string } as bool_literal;
                  loc } =
  let diags = OUT.none in
  let len = String.length literal_string in
  let diags =
    if len > max_length
    then error diags @@ Overlong_literal { loc; literal_string; max_length }
    else diags
  in
  try
    let v = { bool_literal;
              bool_value = VAL.boolean_of_string ~base literal_string } in
    OUT.result ~diags (v &@ loc)
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc diags chars
      ~literal_class:(if base = `Bool then Boolean else Hexadecimal)
      { bool_literal; bool_value = VAL.boolean_zero }
