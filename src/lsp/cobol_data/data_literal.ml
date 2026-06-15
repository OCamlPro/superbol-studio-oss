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
open Data_types

module VAL = Data_value
module OUT = Data_diagnostics

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

let alphanum = Cobol_ptree.alphanum_of_string

(* --- *)

let integer ({ payload = literal; loc }: Cobol_ptree.integer with_loc)
  : integer_literal with_loc OUT.with_diags =
  try
    let int_value = VAL.integer_of_string literal in
    OUT.result ({ int_ptree = literal; int_value } &@ loc)
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc OUT.none chars ~literal_class:Integer
      { int_ptree = literal; int_value = VAL.integer_zero }

(* --- *)

let fixed ({ payload = literal; loc }: Cobol_ptree.fixed with_loc)
  : fixed_literal with_loc OUT.with_diags =
  try
    let fixed_value =
      VAL.fixed_of_strings
        ~integral:literal.fixed_integral
        ~fractional:literal.fixed_fractional
    in
    OUT.result ({ fixed_ptree = literal; fixed_value } &@ loc)
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc OUT.none chars ~literal_class:Fixed
      { fixed_ptree = literal; fixed_value = VAL.fixed_zero }

let fixed_zero: fixed_literal =
  {
    fixed_ptree = Cobol_ptree.fixed_zero;
    fixed_value = VAL.fixed_of_strings ~integral:"0" ~fractional:"1";
  }

let of_fixed_value: fixed_value -> fixed_literal = fun v ->
  {
    fixed_ptree = VAL.to_ptree_fixed v;
    fixed_value = v;
  }

let categorize_fixed: fixed_literal -> [`Z of integer_literal |
                                        `Q of fixed_literal ]  = fun v ->
  if v.fixed_ptree.fixed_fractional = "0"
  then `Z { int_value = v.fixed_value.num;
            int_ptree = v.fixed_ptree.fixed_integral }
  else `Q v

(* --- *)

let floating ({ payload = literal; loc }: Cobol_ptree.floating with_loc)
  : floating_literal with_loc OUT.with_diags =
  try
    let float_value =
      VAL.floating_of_strings
        ~integral:literal.float_significand.fixed_integral
        ~fractional:literal.float_significand.fixed_fractional
        ~exponent:literal.float_exponent
    in
    OUT.result ({ float_ptree = literal; float_value } &@ loc)
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc OUT.none chars ~literal_class:Floating
      { float_ptree = literal; float_value = VAL.floating_zero }


(* --- *)

let boolean
    (* TODO deal with prefix length? *)
    ?(max_length = 8_191)                         (* as per ISO/IEC 1989:2014 *)
    Cobol_ptree.{ payload = { bool_base = base;
                              bool_value = literal_string } as bool_ptree;
                  loc } =
  let diags = OUT.none in
  let len = String.length literal_string in
  let diags =
    if len > max_length
    then error diags @@ Overlong_literal { loc; literal_string; max_length }
    else diags
  in
  try
    let v = { bool_ptree;
              bool_value = VAL.boolean_of_string ~base literal_string } in
    OUT.result ~diags (v &@ loc)
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc diags chars
      ~literal_class:(if base = `Bool then Boolean else Hexadecimal)
      { bool_ptree; bool_value = VAL.boolean_zero }

let of_boolean_value b : boolean_literal =
  {
    bool_ptree = VAL.to_ptree_boolean b;
    bool_value = b;
  }
