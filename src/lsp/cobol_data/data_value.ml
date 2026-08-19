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

(** Representation of basic COBOL values.

    `*_of_strings?` functions may raise {!INVALID_CHARS}. *)

open Data_types

module NEL = Cobol_common.Basics.NEL

exception INVALID_CHARS of (int * char) NEL.t

(* Accumulates a non-empty list of characters, associated with their index in a
   string [S], and raises {!INVALID_CHARS}.

   The string [S] is given in decomposed form: [specs] is a non-empty list of
   tuples [(s, si, f)], that each associates a sub-strings [s] of [S], the index
   [si] of its first character in [S], and a predicate [f] that indicates
   whether the character [c] of [s] is invalid.

   Important: make sure that at leat one of [f c] holds, for [c] a character of
   [s] in a triple [(s, _, f)] given in [specs]. *)
let invalid_chars specs =
  let chars =
    NEL.of_rev_list @@ List.fold_left begin fun chars (s, si, f) ->
      snd @@ String.fold_left begin fun (i, chars) c ->
        succ i, if f c then (si + i, c) :: chars else chars
      end (0, chars) s
    end [] specs
  in
  raise @@ INVALID_CHARS chars

let non_digit = function
  | '0' .. '9' -> false
  | _ -> true

let non_bool_bit ~base = function
  | '0' | '1' -> false
  | '2' .. '9' | 'a' .. 'f' | 'A' .. 'F' when base = `Hex -> false
  | _ -> true

(* --- *)

let integer_zero = Z.zero
let pp_integer = Z.pp_print
let integer_of_string s =
  try Z.of_string_base 10 s
  with Invalid_argument _ ->
    invalid_chars [s, 0, non_digit]

(* --- *)

let fixed_zero = Q.zero
let pp_fixed = Q.pp_print
let fixed_of_string = Q.of_string
let fixed_of_strings ~integral ~fractional =
  try Printf.ksprintf Q.of_string "%s.%s" integral fractional
  with Invalid_argument _ ->
    invalid_chars [integral,   0,                          non_digit;
                   fractional, String.length integral + 1, non_digit]
let fixed_to_string = Q.to_string

(* TODO: check what's the max fractional size allowed (may depend on
   dialect)... *)
let to_ptree_fixed ?(max_fractional_size = 18) q : Cobol_ptree.fixed =
  Num_utils.fixed_decimal_of_rational ~max_fractional_size q

let pp_fixed_as_decimal ppf q =
  Cobol_ptree.pp_fixed ppf (to_ptree_fixed q)

(* --- *)

let floating_zero =
  {
    float_significand = Q.zero;
    float_exponent = 1;
  }

let pp_floating ppf { float_significand; float_exponent } =
  Pretty.print ppf "%aE%d" pp_fixed_as_decimal float_significand float_exponent

let floating_of_strings ~integral ~fractional ~exponent =
  try
    { float_significand = Q.of_string (integral ^ "." ^ fractional);
      float_exponent = int_of_string exponent }
  with Invalid_argument _ ->
    let ilen = String.length integral and flen = String.length fractional in
    invalid_chars [integral,   0,               non_digit;
                   fractional, ilen + 1,        non_digit;
                   exponent,   ilen + flen + 2, non_digit]

(* --- *)

let alphanum_of_string = Cobol_ptree.alphanum_of_string

(* --- *)

let boolean_zero =
  {
    bool_width = 1;
    bool_bits = integer_zero;
  }

let boolean_of_string ?(base: [`Bool | `Hex] = `Bool) literal : boolean_value =
  match literal with
  | "" ->
      { bool_width = 0; bool_bits = Z.zero }
  | s ->
      let bool_width = String.length s * if base = `Bool then 1 else 4 in
      try
        let bool_bits = Z.of_string_base (if base = `Bool then 2 else 16) s in
        { bool_width; bool_bits }
      with Invalid_argument _ ->
        invalid_chars [s, 0, non_bool_bit ~base]

let boolean_to_string { bool_width; bool_bits } : string =
  Z.format (Printf.sprintf "%%0%db" bool_width) bool_bits

let pp_boolean ppf b =
  Pretty.print ppf "b\"%s\"" (boolean_to_string b)

let to_ptree_boolean b : Cobol_ptree.boolean =
  Cobol_ptree.{ bool_base = `Bool; bool_value = boolean_to_string b }
