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

(** Representation of basic COBOL values. *)

open Data_types

module NEL = Cobol_common.Basics.NEL

(* --- *)

type error = Invalid_chars of (int * char) NEL.t                     [@@unboxed]

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
  Error (Invalid_chars chars)

let non_digit = function
  | '0' .. '9' -> false
  | _ -> true

let non_bool_bit ~base = function
  | '0' | '1' -> false
  | '2' .. '9' | 'a' .. 'f' | 'A' .. 'F' when base = `Hex -> false
  | _ -> true

(* --- *)

(** Assumes a leading zero in case of odd length. *)
(* CHECKME: this behavior may depend on compiler/dialect. *)
let of_hex hex =
  let digit c =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
    | _ -> invalid_arg "Invalid hexadecimal digit"
  in
  let len = String.length hex in
  let len, hex = if len mod 2 <> 0 then len + 1, "0"^hex else len, hex in
  String.init (len / 2) begin fun i ->
    let hi = digit hex.[2 * i] in
    let lo = digit hex.[2 * i + 1] in
    Char.chr ((hi lsl 4) lor lo)
  end

let plain_alphanum: string -> alphanum_value =
  Fun.id

let alphanum_of_string ?(hexadecimal = false) s : (alphanum_value, error) result =
  if not hexadecimal then Ok s else
    try Ok (of_hex s)
    with Invalid_argument _ ->
      invalid_chars [s, 0, non_bool_bit ~base:`Hex]

let alphanum (a: Cobol_ptree.alphanum) : (alphanum_value, error) result =
  alphanum_of_string ~hexadecimal:a.hexadecimal a.str

let concat_alphanums a b =
  a ^ b

let compare_alphanums =
  String.compare

let ptree_of_alphanum (a: alphanum_value) : Cobol_ptree.alphanum =
  Cobol_ptree.alphanum_of_string a

(* --- *)

let integer_zero = Z.zero
let integer_of_string s =
  try Ok (Z.of_string_base 10 s)
  with Invalid_argument _ ->
    invalid_chars [s, 0, non_digit]
let integer (i: Cobol_ptree.integer) : (integer_value, error) result =
  integer_of_string i
let string_of_integer =
  Z.to_string

(* --- *)

let fixed_zero = Q.zero
let fixed_of_string = Q.of_string
let fixed_of_strings ~integral ~fractional =
  try Ok (Printf.ksprintf Q.of_string "%s.%s" integral fractional)
  with Invalid_argument _ ->
    invalid_chars [integral,   0,                          non_digit;
                   fractional, String.length integral + 1, non_digit]
let fixed (f: Cobol_ptree.fixed) : (fixed_value, error) result =
  fixed_of_strings ~integral:f.fixed_integral ~fractional:f.fixed_fractional
let string_of_fixed = Q.to_string

(* TODO: check what's the max fractional size allowed (may depend on
   dialect)... *)
let ptree_of_fixed ?(max_fractional_size = 18) q : Cobol_ptree.fixed =
  Num_utils.fixed_decimal_of_rational ~max_fractional_size q

let categorize_fixed: fixed_value -> [`Z of integer_value |
                                      `Q of fixed_value ] = fun f ->
  if Q.den f = Z.one
  then `Z (Q.num f)
  else `Q f

(* --- *)

let floating_zero: floating_value =
  {
    float_significand = Q.zero;
    float_exponent = 1;
  }

let floating_of_strings ~integral ~fractional ~exponent =
  try
    Ok ({ float_significand = Q.of_string (integral ^ "." ^ fractional);
          float_exponent = int_of_string exponent })
  with Invalid_argument _ ->
    let ilen = String.length integral and flen = String.length fractional in
    invalid_chars [integral,   0,               non_digit;
                   fractional, ilen + 1,        non_digit;
                   exponent,   ilen + flen + 2, non_digit]

let floating (f: Cobol_ptree.floating) : (floating_value, error) result =
  floating_of_strings
    ~integral:f.float_significand.fixed_integral
    ~fractional:f.float_significand.fixed_fractional
    ~exponent:f.float_exponent

let ptree_of_floating (f: floating_value) : Cobol_ptree.floating =
  {
    float_significand = ptree_of_fixed f.float_significand;
    float_exponent = string_of_int f.float_exponent;
  }

(* --- *)

let boolean_zero: boolean_value =
  {
    bool_width = 1;
    bool_bits = integer_zero;
  }

let boolean_of_string ?(base: [`Bool | `Hex] = `Bool) literal =
  match literal with
  | "" ->
      Ok { bool_width = 0; bool_bits = Z.zero }
  | s ->
      try
        let bool_width = String.length s * if base = `Bool then 1 else 4 in
        Ok ({ bool_width;
              bool_bits = Z.of_string_base (if base = `Bool then 2 else 16) s })
      with Invalid_argument _ ->
        invalid_chars [s, 0, non_bool_bit ~base]

let boolean (b: Cobol_ptree.boolean) : (boolean_value, error) result =
  boolean_of_string ~base:b.bool_base b.bool_string

let boolean_to_string { bool_width; bool_bits } : string =
  if bool_width = 0
  then ""
  else Z.format (Printf.sprintf "%%0%db" bool_width) bool_bits

let ptree_of_boolean b : Cobol_ptree.boolean =
  Cobol_ptree.{ bool_base = `Bool; bool_string = boolean_to_string b }

(* --- *)
