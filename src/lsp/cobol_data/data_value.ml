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

type integer = Z.t
let integer_zero = Z.zero
let pp_integer = Z.pp_print
let integer_of_string s =
  try Z.of_string_base 10 s
  with Invalid_argument _ ->
    invalid_chars [s, 0, non_digit]

(* --- *)

type fixed = Q.t

let fixed_zero = Q.zero
let pp_fixed = Q.pp_print
let fixed_of_string = Q.of_string
let fixed_of_strings ~integral ~fractional =
  try Q.of_string (integral ^ "." ^ fractional)
  with Invalid_argument _ ->
    invalid_chars [integral,   0,                          non_digit;
                   fractional, String.length integral + 1, non_digit]


(* --- *)

type floating =
  {
    float_significand: fixed;
    float_exponent: int;                    (* 0 <= . <= 9999 in ISO/IEC 2014 *)
  }

let floating_zero =
  {
    float_significand = Q.zero;
    float_exponent = 1;
  }

let pp_floating ppf { float_significand; float_exponent } =
  Pretty.print ppf "%aE%d" pp_fixed float_significand float_exponent

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

type alphanum =
  string
[@@deriving show]

(* --- *)

type boolean =
  {
    bool_width: int;                                (** may be 0 *)
    bool_value: Z.t; [@printer Z.pp_print]          (** irrelevant if 0-width *)
  }
[@@deriving show]

let boolean_zero =
  {
    bool_width = 1;
    bool_value = integer_zero;
  }

let boolean_of_string ?(base: [`Bool | `Hex] = `Bool) literal =
  match literal with
  | "" ->
      { bool_width = 0; bool_value = Z.zero }
  | s ->
      let bool_width = String.length s * if base = `Bool then 1 else 4 in
      try
        let bool_value = Z.of_string_base (if base = `Bool then 2 else 16) s in
        { bool_width; bool_value }
      with Invalid_argument _ ->
        invalid_chars [s, 0, non_bool_bit ~base]
