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

type integer = string                                           [@@deriving ord]

type fixed =
  {
    fixed_integral: string;                               (** Integer part *)
    fixed_fractional: string;                             (** Fractional part *)
  }                                                             [@@deriving ord]

type floating =
  {
    float_significand: fixed;
    float_exponent: string;                 (* 0 <= . <= 9999 in ISO/IEC 2014 *)
  }                                                             [@@deriving ord]

type boolean =
  {
    bool_base: [`Bool | `Hex];
    bool_value: string;
  }                                                             [@@deriving ord]

(* --- *)

let pp_integer = Pretty.string

let fixed_of_strings i d =
  {
    fixed_integral = i;
    fixed_fractional = d;
  }

let pp_fixed ppf { fixed_integral; fixed_fractional } =
  Pretty.print ppf "%s.%s" fixed_integral fixed_fractional

let floating_of_strings i d e =
  {
    float_significand = fixed_of_strings i d;
    float_exponent = e;
  }

let pp_floating ppf { float_significand = s; float_exponent = e } =
  Pretty.print ppf "%aE%s" pp_fixed s e

(* --- *)

let boolean_of_string ?(base: [`Bool | `Hex] = `Bool) s =
  {
    bool_base = base;
    bool_value = s;
  }
