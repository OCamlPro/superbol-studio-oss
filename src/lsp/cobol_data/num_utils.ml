(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

let integral_string_of_rational (Q.{ num; den }) : string =
  if Z.equal den Z.zero then
    Fmt.invalid_arg "%s: denominator is zero" __FUNCTION__;

  Z.to_string @@ Z.div num den

let fixed_decimal_of_rational ~max_fractional_size
    (Q.{ num; den } as q) : Cobol_ptree.fixed =
  if Z.equal den Z.zero then
    Fmt.invalid_arg "%s: denominator is zero" __FUNCTION__;

  let negative = Q.sign q < 0 in
  let num = Z.abs num in
  let den = Z.abs den in

  let q, r = Z.div_rem num den in

  let fixed_integral =
    if negative
    then "-" ^ Z.to_string q
    else Z.to_string q
  in

  let fixed_fractional =
    if Z.equal r Z.zero then "0" else begin
      let buf = Buffer.create max_fractional_size in
      let r = ref r in
      while not (Z.equal !r Z.zero) && Buffer.length buf < max_fractional_size do
        let digit, r' = Z.(div_rem (!r * ~$10)) den in
        Buffer.add_char buf (Char.chr ((*'0'*)48 + Z.to_int digit));
        r := r'
      done;
      if Buffer.length buf > max_fractional_size
      then Buffer.truncate buf max_fractional_size;
      Buffer.contents buf
    end
  in

  Cobol_ptree.{ fixed_integral; fixed_fractional }
