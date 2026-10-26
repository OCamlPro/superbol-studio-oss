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

open Cobol_data

module CHARS = Cobol_common.Basics.CharSet

let config =
  Cobol_data.Picture.TYPES.{
    max_pic_length = 38;                      (* CHECKME: 63 in ISO/IEC 2014. *)
    decimal_char = '.';
    currency_signs = CHARS.singleton '$';
  }

(* --- *)

let of_string s =
  Cobol_data.Picture.of_string config s

let pp_errors =
  Fmt.(list ~sep:(any "@;") (using fst (box Picture.pp_error)))

let same_pic p1 p2 =
  Stdlib.compare p1 p2 = 0

let parse_ok s repr =
  match of_string s with
  | Ok pic when same_pic pic repr ->
      Pretty.out "@[<2>Valid: %S:@ %a@ %a@]@." s
        Picture.pp_category_name pic.category
        Picture.pp_category pic.category
  | Ok pic ->
      Pretty.out "@[<2>Unexpected representation for %S:@ %a@ (expected %a)@]@."
        s Picture.pp pic Picture.pp repr
  | Error (errors, _) ->
      Pretty.out "@[<2>Unexpected errors for %S:@ @[<v>%a@]@]@." s
        pp_errors errors

let parse_ko descr s =
  match of_string s with
  | Ok pic ->
      Pretty.out "@[<2>Unexpectedly valid (%s): %S:@ %a@]@." descr s
        Picture.pp pic
  | Error (errors, _) ->
      Pretty.out "@[<2>%s: %S:@ @[<v>%a@]@]@." descr s pp_errors errors

let same_repr s1 s2 =
  match of_string s1, of_string s2 with
  | Ok p1, Ok p2 when same_pic p1 p2 ->
      Pretty.out "@[<2>Equiv: %S, %S@]@." s1 s2
  | Ok p1, Ok p2 ->
      Pretty.out "@[<2>Not equivalent: %S, %S:@ %a@ vs.@ %a@]@."
        s1 s2 Picture.pp p1 Picture.pp p2
  | Error (errors, _), _ | _, Error (errors, _) ->
      Pretty.out "@[<2>Unexpected errors for %S, %S:@ @[<v>%a@]@]@." s1 s2
        pp_errors errors

(* --- *)

module Pictures = struct
  open Picture.TYPES

  let symb s n = { symbol = s; symbol_occurences = n }

  let a     = symb A
  let b     = symb B
  let comma = symb GroupingSep
  let cs    = symb CS
  let dot   = symb DecimalSep
  let e     = symb E
  let nine  = symb Nine
  let p     = symb P
  let plus  = symb Plus
  let minus = symb Minus
  let s     = symb S
  let slant = symb Slant
  let star  = symb Star
  let v     = symb V
  let z     = symb Z
  let zero  = symb Zero

  let basic_alphanum s n =
    { category = Alphanumeric { length = n; insertions = [] };
      pic = [{symbol = s; symbol_occurences = n}] }
  let basic_boolean n =
    { category = Boolean { length = n };
      pic = [{symbol = One; symbol_occurences = n}] }
  let basic_national n =
    { category = National { length = n; insertions = [] };
      pic = [{symbol = N; symbol_occurences = n}] }
  let fixednum ?(sign = false) ?(basics = []) ?floating ?zerorepl
      digits scale =
    FixedNum { digits; scale; signed = sign;
               editions = { basics; floating; zerorepl } }
  let floatnum ?(signed = false) ?(basics = []) digits scale exp_digits =
    FloatNum { digits; scale; signed; exponent_digits = exp_digits;
               editions = basics; }

  (* --- *)

  let basic_numeric n =
    { category = fixednum n 0;
      pic = [{symbol = Nine; symbol_occurences = n}] }

  let fixed_numeric
      ?(pic_prefix = []) ?(pic_v = []) ?(pic_suffix = [])
      ?basics ?floating
      i d =                                          (* |int_part| |dec_part| *)
    let pic_i = if i = 0 then [] else [nine i]
    and pic_d = if d = 0 then [] else [nine d] in
    { category = fixednum ?basics ?floating (i + d) d;
      pic = pic_prefix @ pic_i @ pic_v @ pic_d @ pic_suffix }

  (* --- *)

  let pic_A =
    { category = Alphabetic { length = 1 };
      pic = [{symbol = A; symbol_occurences = 1}] }

  let pic_AAA =
    { category = Alphabetic { length = 3 };
      pic = [{symbol = A; symbol_occurences = 3}] }

  let pic_X_60_ = basic_alphanum X 60
  let pic_11111 = basic_boolean 5
  let pic_NNNNN = basic_national 5
  let pic_9 = basic_numeric 1
  let pic_999 = basic_numeric 3

  let pic_9A =
    { category = Alphanumeric { length = 2; insertions = [] };
      pic = [nine 1; a 1] }

  let pic_S99 =
    { category = fixednum ~sign:true 2 0;
      pic = [s 1; nine 2] }

  (* let pic_S99p = *)
  (*   let basics = *)
  (*     [ FixedInsertion { fixed_insertion_symbol = Plus; *)
  (*                        fixed_insertion_offset = 3 } ] *)
  (*   in *)
  (*   { category = fixednum ~sign:(Some { sign_position = Trailing; sign_separate = false }) 2 0 ~basics; *)
  (*     pic = [s 1; nine 2; plus 1] } *)

  let pic_V999 = fixed_numeric 0 3
      ~pic_v:[{ symbol = V; symbol_occurences = 1 }]

  let pic_d999 = fixed_numeric 3 0
      ~basics:[ FixedInsertion { fixed_insertion_symbol = CS;
                                 fixed_insertion_offset = 0 } ]
      ~pic_prefix:[cs 1]

  let pic_999d = fixed_numeric 3 0
      ~basics:[ FixedInsertion { fixed_insertion_symbol = CS;
                                 fixed_insertion_offset = 3 } ]
      ~pic_suffix:[cs 1]

  let pic_d99p = fixed_numeric 2 0
      ~basics:[ FixedInsertion { fixed_insertion_symbol = CS;
                                 fixed_insertion_offset = 0 };
                FixedInsertion { fixed_insertion_symbol = Plus;
                                 fixed_insertion_offset = 3 } ]
      ~pic_prefix:[cs 1]
      ~pic_suffix:[plus 1]

  let pic_p99d = fixed_numeric 2 0
      ~basics:[ FixedInsertion { fixed_insertion_symbol = Plus;
                                 fixed_insertion_offset = 0 };
                FixedInsertion { fixed_insertion_symbol = CS;
                                 fixed_insertion_offset = 3 } ]
      ~pic_prefix:[plus 1]
      ~pic_suffix:[cs 1]

  let pic_pd99 = fixed_numeric 2 0
      ~basics:[ FixedInsertion { fixed_insertion_symbol = Plus;
                                 fixed_insertion_offset = 0 };
                FixedInsertion { fixed_insertion_symbol = CS;
                                 fixed_insertion_offset = 1 } ]
      ~pic_prefix:[plus 1; cs 1]

  let pic_99dp = fixed_numeric 2 0
      ~basics:[ FixedInsertion { fixed_insertion_symbol = CS;
                                 fixed_insertion_offset = 2 };
                FixedInsertion { fixed_insertion_symbol = Plus;
                                 fixed_insertion_offset = 3 } ]
      ~pic_suffix:[cs 1; plus 1]

  let pic_dddddddddd =
    let floating =
      { floating_insertion_symbol = CS;
        floating_insertion_ranges = [{ floating_range_offset = 0;
                                       floating_range_length = 10 }] }
    in
    { category = fixednum 9 0 ~floating;
      pic = [cs 10] }

  let pic_pdd9 =
    let basics =
      [ FixedInsertion { fixed_insertion_symbol = Plus;
                         fixed_insertion_offset = 0 } ]
    and floating =
      { floating_insertion_symbol = CS;
        floating_insertion_ranges = [{ floating_range_offset = 1;
                                       floating_range_length = 2 }] }
    in
    { category = fixednum 2 0 ~basics ~floating;
      pic = [plus 1; cs 2; nine 1] }

  let pic_mdd9 =
    let basics =
      [ FixedInsertion { fixed_insertion_symbol = Minus;
                         fixed_insertion_offset = 0 } ]
    and floating =
      { floating_insertion_symbol = CS;
        floating_insertion_ranges = [{ floating_range_offset = 1;
                                       floating_range_length = 2 }] }
    in
    { category = fixednum 2 0 ~basics ~floating;
      pic = [minus 1; cs 2; nine 1] }

  let pic_dd9p =
    let basics =
      [ FixedInsertion { fixed_insertion_symbol = Plus;
                         fixed_insertion_offset = 3 } ]
    and floating =
      { floating_insertion_symbol = CS;
        floating_insertion_ranges = [{ floating_range_offset = 0;
                                       floating_range_length = 2 }] }
    in
    { category = fixednum 2 0 ~basics ~floating;
      pic = [cs 2; nine 1; plus 1] }

  let pic_ddp =
    let basics =
      [ FixedInsertion { fixed_insertion_symbol = Plus;
                         fixed_insertion_offset = 2 } ]
    and floating =
      { floating_insertion_symbol = CS;
        floating_insertion_ranges = [{ floating_range_offset = 0;
                                       floating_range_length = 2 }] }
    in
    { category = fixednum 1 0 ~basics ~floating;
      pic = [cs 2; plus 1] }

  let pic_Bdd =
    let basics =
      [ SimpleInsertion { simple_insertion_symbols = b 1;
                          simple_insertion_offset = 0 } ]
    and floating =
      { floating_insertion_symbol = CS;
        floating_insertion_ranges = [{ floating_range_offset = 1;
                                       floating_range_length = 2 }] }
    in
    { category = fixednum 1 0 ~basics ~floating;
      pic = [b 1; cs 2] }

  let pic_Bddd =
    let basics =
      [ SimpleInsertion { simple_insertion_symbols = b 1;
                          simple_insertion_offset = 0 } ]
    and floating =
      { floating_insertion_symbol = CS;
        floating_insertion_ranges = [{ floating_range_offset = 1;
                                       floating_range_length = 3 }] }
    in
    { category = fixednum 2 0 ~basics ~floating;
      pic = [b 1; cs 3] }

  let pic_Bdds9 =
    let basics =
      [ SimpleInsertion { simple_insertion_symbols = b 1;
                          simple_insertion_offset = 0 };
        SimpleInsertion { simple_insertion_symbols = slant 1;
                          simple_insertion_offset = 3 } ]
    and floating =
      { floating_insertion_symbol = CS;
        floating_insertion_ranges = [{ floating_range_offset = 1;
                                       floating_range_length = 2 }] }
    in
    { category = fixednum 2 0 ~basics ~floating;
      pic = [b 1; cs 2; slant 1; nine 1] }

  let pic_Bddsd9 =
    let basics =
      [ SimpleInsertion { simple_insertion_symbols = b 1;
                          simple_insertion_offset = 0 };
        SimpleInsertion { simple_insertion_symbols = slant 1;
                          simple_insertion_offset = 3 } ]
    and floating =
      { floating_insertion_symbol = CS;
        floating_insertion_ranges = [{ floating_range_offset = 1;
                                       floating_range_length = 2 };
                                     { floating_range_offset = 4;
                                       floating_range_length = 1 }] }
    in
    { category = fixednum 3 0 ~basics ~floating;
      pic = [b 1; cs 2; slant 1; cs 1; nine 1] }

  let pic_0000pB0ppp0Bcp =
    let basics =
      [ SimpleInsertion { simple_insertion_symbols = zero 4;
                          simple_insertion_offset = 0 };
        SimpleInsertion { simple_insertion_symbols = b 1;
                          simple_insertion_offset = 5 };
        SimpleInsertion { simple_insertion_symbols = zero 1;
                          simple_insertion_offset = 6 };
        SimpleInsertion { simple_insertion_symbols = zero 1;
                          simple_insertion_offset = 10 };
        SimpleInsertion { simple_insertion_symbols = b 1;
                          simple_insertion_offset = 11 };
        SimpleInsertion { simple_insertion_symbols = comma 1;
                          simple_insertion_offset = 12 } ]
    and floating =
      { floating_insertion_symbol = Plus;
        floating_insertion_ranges = [{ floating_range_offset = 4;
                                       floating_range_length = 1 };
                                     { floating_range_offset = 7;
                                       floating_range_length = 3 };
                                     { floating_range_offset = 13;
                                       floating_range_length = 1 }] }
    in
    { category = fixednum 4 0 ~basics ~floating;
      pic = [zero 4; plus 1; b 1; zero 1; plus 3; zero 1;
             b 1; comma 1; plus 1] }

  let pic_cc999c999v999 =
    let basics =
      [ SimpleInsertion { simple_insertion_symbols = comma 2;
                          simple_insertion_offset = 0 };
        SimpleInsertion { simple_insertion_symbols = comma 1;
                          simple_insertion_offset = 5 };
        SpecialInsertion { special_insertion_offset = 9 } ]
    in
    { category = fixednum 9 3 ~basics;
      pic = [comma 2; nine 3; comma 1; nine 3; dot 1; nine 3] }

  let pic_PPP999 =
    { category = fixednum 6 6;
      pic = [p 3; nine 3] }

  let pic_999PPP =
    { category = fixednum 6 (-3);
      pic = [nine 3; p 3] }

  let pic_SPPP9 =
    { category = fixednum 4 4 ~sign:true;
      pic = [s 1; p 3; nine 1] }

  let pic_ZZZ999V99 =
    let zerorepl = { zero_replacement_symbol = Z;
                     zero_replacement_ranges = [{ floating_range_offset = 0;
                                                  floating_range_length = 3 }] }
    in
    { category = fixednum 8 2 ~zerorepl;
      pic = [z 3; nine 3; v 1; nine 2] }

  let pic_ZZZcZZZvZZZ =
    let zerorepl = { zero_replacement_symbol = Z;
                     zero_replacement_ranges = [{ floating_range_offset = 0;
                                                  floating_range_length = 3 };
                                                { floating_range_offset = 4;
                                                  floating_range_length = 3 };
                                                { floating_range_offset = 8;
                                                  floating_range_length = 3 }] }
    and basics =
      [ SimpleInsertion { simple_insertion_symbols = comma 1;
                          simple_insertion_offset = 3 };
        SpecialInsertion { special_insertion_offset = 7 } ]
    in
    { category = fixednum 9 3 ~basics ~zerorepl;
      pic = [z 3; comma 1; z 3; dot 1; z 3] }

  let pic_ssscsssvsss =
    let zerorepl = { zero_replacement_symbol = Star;
                     zero_replacement_ranges = [{ floating_range_offset = 0;
                                                  floating_range_length = 3 };
                                                { floating_range_offset = 4;
                                                  floating_range_length = 3 };
                                                { floating_range_offset = 7;
                                                  floating_range_length = 3 }] }
    and basics =
      [ SimpleInsertion { simple_insertion_symbols = comma 1;
                          simple_insertion_offset = 3 } ]
    in
    { category = fixednum 9 3 ~basics ~zerorepl;
      pic = [star 3; comma 1; star 3; v 1; star 3] }

  let pic_msBs99 =
    let zerorepl = { zero_replacement_symbol = Star;
                     zero_replacement_ranges = [{ floating_range_offset = 1;
                                                  floating_range_length = 1 };
                                                { floating_range_offset = 3;
                                                  floating_range_length = 1 }] }
    and basics =
      [ FixedInsertion { fixed_insertion_symbol = Minus;
                         fixed_insertion_offset = 0 };
        SimpleInsertion { simple_insertion_symbols = b 1;
                          simple_insertion_offset = 2 } ]
    in
    { category = fixednum 4 0 ~basics ~zerorepl;
      pic = [minus 1; star 1; b 1; star 1; nine 2] }

  let pic_9Ep9 =
    { category = floatnum 1 0 1;
      pic = [nine 1; e 1; plus 1; nine 1] }

  let pic_999c999Ep999 =
    let basics =
      [ SimpleInsertion { simple_insertion_symbols = comma 1;
                          simple_insertion_offset = 3 } ]
    in
    { category = floatnum 6 0 3 ~basics;
      pic = [nine 3; comma 1; nine 3; e 1; plus 1; nine 3] }

  let pic_999c999v999Ep999 =
    let basics =
      [ SimpleInsertion { simple_insertion_symbols = comma 1;
                          simple_insertion_offset = 3 };
        SpecialInsertion { special_insertion_offset = 7 } ]
    in
    { category = floatnum 9 3 3 ~basics;
      pic = [nine 3; comma 1; nine 3; dot 1; nine 3; e 1; plus 1; nine 3] }

  let pic_VP9B =
    let basics =
      [ SimpleInsertion { simple_insertion_symbols = b 1;
                          simple_insertion_offset = 2 } ]
    in
    { category = fixednum 2 2 ~basics;
      pic = [v 1; p 1; nine 1; b 1] }

  let pic_B9PPP =
    let basics =
      [ SimpleInsertion { simple_insertion_symbols = b 1;
                          simple_insertion_offset = 0 } ]
    in
    { category = fixednum 4 (-3) ~basics;
      pic = [b 1; nine 1; p 3] }

  let pic_pppppPPP =
    let floating =
      { floating_insertion_symbol = Plus;
        floating_insertion_ranges = [{ floating_range_offset = 0;
                                       floating_range_length = 5 }] }
    in
    { category = fixednum 7 (-3) ~floating;
      pic = [plus 5; p 3] }

  let pic_ppvpp =
    let basics =
      [ SpecialInsertion { special_insertion_offset = 2 } ]
    and floating =
      { floating_insertion_symbol = Plus;
        floating_insertion_ranges = [{ floating_range_offset = 0;
                                       floating_range_length = 2 };
                                     { floating_range_offset = 3;
                                       floating_range_length = 2 }] }
    in
    { category = fixednum 3 2 ~basics ~floating;
      pic = [plus 2; dot 1; plus 2] }

end

(*CHECKME: In GNUCobol the message is: only up to 10 significant digits are
 * permitted wit hin parentheses.
 * I did not find such claim in the standards or on IBM documentation.*)
(*TODO: Review the precedence rules, pp. 268-269 of ANSI Standard.*)
(*TODO: Review the precedence rules, pp. 268-269 of ANSI Standard.*)


let%expect_test "valid-pictures" =
  let open Pictures in
  parse_ok "A"                       pic_A;
  parse_ok "AAA"                     pic_AAA;
  parse_ok "aaa"                     pic_AAA;
  parse_ok "aAa"                     pic_AAA;
  parse_ok "A(3)"                    pic_AAA;
  parse_ok "a(3)"                    pic_AAA;
  parse_ok "AA(2)"                   pic_AAA;
  parse_ok "A(2)A"                   pic_AAA;
  parse_ok "A(1)A(2)"                pic_AAA;
  parse_ok "A(2)A(1)"                pic_AAA;
  parse_ok "9"                       pic_9;
  parse_ok "999"                     pic_999;
  parse_ok "9(3)"                    pic_999;
  parse_ok "X(60)"                   pic_X_60_;
  parse_ok "9A"                      pic_9A;
  parse_ok "111(1)11"                pic_11111;
  parse_ok "S99"                     pic_S99;
  parse_ok "V9(3)"                   pic_V999;
  parse_ok "$(10)"                   pic_dddddddddd;
  parse_ok "$999"                    pic_d999;
  parse_ok "999$"                    pic_999d;
  parse_ok "$99+"                    pic_d99p;
  parse_ok "+99$"                    pic_p99d;
  parse_ok "+$99"                    pic_pd99;
  parse_ok "99$+"                    pic_99dp;
  parse_ok "+$$9"                    pic_pdd9;
  parse_ok "-$$9"                    pic_mdd9;
  parse_ok "$$9+"                    pic_dd9p;
  parse_ok "$$+"                     pic_ddp;
  parse_ok "B$$"                     pic_Bdd;
  parse_ok "B$$$"                    pic_Bddd;
  parse_ok "B$$/9"                   pic_Bdds9;
  parse_ok "B$$/$9"                  pic_Bddsd9;
  parse_ok "0000+B0+++0B,+"          pic_0000pB0ppp0Bcp;
  parse_ok ",,999,999.999"           pic_cc999c999v999;
  parse_ok "PPP999"                  pic_PPP999;
  parse_ok "999PPP"                  pic_999PPP;
  parse_ok "SPPP9"                   pic_SPPP9;
  parse_ok "VP9B"                    pic_VP9B;
  parse_ok "B9P(3)"                  pic_B9PPP;
  parse_ok "+(5)P(3)"                pic_pppppPPP;
  parse_ok "++.++"                   pic_ppvpp;
  parse_ok "ZZZ999V99"               pic_ZZZ999V99;
  parse_ok "ZZZ,ZZZ.ZZZ"             pic_ZZZcZZZvZZZ;
  parse_ok "***,***V***"             pic_ssscsssvsss;
  parse_ok "-*B*99"                  pic_msBs99;
  parse_ok "9E+9"                    pic_9Ep9;
  parse_ok "999,999E+999"            pic_999c999Ep999;
  parse_ok "999,999.999E+999"        pic_999c999v999Ep999;
  [%expect {|
    Valid: "A": alphabetic ALPHABETIC(1)
    Valid: "AAA": alphabetic ALPHABETIC(3)
    Valid: "aaa": alphabetic ALPHABETIC(3)
    Valid: "aAa": alphabetic ALPHABETIC(3)
    Valid: "A(3)": alphabetic ALPHABETIC(3)
    Valid: "a(3)": alphabetic ALPHABETIC(3)
    Valid: "AA(2)": alphabetic ALPHABETIC(3)
    Valid: "A(2)A": alphabetic ALPHABETIC(3)
    Valid: "A(1)A(2)": alphabetic ALPHABETIC(3)
    Valid: "A(2)A(1)": alphabetic ALPHABETIC(3)
    Valid: "9": numeric NUMERIC(digits = 1, scale = 0, signed = false)
    Valid: "999": numeric NUMERIC(digits = 3, scale = 0, signed = false)
    Valid: "9(3)": numeric NUMERIC(digits = 3, scale = 0, signed = false)
    Valid: "X(60)": alphanumeric ALPHANUMERIC(60)
    Valid: "9A": alphanumeric ALPHANUMERIC(2)
    Valid: "111(1)11": boolean BOOLEAN(5)
    Valid: "S99": numeric NUMERIC(digits = 2, scale = 0, signed = true)
    Valid: "V9(3)": numeric NUMERIC(digits = 3, scale = 3, signed = false)
    Valid: "$(10)": numeric-edited NUMERIC(digits = 9, scale = 0, signed = false)
    Valid: "$999": numeric-edited NUMERIC(digits = 3, scale = 0, signed = false)
    Valid: "999$": numeric-edited NUMERIC(digits = 3, scale = 0, signed = false)
    Valid: "$99+": numeric-edited NUMERIC(digits = 2, scale = 0, signed = false)
    Valid: "+99$": numeric-edited NUMERIC(digits = 2, scale = 0, signed = false)
    Valid: "+$99": numeric-edited NUMERIC(digits = 2, scale = 0, signed = false)
    Valid: "99$+": numeric-edited NUMERIC(digits = 2, scale = 0, signed = false)
    Valid: "+$$9": numeric-edited NUMERIC(digits = 2, scale = 0, signed = false)
    Valid: "-$$9": numeric-edited NUMERIC(digits = 2, scale = 0, signed = false)
    Valid: "$$9+": numeric-edited NUMERIC(digits = 2, scale = 0, signed = false)
    Valid: "$$+": numeric-edited NUMERIC(digits = 1, scale = 0, signed = false)
    Valid: "B$$": numeric-edited NUMERIC(digits = 1, scale = 0, signed = false)
    Valid: "B$$$": numeric-edited NUMERIC(digits = 2, scale = 0, signed = false)
    Valid: "B$$/9": numeric-edited NUMERIC(digits = 2, scale = 0, signed = false)
    Valid: "B$$/$9": numeric-edited
      NUMERIC(digits = 3, scale = 0, signed = false)
    Valid: "0000+B0+++0B,+": numeric-edited
      NUMERIC(digits = 4, scale = 0, signed = false)
    Valid: ",,999,999.999": numeric-edited
      NUMERIC(digits = 9, scale = 3, signed = false)
    Valid: "PPP999": numeric NUMERIC(digits = 6, scale = 6, signed = false)
    Valid: "999PPP": numeric NUMERIC(digits = 6, scale = -3, signed = false)
    Valid: "SPPP9": numeric NUMERIC(digits = 4, scale = 4, signed = true)
    Valid: "VP9B": numeric-edited NUMERIC(digits = 2, scale = 2, signed = false)
    Valid: "B9P(3)": numeric-edited
      NUMERIC(digits = 4, scale = -3, signed = false)
    Valid: "+(5)P(3)": numeric-edited
      NUMERIC(digits = 7, scale = -3, signed = false)
    Valid: "++.++": numeric-edited NUMERIC(digits = 3, scale = 2, signed = false)
    Valid: "ZZZ999V99": numeric-edited
      NUMERIC(digits = 8, scale = 2, signed = false)
    Valid: "ZZZ,ZZZ.ZZZ": numeric-edited
      NUMERIC(digits = 9, scale = 3, signed = false)
    Valid: "***,***V***": numeric-edited
      NUMERIC(digits = 9, scale = 3, signed = false)
    Valid: "-*B*99": numeric-edited
      NUMERIC(digits = 4, scale = 0, signed = false)
    Valid: "9E+9": numeric
      FLOAT(digits = 1, scale = 0, exponent_digits = 1, signed = false)
    Valid: "999,999E+999": numeric-edited
      FLOAT(digits = 6, scale = 0, exponent_digits = 3, signed = false)
    Valid: "999,999.999E+999": numeric-edited
      FLOAT(digits = 9, scale = 3, exponent_digits = 3, signed = false) |}]

let%expect_test "invalid-pictures" =
  parse_ko "Empty"                   "";
  parse_ko "Trailing +/- after S"    "S99+" (* pic_S99p *);                (* could be *)
  parse_ko "Fixed cs after S"        "S$9";
  parse_ko "Floating cs after S"     "S$(10)";
  parse_ko "Floating + after S"      "S+(10)";
  parse_ko "Invalid floating edit."  "+$99++";
  parse_ko "Trailing + sign"         "+$99+";
  parse_ko "Non-trailing +"          "$$+9";
  parse_ko "Duplicated CR"           "9CRCR";
  parse_ko "Duplicated DB"           "9DBDB";
  parse_ko "Duplicated S"            "SS9S";
  parse_ko "Duplicated ."            "99..9";
  parse_ko "Dulpicated V"            "99VV9";
  parse_ko "Duplicated E"            "9EE+9";
  parse_ko "Missing + after E"       "9E9";
  parse_ko "Missing + after E"       "9E";
  parse_ko "Missing digits after E"  "9E+";
  parse_ko "Too long"                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
                                      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
                                      XXX";
  parse_ko "Even longer"             "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
                                      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
                                      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
                                      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
                                      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
                                      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
                                      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
                                      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
                                      XXXXXX";
  parse_ko "Invalid char"            "9K";
  parse_ko "Invalid char"            "999C";
  parse_ko "Invalid char"            "999D";
  parse_ko "Invalid char"            "999_";
  parse_ko "Invalid char"            "9@99";
  parse_ko "Too many digits"         "9(50)";
  parse_ko "Too long symbol occ."    "9(11111111111111)";
  parse_ko "Invalid parens"          "(";
  parse_ko "Invalid parens"          ")";
  parse_ko "Invalid parens"          "(((";
  parse_ko "Invalid parens"          ")(";
  parse_ko "Invalid symbol occ."     "x(0)";
  parse_ko "Invalid symbol occ."     "x(-1)";
  parse_ko "Nested parens"           "9((100))";
  parse_ko "Multiple parens"         "9(5)(3)";
  parse_ko "Empty parens"            "9()";
  parse_ko "Exclusive P & ."         "P(3)9.9";
  parse_ko "Exclusive V & ."         "9V.9";
  parse_ko "Exclusive Z & *"         "Z*";
  parse_ko "Exclusive + & -"         "+(5)--";
  parse_ko "Exclusive Z & cs"        "$(4)Z(9)";
  parse_ko "Exclusive B & *"         "$$B*(4)";
  parse_ko "Exclusive N & X"         "NX";
  parse_ko "Exclusive A & N"         "AN";
  parse_ko "Exclusive A & Z"         "AZ(3)";
  parse_ko "Exclusive . & X"         "99.99XXXXX";
  parse_ko "Exclusive S & A"         "SA";
  parse_ko "Exclusive S & A"         "SA";
  parse_ko "Exclusive cs & +/-"      "$$$B+++B---";
  parse_ko "Exclusive + & trailing +""+++9(5)+";
  parse_ko "Exclusive + & CR"        "+9(5)CR";
  parse_ko "Exclusive - & DB"        "-9(5)DB";
  parse_ko "Non-leading/trailing +"  "BBB+BBB99";
  parse_ko "Non-leading B"           "99-B";
  parse_ko "Non-trailing CR"         "9CRB";
  parse_ko "Non-trailing DB"         "DB9(5)";
  parse_ko "Non-leading cs"          "99$$$";
  parse_ko "Non-leading cs"          "99$B";
  parse_ko "Non-leading cs"          "99$BB";
  parse_ko "Non-leading cs"          "0$99";
  parse_ko "Missplaced cs/+"         "$+99";
  parse_ko "Invalid P strings"       "PPPVP9";
  parse_ko "No data"                 "B(5)";
  parse_ko "No data"                 "B$";
  parse_ko "No data"                 "+";
  parse_ko "No data"                 "$";
  [%expect {|
    Empty: "": Empty PICTURE string
    Trailing +/- after S: "S99+": a trailing +/- sign may not follow S
    Fixed cs after S: "S$9":
      a leading currency symbol may not follow S
      PICTURE string describes an empty data item
      Unexpected currency symbol in PICTURE string of category
      NUMERIC(digits = 0, scale = 0, signed = true)
    Floating cs after S: "S$(10)":
      a leading currency symbol may not follow S
      PICTURE string describes an empty data item
      Unexpected currency symbol in PICTURE string of category
      NUMERIC(digits = 0, scale = 0, signed = true)
    Floating + after S: "S+(10)":
      a leading +/- sign may not follow S
      PICTURE string describes an empty data item
      Unexpected + in PICTURE string of category
      NUMERIC(digits = 0, scale = 0, signed = true)
    Invalid floating edit.: "+$99++":
      a leading +/- sign may only appear once in a PICTURE string
      Unexpected + in PICTURE string of category
      NUMERIC-EDITED(digits = 2, scale = 0, signed = false,
                     editions = { Data_picture.TYPES.basics =
                                  [(Data_picture.TYPES.FixedInsertion
                                      { Data_picture.TYPES.fixed_insertion_symbol =
                                        +; fixed_insertion_offset = 0 });
                                    (Data_picture.TYPES.FixedInsertion
                                       { Data_picture.TYPES.fixed_insertion_symbol =
                                         currency symbol;
                                         fixed_insertion_offset = 1 })
                                    ];
                                  floating = None; zerorepl = None })
    Trailing + sign: "+$99+":
      a trailing +/- sign may not follow a leading +/- sign
    Non-trailing +: "$$+9":
      a leading +/- sign may not follow
      a floating currency symbol string which is before the decimal point
    Duplicated CR: "9CRCR":
      CR may only occur once in a PICTURE string
      CR must be at end of PICTURE string
    Duplicated DB: "9DBDB":
      DB may only occur once in a PICTURE string
      DB must be at end of PICTURE string
    Duplicated S: "SS9S":
      S must be at start of PICTURE string
      S may only occur once in a PICTURE string
    Duplicated .: "99..9": decimal point may only occur once in a PICTURE string
    Dulpicated V: "99VV9": V may only occur once in a PICTURE string
    Duplicated E: "9EE+9":
      a leading +/- sign may not follow 9
      E may only occur once in a PICTURE string
    Missing + after E: "9E9": Missing + symbol in exponent
    Missing + after E: "9E":
      Missing digits in exponent
      Missing + symbol in exponent
    Missing digits after E: "9E+": Missing digits in exponent
    Too long: "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX":
      length of PICTURE string exeeds allowed size (max length: 38, given
      length:77)
    Even longer: "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX":
      length of PICTURE string exeeds allowed size (max length: 38, given
      length:302)
    Invalid char: "9K": Unexpected character K in PICTURE string
    Invalid char: "999C": Unexpected character C in PICTURE string
    Invalid char: "999D": Unexpected character D in PICTURE string
    Invalid char: "999_": Unexpected character _ in PICTURE string
    Invalid char: "9@99": Unexpected character @ in PICTURE string
    Too many digits: "9(50)":
      Numeric item cannot be longer than 38 digits, 50 found
    Too long symbol occ.: "9(11111111111111)":
      Numeric item cannot be longer than 38 digits, 11111111111111 found
    Invalid parens: "(": Unexpected character ( in PICTURE string
    Invalid parens: ")": Unexpected character ) in PICTURE string
    Invalid parens: "(((": Unexpected character ( in PICTURE string
    Invalid parens: ")(": Unexpected character ) in PICTURE string
    Invalid symbol occ.: "x(0)": PICTURE string describes an empty data item
    Invalid symbol occ.: "x(-1)": Unexpected character ( in PICTURE string
    Nested parens: "9((100))": Unexpected character ( in PICTURE string
    Multiple parens: "9(5)(3)": Unexpected character ( in PICTURE string
    Empty parens: "9()": Unexpected character ( in PICTURE string
    Exclusive P & .: "P(3)9.9":
      decimal point and P are mutually exclusive in a PICTURE string
    Exclusive V & .: "9V.9":
      decimal point and V are mutually exclusive in a PICTURE string
    Exclusive Z & *: "Z*": * and Z are mutually exclusive in a PICTURE string
    Exclusive + & -: "+(5)--":
      a leading +/- sign may not follow
      a floating +/- string which is before the decimal point
      Unexpected - in PICTURE string of category
      NUMERIC-EDITED(digits = 4, scale = 0, signed = false,
                     editions = { Data_picture.TYPES.basics = [];
                                  floating =
                                  (Some { Data_picture.TYPES.floating_insertion_symbol =
                                          +;
                                          floating_insertion_ranges =
                                          [{ Data_picture.TYPES.floating_range_offset =
                                             0; floating_range_length = 5 }
                                            ]
                                          });
                                  zerorepl = None })
    Exclusive Z & cs: "$(4)Z(9)":
      a Z or * which is before the decimal point may not follow
      a floating currency symbol string which is before the decimal point
    Exclusive B & *: "$$B*(4)":
      a Z or * which is before the decimal point may not follow
      a floating currency symbol string which is before the decimal point
    Exclusive N & X: "NX":
      A or X may not follow N
      Unexpected X in PICTURE string of category NATIONAL(1)
    Exclusive A & N: "AN":
      N may not follow A or X
      Unexpected N in PICTURE string of category ALPHABETIC(1)
    Exclusive A & Z: "AZ(3)":
      a Z or * which is before the decimal point may not follow A or X
      Unexpected Z in PICTURE string of category ALPHABETIC(1)
    Exclusive . & X: "99.99XXXXX":
      A or X may not follow decimal point ('.')
      Unexpected X in PICTURE string of category
      NUMERIC-EDITED(digits = 4, scale = 2, signed = false,
                     editions = { Data_picture.TYPES.basics =
                                  [(Data_picture.TYPES.SpecialInsertion
                                      { Data_picture.TYPES.special_insertion_offset =
                                        2 })
                                    ];
                                  floating = None; zerorepl = None })
    Exclusive S & A: "SA":
      A or X may not follow S
      PICTURE string describes an empty data item
      Unexpected A in PICTURE string of category
      NUMERIC(digits = 0, scale = 0, signed = true)
    Exclusive S & A: "SA":
      A or X may not follow S
      PICTURE string describes an empty data item
      Unexpected A in PICTURE string of category
      NUMERIC(digits = 0, scale = 0, signed = true)
    Exclusive cs & +/-: "$$$B+++B---":
      a leading +/- sign may not follow B, 0 or /
      Unexpected + in PICTURE string of category
      NUMERIC-EDITED(digits = 2, scale = 0, signed = false,
                     editions = { Data_picture.TYPES.basics =
                                  [(Data_picture.TYPES.SimpleInsertion
                                      { Data_picture.TYPES.simple_insertion_symbols =
                                        { Data_picture.TYPES.symbol = B;
                                          symbol_occurences = 1 };
                                        simple_insertion_offset = 3 })
                                    ];
                                  floating =
                                  (Some { Data_picture.TYPES.floating_insertion_symbol =
                                          currency symbol;
                                          floating_insertion_ranges =
                                          [{ Data_picture.TYPES.floating_range_offset =
                                             0; floating_range_length = 3 }
                                            ]
                                          });
                                  zerorepl = None })
    Exclusive + & trailing +: "+++9(5)+":
      a floating +/- string which is before the decimal point may not follow 9
    Exclusive + & CR: "+9(5)CR": CR or DB may not follow a leading +/- sign
    Exclusive - & DB: "-9(5)DB": CR or DB may not follow a leading +/- sign
    Non-leading/trailing +: "BBB+BBB99":
      a leading +/- sign may not follow B, 0 or /
    Non-leading B: "99-B": a leading +/- sign may not follow 9
    Non-trailing CR: "9CRB": CR must be at end of PICTURE string
    Non-trailing DB: "DB9(5)": DB must be at end of PICTURE string
    Non-leading cs: "99$$$":
      a floating currency symbol string which is before the decimal point may not
      follow 9
    Non-leading cs: "99$B": B, 0 or / may not follow a trailing currency symbol
    Non-leading cs: "99$BB": B, 0 or / may not follow a trailing currency symbol
    Non-leading cs: "0$99": a leading currency symbol may not follow B, 0 or /
    Missplaced cs/+: "$+99":
      a leading +/- sign may not follow a leading currency symbol
    Invalid P strings: "PPPVP9":
      V may not follow a P which is after the decimal point
      Unexpected V in PICTURE string of category
      NUMERIC(digits = 3, scale = 3, signed = false)
    No data: "B(5)": PICTURE string describes an empty data item
    No data: "B$":
      a leading currency symbol may not follow B, 0 or /
      PICTURE string describes an empty data item
    No data: "+": PICTURE string describes an empty data item
    No data: "$": PICTURE string describes an empty data item |}]

let%expect_test "equivalent-pictures" =
  same_repr "9(3)"                   "999";
  same_repr "X(2)9(3)X(1)"           "XX999X";
  same_repr "Z(9).99-"               "ZZZZZZZZZ.99-";
  same_repr "Z(9).99"                "ZZZZZZZZZ.99";
  same_repr "Z(9)V99"                "ZZZZZZZZZV99";
  same_repr "*(6).99"                "******.99";
  same_repr "+Z(5).99"               "+ZZZZZ.99";
  [%expect {|
    Equiv: "9(3)", "999"
    Equiv: "X(2)9(3)X(1)", "XX999X"
    Equiv: "Z(9).99-", "ZZZZZZZZZ.99-"
    Equiv: "Z(9).99", "ZZZZZZZZZ.99"
    Equiv: "Z(9)V99", "ZZZZZZZZZV99"
    Equiv: "*(6).99", "******.99"
    Equiv: "+Z(5).99", "+ZZZZZ.99" |}]
