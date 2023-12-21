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
open Cobol_common.Srcloc.INFIX

module CHARS = Cobol_common.Basics.CharSet
module DIAGS = Cobol_common.Diagnostics

module Config = struct
  include Cobol_config.Default
  let pic_length =
    (* CHECKME: 63 in ISO/IEC 2014. *)
    Cobol_config.Options.pic_length#from_val ~config 38
end

module Env = struct
  let decimal_char = '.'
  let currency_signs = CHARS.singleton '$'
end

module PIC = Cobol_data.Picture.Make (Config) (Env)

(* --- *)

let dummy_loc = Cobol_common.Srcloc.raw Lexing.(dummy_pos, dummy_pos)

let picture =
  Alcotest.testable Cobol_data.Picture.pp (fun p1 p2 -> Stdlib.compare p1 p2 = 0)

let of_string s =
  try ~&(PIC.of_string (s &@ dummy_loc))
  with PIC.InvalidPicture (_, diags, pic) ->
    Pretty.failwith "%a" DIAGS.Set.pp diags Picture.pp pic

let check_ok s repr =
  Alcotest.check picture "Picture parsed" repr (of_string s)

let check_eq s1 s2 =
  Alcotest.check picture "Equivalent pictures" (of_string s1) (of_string s2)

let check_ko ?(show = false) s =
  Alcotest.check_raises "Invalid picture" Exit
    (fun () -> try ignore @@ of_string s with _ when not show -> raise Exit)

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
  let fixednum ?(with_sign = false) ?(basics = []) ?floating ?zerorepl
      digits scale =
    FixedNum { digits; scale; with_sign;
               editions = { basics; floating; zerorepl } }
  let floatnum ?(with_sign = false) ?(basics = []) digits scale exp_digits =
    FloatNum { digits; scale; with_sign; exponent_digits = exp_digits;
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
    { category = fixednum ~with_sign:true 2 0;
      pic = [s 1; nine 2] }

  (* let pic_S99p = *)
  (*   let basics = *)
  (*     [ FixedInsertion { fixed_insertion_symbol = Plus; *)
  (*                        fixed_insertion_offset = 3 } ] *)
  (*   in *)
  (*   { category = fixednum ~with_sign:true 2 0 ~basics; *)
  (*     pic = [s 1; nine 2; plus 1] } *)

  let pic_V999 = fixed_numeric 0 3
      ~pic_v:[{ symbol = V; symbol_occurences = 1 }]

  let pic_d999 = fixed_numeric 3 0
      ~basics:[ FixedInsertion { fixed_insertion_symbol = CS;
                                 fixed_insertion_offset = 0 } ]
      ~pic_prefix:[cs 1]

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
        SpecialInsertion { special_insertion_offset = 9;
                           special_insertion_length = 1 } ]
    in
    { category = fixednum 9 3 ~basics;
      pic = [comma 2; nine 3; comma 1; nine 3; dot 1; nine 3] }

  let pic_PPP999 =
    { category = fixednum 6 6;
      pic = [p 3; nine 3] }

  let pic_999PPP =
    { category = fixednum 6 (-3);
      pic = [nine 3; p 3] }

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
        SpecialInsertion { special_insertion_offset = 7;
                           special_insertion_length = 1 } ]
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
        SpecialInsertion { special_insertion_offset = 7;
                           special_insertion_length = 1 } ]
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
      [ SpecialInsertion { special_insertion_offset = 2;
                           special_insertion_length = 1 } ]
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

let () =
  let parse_ok pic_string pic_repr =
    Alcotest.test_case ("Valid: " ^ pic_string) `Quick @@
    fun () -> check_ok pic_string pic_repr
  and parse_ko ?show descr pic_string =
    Alcotest.test_case (descr ^ ": " ^ pic_string) `Quick @@
    fun () -> check_ko ?show pic_string
  and same_repr pic_string_1 pic_string_2 =
    Alcotest.test_case ("Equiv: " ^ pic_string_1 ^ ", " ^ pic_string_2) `Quick @@
    fun () -> check_eq pic_string_1 pic_string_2
  in
  let open Pictures in
  Alcotest.(run "picture parsing" [
    "PIC.of_string", [
      parse_ko "Empty"                   "";
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
      parse_ko "Trailing +/- after S"    "S99+" (* pic_S99p *);                (* could be *)
      parse_ok "V9(3)"                   pic_V999;
      parse_ok "$(10)"                   pic_dddddddddd;
      parse_ko "Fixed cs after S"        "S$9";
      parse_ko "Floating cs after S"     "S$(10)";
      parse_ko "Floating + after S"      "S+(10)";
      parse_ok "$999"                    pic_d999;
      parse_ok "$99+"                    pic_d99p;
      parse_ok "+99$"                    pic_p99d;
      parse_ok "+$99"                    pic_pd99;
      parse_ok "99$+"                    pic_99dp;
      parse_ok "+$$9"                    pic_pdd9;
      parse_ok "-$$9"                    pic_mdd9;
      parse_ok "$$9+"                    pic_dd9p;
      parse_ko "Invalid floating edit."  "+$99++";
      parse_ko "Trailing + sign"         "+$99+";
      parse_ko "Non-trailing +"          "$$+9";
      parse_ok "$$+"                     pic_ddp;
      parse_ok "B$$"                     pic_Bdd;
      parse_ok "B$$$"                    pic_Bddd;
      parse_ok "B$$/9"                   pic_Bdds9;
      parse_ok "B$$/$9"                  pic_Bddsd9;
      parse_ok "0000+B0+++0B,+"          pic_0000pB0ppp0Bcp;
      parse_ko "Duplicated CR"           "9CRCR";
      parse_ko "Duplicated DB"           "9DBDB";
      parse_ko "Duplicated S"            "SS9S";
      parse_ko "Duplicated ."            "99..9";
      parse_ko "Dulpicated V"            "99VV9";
      parse_ok ",,999,999.999"           pic_cc999c999v999;
      parse_ok "PPP999"                  pic_PPP999;
      parse_ok "999PPP"                  pic_999PPP;
      parse_ok "VP9B"                    pic_VP9B;
      parse_ok "B9P(3)"                  pic_B9PPP;
      parse_ok "+(5)P(3)"                pic_pppppPPP;
      parse_ok "++.++"                   pic_ppvpp;
      parse_ok "ZZZ999V99"               pic_ZZZ999V99;
      parse_ok "ZZZ,ZZZ.ZZZ"             pic_ZZZcZZZvZZZ;
      parse_ok "***,***V***"             pic_ssscsssvsss;
      parse_ok "-*B*99"                  pic_msBs99;
      parse_ok "9E+9"                    pic_9Ep9;
      parse_ko "Duplicated E"            "9EE+9";
      parse_ko "Missing + after E"       "9E9";
      parse_ko "Missing + after E"       "9E";
      parse_ko "Missing digits after E"  "9E+";
      parse_ok "999,999E+999"            pic_999c999Ep999;
      parse_ok "999,999.999E+999"        pic_999c999v999Ep999;
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
      parse_ko "Invalid parens"          "(";
      parse_ko "Invalid parens"          ")";
      parse_ko "Invalid parens"          "(((";
      parse_ko "Invalid parens"          ")(";
      parse_ko "Invalid symbol occ."     "x(0)";
      parse_ko "Invalid symbol occ."     "x(-1)";
      (* parse_ko "Too long symbol occ."         "9(11111111111111)"; *)
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
      same_repr "9(3)"                   "999";
      same_repr "X(2)9(3)X(1)"           "XX999X";
    ]
  ])
