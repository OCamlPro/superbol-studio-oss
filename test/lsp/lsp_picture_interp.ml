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


open Lsp_testing
module CHARS = Cobol_common.Basics.CharSet

let unit_tests =
  [ ("99,B999,B000", 1234., "01, 234, 000");
    ("99,999", 12345., "12,345");
    ("999.99", 1.234, "001.23");
    ("999.99", 12.34, "012.34");
    ("999.99", 123.45, "123.45");
    ("999.99", 1234.5, "234.50");
    (* ("+999.99E+99", 12345., "+123.45E+02"); *)
    ("999.99+", +6555.556, "555.55+");
    ("+9999.99", -6555.555, "-6555.55");
    ("9999.99", +1234.56, "1234.56");
    ("$999.99", -123.45, "$123.45");
    ("-$999.99", -123.456, "-$123.45");
    ("-$999.99", +123.456, " $123.45");
    ("$9999.99CR", +123.45, "$0123.45  ");
    ("$9999.99DB", -123.45, "$0123.45DB");
    (* ("U999.99", -123.45, "EUR123.45"); *)
    (* ("-u999.99", -123.456, "-USD123.45"); *)
    ("$$$$.99", 0.123, "   $.12");
    ("$$$9.99", 0.12, "  $0.12");
    ("$,$$$,999.99", -1234.56, "   $1,234.56");
    (* ("U,UUU,UU9.99-", -1234.56, "EUR1,234.56-"); *)
    (* ("u,uuu,uu9.99", 1234.56, "USD1,234.56"); *)
    ("+,+++,999.99", -123456.789, " -123,456.78");
    ("$$,$$$,$$$.99CR", -1234567., "$1,234,567.00CR");
    ("++,+++,+++.+++", 0000.00, "              ");
    ("$B++/+++.+", 0000.00, "          ");
    ("****.**", 0000.00, "****.**");
    ("ZZZZ.ZZ", 0000.00, "       ");
    ("ZZZZ.99", 0000.00, "    .00");
    ("****.99", 0000.00, "****.00");
    ("ZZ99.99", 0000.00, "  00.00");
    ("Z,ZZZ.ZZ+", +123.456, "  123.45+");
    ("*,***.**+", -123.45, "**123.45-");
    ("**,***,***.**", +12345678.9, "12,345,678.90");
    ("$Z,ZZZ,ZZZ.ZZCR", +12345.67, "$   12,345.67  ");
    ("$B*,***.**BBDB", 0., "*******.******");
    ("$B*,***,***.**BBDB", -12345.67, "$ ***12,345.67  DB");]

let () =
  let config: Cobol_data.Picture.TYPES.config =
    { max_pic_length = 100; decimal_char = '.';
      currency_signs = CHARS.add '$' CHARS.empty }
  in
  List.iter begin fun (pic, value, expected) ->
    match Cobol_data.Picture.of_string config pic with
    | Ok picture ->
        let example = LSP.INTERNAL.Picture_interp.example_of ~picture value in
        let error_msg =
          Pretty.to_string "ERROR: different result (%s, %f) -> '%s' \
                            (expected: '%s')" pic value example expected in
        if not (String.equal example expected)
        then failwith error_msg
    | Error _ ->
        failwith @@
        Pretty.to_string "ERROR: Unable to form picture with picture-string \
                          '%s'\n" pic
  end unit_tests
