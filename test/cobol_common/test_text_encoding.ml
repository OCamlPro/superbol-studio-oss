(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

module TE = Cobol_common.Text_encoding

(* Expected outputs are those of VS Code, obtained by decoding the same bytes
   with iconv-lite 0.6.3 (after detecting byte order marks as VS Code does). *)

let encoding name = Result.get_ok (TE.of_name name)

let hex s =
  String.concat " " @@
  List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) @@
  List.of_seq (String.to_seq s)

let decode name s = print_endline (hex (TE.decode (encoding name) s))

let%expect_test "ascii and well-formed utf8 are returned as they are" =
  List.iter begin fun (name, s) ->
    Printf.printf "%s: %b\n" name (TE.decode (encoding name) s == s)
  end [ "utf8", "  PROGRAM-ID. prog.";
        "utf8", "caf\xc3\xa9";
        "windows1252", "  PROGRAM-ID. prog.";
        "cp850", "" ];
  [%expect {|
    utf8: true
    utf8: true
    windows1252: true
    cp850: true |}];;

let%expect_test "utf8 byte order marks are removed" =
  decode "utf8" "\xef\xbb\xbfcaf\xc3\xa9";
  decode "utf8bom" "\xef\xbb\xbfcaf\xc3\xa9";
  decode "utf8bom" "caf\xc3\xa9";
  [%expect {|
    63 61 66 c3 a9
    63 61 66 c3 a9
    63 61 66 c3 a9 |}];;

let%expect_test "ill-formed utf8 is replaced per maximal subpart" =
  decode "utf8" "\xc3\x28|\xe2\x82|\xf0\x9f\x98|\xed\xa0\x80|\xf4\x90\x80\x80|\
                 \xc0\xaf|\xff|A\xe2\x82";
  [%expect {| ef bf bd 28 7c ef bf bd 7c ef bf bd 7c ef bf bd ef bf bd ef bf bd 7c ef bf bd ef bf bd ef bf bd ef bf bd 7c ef bf bd ef bf bd 7c ef bf bd 7c 41 ef bf bd |}];;

let%expect_test "single-byte encodings" =
  decode "windows1252" "caf\xe9 \x80 \x81";
  decode "cp850" "\x82t\x82";
  decode "iso88591" "\x80\xe9";
  [%expect {|
    63 61 66 c3 a9 20 e2 82 ac 20 ef bf bd
    c3 a9 74 c3 a9
    c2 80 c3 a9 |}];;

let%expect_test "byte order marks take precedence" =
  decode "windows1252" "\xef\xbb\xbfcaf\xc3\xa9";
  decode "windows1252" "\xff\xfeA\x00\xe9\x00";
  decode "windows1252" "\xfe\xff\x00A\x00\xe9";
  [%expect {|
    63 61 66 c3 a9
    41 c3 a9
    41 c3 a9 |}];;

let%expect_test "utf16" =
  decode "utf16le" "A\x00\xe9\x00B";           (* odd trailing byte dropped *)
  decode "utf16le" "\x00\xd8A\x00";                        (* lone surrogate *)
  decode "utf16be" "\x00A\xd8\x3d\xde\x00";                  (* surrogate pair *)
  [%expect {|
    41 c3 a9
    ef bf bd 41
    41 f0 9f 98 80 |}];;

let%expect_test "names" =
  print_endline (TE.name (encoding "cp850"));
  print_endline (Result.get_error (TE.of_name "gbk"));
  [%expect {|
    cp850
    Unsupported encoding `gbk' (supported encodings are: utf8, utf8bom, utf16le, utf16be, windows1252, iso88591, iso88593, iso885915, macroman, cp437, windows1256, iso88596, windows1257, iso88594, iso885914, windows1250, iso88592, cp852, windows1251, cp866, cp1125, iso88595, koi8r, koi8u, iso885913, windows1253, iso88597, windows1255, iso88598, iso885910, iso885916, windows1254, iso88599, windows1258, windows874, iso885911, koi8ru, koi8t, cp865, cp850, cp857) |}];;
