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

let%expect_test "tokens" =
  (* Just check we extract tokens properly *)
  Parser_testing.show_parsed_tokens {|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
           STOP RUN.
  |};
  [%expect {|
    IDENTIFICATION, DIVISION, ., PROGRAM-ID, ., INFO_WORD[prog], ., PROCEDURE,
    DIVISION, ., STOP, RUN, ., EOF
|}];;

let%expect_test "tokens-after-syntax-errors" =
  (* Check we extract tokens properly even after syntax errors *)
  Parser_testing.show_parsed_tokens {|
       IDENTIFICATION
       PROGRAM-ID.
       PROCEDURE DIVISION
           MOVE X Y
           STOP RUN.
  |};
  [%expect {|
    IDENTIFICATION, PROGRAM-ID, ., INFO_WORD[PROCEDURE], DIVISION, MOVE, WORD[X],
    WORD[Y], STOP, RUN, ., EOF
|}];;

let%expect_test "token-locations" =
  Parser_testing.show_parsed_tokens ~source_format:Auto ~with_locations:true
    ~parser_options:(Parser_testing.options ~verbose:true ())
    {|(TMP:1)|};
  [%expect {|
    Tks: (, WORD[TMP], :, DIGITS[1], ), EOF
    (@<prog.cob:1-0|1-1>
    WORD[TMP]@<prog.cob:1-1|1-4>
    :@<prog.cob:1-4|1-5>
    DIGITS[1]@<prog.cob:1-5|1-6>
    )@<prog.cob:1-6|1-7>
    EOF@<prog.cob:1-7|1-7> |}];;
