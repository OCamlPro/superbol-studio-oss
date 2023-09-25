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

let%expect_test "default-point/comma" =
  Parser_testing.show_parsed_tokens {|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        PROCEDURE DIVISION.
            DISPLAY 1.1
            DISPLAY 1,1
            DISPLAY -1.1
            DISPLAY -1,1.
  |};
  [%expect {|
    IDENTIFICATION, DIVISION, ., PROGRAM-ID, ., WORD[PROG], ., PROCEDURE,
    DIVISION, ., DISPLAY, FIXED[1.1], DISPLAY, DIGITS[1], DIGITS[1], DISPLAY,
    FIXED[-1.1], DISPLAY, SINT[-1], DIGITS[1], ., EOF
|}];;

let%expect_test "decimal-point-is-comma" =
  Parser_testing.show_parsed_tokens {|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
        PROCEDURE DIVISION.
            DISPLAY 1.1
            DISPLAY 1,1
            DISPLAY -1.1
            DISPLAY -1,1.
  |};
  [%expect {|
    IDENTIFICATION, DIVISION, ., PROGRAM-ID, ., WORD[PROG], ., ENVIRONMENT,
    DIVISION, ., CONFIGURATION, SECTION, ., SPECIAL-NAMES, ., DECIMAL-POINT, IS,
    COMMA, ., PROCEDURE, DIVISION, ., DISPLAY, DIGITS[1], DIGITS[1], DISPLAY,
    FIXED[1,1], DISPLAY, SINT[-1], DIGITS[1], DISPLAY, FIXED[-1,1], ., EOF
|}];;

let%expect_test "decimal-point-is-comma-with-missing-period" =
  (* Note the two missing periods after `COMMA` and `PROCEDURE DIVISION`: this
     tests the proper retokenization and handling of interveaving comma in the
     stream of already issued tokens. *)
  Parser_testing.show_parsed_tokens {|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA
        PROCEDURE DIVISION
            DISPLAY 1.1
            DISPLAY 1,1
            DISPLAY -1.1
            DISPLAY -1,1.
  |};
  [%expect {|
    IDENTIFICATION, DIVISION, ., PROGRAM-ID, ., WORD[PROG], ., ENVIRONMENT,
    DIVISION, ., CONFIGURATION, SECTION, ., SPECIAL-NAMES, ., DECIMAL-POINT, IS,
    COMMA, PROCEDURE, DIVISION, DISPLAY, DIGITS[1], DIGITS[1], DISPLAY,
    DIGITS[1], DIGITS[1], DISPLAY, SINT[-1], DIGITS[1], DISPLAY, FIXED[-1,1], .,
    EOF
|}];;
