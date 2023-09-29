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

let%expect_test "context-sensitive-tokens" =
  Parser_testing.show_parsed_tokens {|
       IDENTIFICATION     DIVISION.
       PROGRAM-ID.        prog.
       DATA               DIVISION.
       WORKING-STORAGE    SECTION.
       01  AWAY-FROM-ZERO PIC 9 VALUE 0.
       01  BYTE-LENGTH    PIC 9.
       01  X              CONSTANT AS BYTE-LENGTH OF BYTE-LENGTH.
       01  Y              CONSTANT AS LENGTH OF BYTE-LENGTH.
       PROCEDURE          DIVISION.
           COMPUTE X ROUNDED MODE AWAY-FROM-ZERO
                   AWAY-FROM-ZERO = 1.1
           END-COMPUTE
           DISPLAY X AWAY-FROM-ZERO NO ADVANCING
           END-DISPLAY.
           STOP RUN.
  |};
  [%expect {|
    IDENTIFICATION, DIVISION, ., PROGRAM-ID, ., INFO_WORD[PROG], ., DATA,
    DIVISION, ., WORKING-STORAGE, SECTION, ., DIGITS[01], WORD[AWAY-FROM-ZERO],
    PICTURE, PICTURE_STRING[9], VALUE, DIGITS[0], ., DIGITS[01],
    WORD[BYTE-LENGTH], PICTURE, PICTURE_STRING[9], ., DIGITS[01], WORD[X],
    CONSTANT, AS, BYTE-LENGTH, OF, WORD[BYTE-LENGTH], ., DIGITS[01], WORD[Y],
    CONSTANT, AS, LENGTH, OF, WORD[BYTE-LENGTH], ., PROCEDURE, DIVISION, .,
    COMPUTE, WORD[X], ROUNDED, MODE, AWAY-FROM-ZERO, WORD[AWAY-FROM-ZERO], =,
    FIXED[1.1], END-COMPUTE, DISPLAY, WORD[X], WORD[AWAY-FROM-ZERO], NO,
    ADVANCING, END-DISPLAY, ., STOP, RUN, ., EOF
|}];;

let%expect_test "context-sensitive-tokens-with-syntax-errors" =
  Parser_testing.show_parsed_tokens {|
       IDENTIFICATION     DIVISION.
       PROGRAM-ID.        prog.
       DATA               DIVISION.
       WORKING-STORAGE    SECTION.
       01  AWAY-FROM-ZERO PIC 9 VALUE 0.
       01  BYTE-LENGTH    PIC 9.
       01  X              CONSTANT AS BYTE-LENGTH BYTE-LENGTH.
       01  Y              CONSTANT LENGTH OF BYTE-LENGTH.
       PROCEDURE          DIVISION.
           COMPUTE X ROUNDED AWAY-FROM-ZERO
                   AWAY-FROM-ZERO = 1.1
           END-COMPUTE
           DISPLAY X AWAY-FROM-ZERO NO ADVANCING
           END-DISPLAY.
           STOP RUN.
  |};
  [%expect {|
    IDENTIFICATION, DIVISION, ., PROGRAM-ID, ., INFO_WORD[PROG], ., DATA,
    DIVISION, ., WORKING-STORAGE, SECTION, ., DIGITS[01], WORD[AWAY-FROM-ZERO],
    PICTURE, PICTURE_STRING[9], VALUE, DIGITS[0], ., DIGITS[01],
    WORD[BYTE-LENGTH], PICTURE, PICTURE_STRING[9], ., DIGITS[01], WORD[X],
    CONSTANT, AS, BYTE-LENGTH, WORD[BYTE-LENGTH], ., DIGITS[01], WORD[Y],
    CONSTANT, LENGTH, OF, WORD[BYTE-LENGTH], ., PROCEDURE, DIVISION, ., COMPUTE,
    WORD[X], ROUNDED, AWAY-FROM-ZERO, AWAY-FROM-ZERO, =, FIXED[1.1], END-COMPUTE,
    DISPLAY, WORD[X], WORD[AWAY-FROM-ZERO], NO, ADVANCING, END-DISPLAY, ., STOP,
    RUN, ., EOF
|}];;
