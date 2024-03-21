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
    IDENTIFICATION, DIVISION, ., PROGRAM-ID, ., INFO_WORD[prog], ., DATA,
    DIVISION, ., WORKING-STORAGE, SECTION, ., DIGITS[01], WORD[AWAY-FROM-ZERO],
    PICTURE, PICTURE_STRING[9], VALUE, DIGITS[0], ., DIGITS[01],
    WORD[BYTE-LENGTH], PICTURE, PICTURE_STRING[9], ., DIGITS[01], WORD[X],
    CONSTANT, AS, BYTE-LENGTH, OF, WORD[BYTE-LENGTH], ., DIGITS[01], WORD[Y],
    CONSTANT, AS, LENGTH, OF, WORD[BYTE-LENGTH], ., PROCEDURE, DIVISION, .,
    COMPUTE, WORD[X], ROUNDED, MODE, AWAY-FROM-ZERO, WORD[AWAY-FROM-ZERO], =,
    FIXED[1.1], END-COMPUTE, DISPLAY, WORD[X], WORD[AWAY-FROM-ZERO],
    WITH_NO_ADVANCING, END-DISPLAY, ., STOP, RUN, ., EOF
|}];;

let%expect_test "context-sensitive-tokens-bis" =
  Parser_testing.show_parsed_tokens {|
       IDENTIFICATION     DIVISION.
       PROGRAM-ID.        prog.
       DATA               DIVISION.
       WORKING-STORAGE    SECTION.
       01  AWAY-FROM-ZERO PIC 9 VALUE 0.
       01  BYTE-LENGTH    PIC 9.
       01  X              CONSTANT BYTE-LENGTH BYTE-LENGTH.
       01  Y              CONSTANT LENGTH BYTE-LENGTH.
       PROCEDURE          DIVISION.
           STOP RUN.
  |};
  [%expect {|
    IDENTIFICATION, DIVISION, ., PROGRAM-ID, ., INFO_WORD[prog], ., DATA,
    DIVISION, ., WORKING-STORAGE, SECTION, ., DIGITS[01], WORD[AWAY-FROM-ZERO],
    PICTURE, PICTURE_STRING[9], VALUE, DIGITS[0], ., DIGITS[01],
    WORD[BYTE-LENGTH], PICTURE, PICTURE_STRING[9], ., DIGITS[01], WORD[X],
    CONSTANT, BYTE-LENGTH, WORD[BYTE-LENGTH], ., DIGITS[01], WORD[Y], CONSTANT,
    LENGTH, WORD[BYTE-LENGTH], ., PROCEDURE, DIVISION, ., STOP, RUN, ., EOF
|}];;

let%expect_test "context-sensitive-tokens-ter" =
  Parser_testing.show_parsed_tokens {|
       IDENTIFICATION     DIVISION.
       PROGRAM-ID.        prog.
       DATA               DIVISION.
       WORKING-STORAGE    SECTION.
       01  AWAY-FROM-ZERO PIC 9 VALUE 0.
       01  BYTE-LENGTH    PIC 9.
       01  X              CONSTANT IS GLOBAL AS BYTE-LENGTH Y.
       01  Y              CONSTANT LENGTH BYTE-LENGTH.
       PROCEDURE          DIVISION.
           STOP RUN.
  |};
  [%expect {|
    IDENTIFICATION, DIVISION, ., PROGRAM-ID, ., INFO_WORD[prog], ., DATA,
    DIVISION, ., WORKING-STORAGE, SECTION, ., DIGITS[01], WORD[AWAY-FROM-ZERO],
    PICTURE, PICTURE_STRING[9], VALUE, DIGITS[0], ., DIGITS[01],
    WORD[BYTE-LENGTH], PICTURE, PICTURE_STRING[9], ., DIGITS[01], WORD[X],
    CONSTANT, IS_GLOBAL, AS, BYTE-LENGTH, WORD[Y], ., DIGITS[01], WORD[Y],
    CONSTANT, LENGTH, WORD[BYTE-LENGTH], ., PROCEDURE, DIVISION, ., STOP, RUN, .,
    EOF
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
    IDENTIFICATION, DIVISION, ., PROGRAM-ID, ., INFO_WORD[prog], ., DATA,
    DIVISION, ., WORKING-STORAGE, SECTION, ., DIGITS[01], WORD[AWAY-FROM-ZERO],
    PICTURE, PICTURE_STRING[9], VALUE, DIGITS[0], ., DIGITS[01],
    WORD[BYTE-LENGTH], PICTURE, PICTURE_STRING[9], ., DIGITS[01], WORD[X],
    CONSTANT, AS, BYTE-LENGTH, WORD[BYTE-LENGTH], ., DIGITS[01], WORD[Y],
    CONSTANT, LENGTH, OF, WORD[BYTE-LENGTH], ., PROCEDURE, DIVISION, ., COMPUTE,
    WORD[X], ROUNDED, AWAY-FROM-ZERO, AWAY-FROM-ZERO, =, FIXED[1.1], END-COMPUTE,
    DISPLAY, WORD[X], WORD[AWAY-FROM-ZERO], WITH_NO_ADVANCING, END-DISPLAY, .,
    STOP, RUN, ., EOF
|}];;

let%expect_test "context-sensitive-tokens-lower-case" =
  Parser_testing.show_parsed_tokens {|
       program-id.        prog.
       data               division.
       working-storage    section.
       01  byte-length    pic 9.
       01  X              constant byte-length byte-length.
       procedure division.
  |};
  (* Note the `WORD[BYTE-LENGTH]` below, that was originally lower-case.  TODO:
     find a *clean* way to fix that. *)
  [%expect {|
    PROGRAM-ID, ., INFO_WORD[prog], ., DATA, DIVISION, ., WORKING-STORAGE,
    SECTION, ., DIGITS[01], WORD[byte-length], PICTURE, PICTURE_STRING[9], .,
    DIGITS[01], WORD[X], CONSTANT, BYTE-LENGTH, WORD[BYTE-LENGTH], ., PROCEDURE,
    DIVISION, ., EOF
|}];;
