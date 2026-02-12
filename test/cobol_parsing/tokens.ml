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

let%expect_test "tokens-with-attached-ampersand" =
  (* Just check we extract tokens properly *)
  Parser_testing.show_parsed_tokens {|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
           DISPLAY X"00"&A.
           DISPLAY A&B.
           DISPLAY A&X"00".
           DISPLAY X"100"&X"00".
  |};
  [%expect {|
    IDENTIFICATION, DIVISION, ., PROGRAM-ID, ., INFO_WORD[prog], ., PROCEDURE,
    DIVISION, ., DISPLAY, X"00", &, WORD[A], ., DISPLAY, WORD[A], &, WORD[B], .,
    DISPLAY, WORD[A], &, X"00", ., DISPLAY, X"100", &, X"00", ., EOF
|}];;

(* --- *)

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

let%expect_test "token-locations-with-missing-comment-paragraph" =
  Parser_testing.show_parsed_tokens ~source_format:Auto ~with_locations:true
    ~parser_options:(Parser_testing.options ~verbose:true ())
    "IDENTIFICATION DIVISION.\nAUTHOR.";
  [%expect {|
    Tks: IDENTIFICATION, DIVISION, .
    Tks: AUTHOR, ., COMMENT_ENTRY[], EOF
    IDENTIFICATION@<prog.cob:1-0|1-14>
    DIVISION@<prog.cob:1-15|1-23>
    .@<prog.cob:1-23|1-24>
    AUTHOR@<prog.cob:2-0|2-6>
    .@<prog.cob:2-6|2-7>
    COMMENT_ENTRY[]@<prog.cob:2-7|2-7>
    EOF@<prog.cob:2-7|2-7> |}];;

let%expect_test "token-locations-with-missing-program-id" =
  Parser_testing.show_parsed_tokens ~source_format:Auto ~with_locations:true
    ~parser_options:(Parser_testing.options ~verbose:true ())
    {|PROCEDURE DIVISION.
    para-1.
        IF X>9
        THEN
           IF X>6
           THEN
              DISPLAY "2"
           else
              move 1 to x
        else
          move 1 to x.|};
    [%expect {||}];;
