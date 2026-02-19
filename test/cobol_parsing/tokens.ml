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
    [%expect {|
      Tks: PROCEDURE, DIVISION, .
      Tks':
      Tks: WORD[para-1], .
      Tks: IF
      Tks: WORD[X], >, DIGITS[9]
      Tks: THEN, IF
      Tks: WORD[X], >, DIGITS[6]
      Tks: THEN, DISPLAY, "2"
      Incoming: {UNDERLINE, REVERSE-VIDEO, LOWLIGHT, HIGHLIGHT, FOREGROUND-COLOR,
                 ERASE, BLINK, BELL, BACKGROUND-COLOR}
      Tks': "2"
      Tks: ELSE, MOVE, DIGITS[1], TO
      Outgoing: {UNDERLINE, REVERSE-VIDEO, LOWLIGHT, HIGHLIGHT, FOREGROUND-COLOR,
                 ERASE, BLINK, BELL, BACKGROUND-COLOR}
      Tks: WORD[x]
      Tks: ELSE, MOVE, DIGITS[1], TO, WORD[x], ., EOF
      Tks':
      PROCEDURE@<prog.cob:1-0|1-9>
      DIVISION@<prog.cob:1-10|1-18>
      .@<prog.cob:1-18|1-19>
      WORD[para-1]@<prog.cob:2-4|2-10>
      .@<prog.cob:2-10|2-11>
      IF@<prog.cob:3-8|3-10>
      WORD[X]@<prog.cob:3-11|3-12>
      >@<prog.cob:3-12|3-13>
      DIGITS[9]@<prog.cob:3-13|3-14>
      THEN@<prog.cob:4-8|4-12>
      IF@<prog.cob:5-11|5-13>
      WORD[X]@<prog.cob:5-14|5-15>
      >@<prog.cob:5-15|5-16>
      DIGITS[6]@<prog.cob:5-16|5-17>
      THEN@<prog.cob:6-11|6-15>
      DISPLAY@<prog.cob:7-14|7-21>
      "2"@<prog.cob:7-22|7-25>
      ELSE@<prog.cob:8-11|8-15>
      MOVE@<prog.cob:9-14|9-18>
      DIGITS[1]@<prog.cob:9-19|9-20>
      TO@<prog.cob:9-21|9-23>
      WORD[x]@<prog.cob:9-24|9-25>
      ELSE@<prog.cob:10-8|10-12>
      MOVE@<prog.cob:11-10|11-14>
      DIGITS[1]@<prog.cob:11-15|11-16>
      TO@<prog.cob:11-17|11-19>
      WORD[x]@<prog.cob:11-20|11-21>
      .@<prog.cob:11-21|11-22>
      EOF@<prog.cob:11-22|11-22> |}];;

let%expect_test "tokens-with-tabs" =
  Parser_testing.show_parsed_tokens ~source_format:(SF SFFixed)
    ~parser_options:(Parser_testing.options ~verbose:true ())
    {|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
      		STRING 	W-AGT ";"   W-RUBNUM (J) ";" 
      		W-RENVOINOTE W-DEST-NOM  ";" 
      		W-DEST-RUE1  ";"  W-DEST-RUE2  ";" 
      		W-DEST-CP ";"  W-DEST-VILLE 
      		";" W-DEST-TEL1 (1:2)  " "
                   W-DEST-TEL1 (3:2)  " "
                   W-DEST-TEL1 (5:2)  " "
                   W-DEST-TEL1 (7:2)  " "
                   W-DEST-TEL1 (9:2)
             delimited  by  "   "  into  LARTISAN.
|};
    [%expect {|
      Tks: IDENTIFICATION, DIVISION, .
      Tks: PROGRAM-ID, ., INFO_WORD[prog], .
      Incoming: {RECURSIVE}
      Tks': ., INFO_WORD[prog], .
      Tks: PROCEDURE, DIVISION, .
      Outgoing: {RECURSIVE}
      Tks':
      Tks: STRING, WORD[W-AGT], ";", WORD[W-RUBNUM], (, WORD[J], ), ";"
      Tks: WORD_IN_AREA_A[W-RENVOINOTE], WORD[W-DEST-NOM], ";"
      Tks: WORD_IN_AREA_A[W-DEST-RUE1], ";", WORD[W-DEST-RUE2], ";"
      Tks: WORD_IN_AREA_A[W-DEST-CP], ";"
      Tks: WORD[W-DEST-VILLE], ";", WORD[W-DEST-TEL1], (, DIGITS[1], :, DIGITS[2],
           ), " "
      Tks: WORD[W-DEST-TEL1], (, DIGITS[3], :, DIGITS[2], ), " "
      Tks: WORD[W-DEST-TEL1], (, DIGITS[5], :, DIGITS[2], ), " "
      Tks: WORD[W-DEST-TEL1], (, DIGITS[7], :, DIGITS[2], ), " "
      Tks: WORD[W-DEST-TEL1]
      Tks: (, DIGITS[9], :, DIGITS[2], ), DELIMITED, BY, "   ", INTO,
           WORD[LARTISAN], .
      Tks: EOF
      Tks':
      IDENTIFICATION, DIVISION, ., PROGRAM-ID, ., INFO_WORD[prog], ., PROCEDURE,
      DIVISION, ., STRING, WORD[W-AGT], ";", WORD[W-RUBNUM], (, WORD[J], ), ";",
      WORD_IN_AREA_A[W-RENVOINOTE], WORD[W-DEST-NOM], ";",
      WORD_IN_AREA_A[W-DEST-RUE1], ";", WORD[W-DEST-RUE2], ";",
      WORD_IN_AREA_A[W-DEST-CP], ";", WORD[W-DEST-VILLE], ";", WORD[W-DEST-TEL1],
      (, DIGITS[1], :, DIGITS[2], ), " ", WORD[W-DEST-TEL1], (, DIGITS[3], :,
      DIGITS[2], ), " ", WORD[W-DEST-TEL1], (, DIGITS[5], :, DIGITS[2], ), " ",
      WORD[W-DEST-TEL1], (, DIGITS[7], :, DIGITS[2], ), " ", WORD[W-DEST-TEL1], (,
      DIGITS[9], :, DIGITS[2], ), DELIMITED, BY, "   ", INTO, WORD[LARTISAN], .,
      EOF |}];;
