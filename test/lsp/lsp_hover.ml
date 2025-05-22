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

open Lsp.Types
open Lsp_testing

let print_hovered server ~projdir (prog, prog_positions) =
  let server, prog = add_cobol_doc server ~projdir "prog.cob" prog in
  let location_as_srcloc = new srcloc_resuscitator_cache in
  let hover_position ?key position =
    let params = HoverParams.create ~position ~textDocument:prog () in
    Pretty.out "%a(line %d, character %d):@."
      Fmt.(option ~none:nop @@ fmt "%s ") key
      position.line position.character;
    match
      LSP.Request.hover ~always_show_hover_text_in_data_div:true server params
    with
    | None ->
        Pretty.out "Hovering nothing worthy@."
    | Some { contents = `List strings; range } ->
        location_as_srcloc#print_optional_range_for ~uri:prog.uri range;
        List.iter (fun MarkedString.{ value; _ } -> print_endline value) strings
    | Some { contents = `MarkedString MarkedString.{ value; _ } |
                        `MarkupContent MarkupContent.{ value; _ }; range } ->
        location_as_srcloc#print_optional_range_for ~uri:prog.uri range;
        print_endline value
  in
  List.iter (fun pos -> hover_position pos) prog_positions.pos_anonymous;
  StrMap.iter (fun key pos -> hover_position ~key pos) prog_positions.pos_map

(* hover copy *)

let%expect_test "hover-copy" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  let server,    _ = add_cobol_doc server ~projdir "lib.cpy" {cobol|
       01 FIELD PIC X.
  |cobol} in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       _|_COP_|_Y "_|_li_|_b.cpy".
       PROCEDURE DIVISION.
          DISPLAY FIELD
          STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/lib.cpy appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib.cpy"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 5, character 7):
    __rootdir__/prog.cob:6.7-6.22:
       3          PROGRAM-ID. prog.
       4          DATA DIVISION.
       5          WORKING-STORAGE SECTION.
       6 >        COPY "lib.cpy".
    ----          ^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8             DISPLAY FIELD
    ```cobol
    FIELD
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    ---
    Additional pre-processing:
    ```cobol
           01 FIELD PIC X.
    ```
    (line 5, character 10):
    __rootdir__/prog.cob:6.7-6.22:
       3          PROGRAM-ID. prog.
       4          DATA DIVISION.
       5          WORKING-STORAGE SECTION.
       6 >        COPY "lib.cpy".
    ----          ^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8             DISPLAY FIELD
    ```cobol
    FIELD
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    ---
    Additional pre-processing:
    ```cobol
           01 FIELD PIC X.
    ```
    (line 5, character 13):
    __rootdir__/prog.cob:6.7-6.22:
       3          PROGRAM-ID. prog.
       4          DATA DIVISION.
       5          WORKING-STORAGE SECTION.
       6 >        COPY "lib.cpy".
    ----          ^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8             DISPLAY FIELD
    ```cobol
    FIELD
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    ---
    Additional pre-processing:
    ```cobol
           01 FIELD PIC X.
    ```
    (line 5, character 15):
    __rootdir__/prog.cob:6.7-6.22:
       3          PROGRAM-ID. prog.
       4          DATA DIVISION.
       5          WORKING-STORAGE SECTION.
       6 >        COPY "lib.cpy".
    ----          ^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8             DISPLAY FIELD
    ```cobol
    FIELD
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    ---
    Additional pre-processing:
    ```cobol
           01 FIELD PIC X.
    ``` |}];;

let%expect_test "hover-typedef-from-copy" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  let server,    _ = add_cobol_doc server ~projdir "lib.cpy" {cobol|
       01 FIELD PIC X.
  |cobol} in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "lib.cpy".
       PROCEDURE DIVISION.
          DISPLAY FIEL_|_D
          STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/lib.cpy appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib.cpy"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 7, character 22):
    __rootdir__/prog.cob:8.18-8.23:
       5          WORKING-STORAGE SECTION.
       6          COPY "lib.cpy".
       7          PROCEDURE DIVISION.
       8 >           DISPLAY FIELD
    ----                     ^^^^^
       9             STOP RUN.
      10
    ```cobol
    FIELD
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1) |}];;

(* Hover replaced *)

let%expect_test "hover-replaced" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
       REPLACE =="A"== BY =="B" "C"==.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
          DISPLAY "_|_A"
          STOP RUN.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 5, character 19):
    __rootdir__/prog.cob:6.18-6.21:
       3          IDENTIFICATION DIVISION.
       4          PROGRAM-ID. prog.
       5          PROCEDURE DIVISION.
       6 >           DISPLAY "A"
    ----                     ^^^
       7             STOP RUN.
       8
    ```cobol
    "B" "C"
    ``` |}];;

(* Hover typedef vars *)

let%expect_test "hover-typedef-vars" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 DATA-N_|_AME PI_|_C X.
        01 STR_|_UCT.
          02 STRUCT-1 PICTURE 9_|_99 VALUE 123.
          02 STR_|_UCT-2 PICTURE X VALUE QUOTE.
          02 STRUCT-3 PICTURE X(6) VAL_|_UE "ABC456".
        01 BIG_|_ PIC X(38) VALUE "************************************".
        PROCEDURE DIVISION.
          DISPLAY _|_DATA-NAME STRUC_|_T S_|_TRUCT-1 STR_|_UCT-2 STR_|_UCT-3
          STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 5, character 17):
    __rootdir__/prog.cob:6.11-6.20:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^^^^^^^^^
       7           01 STRUCT.
       8             02 STRUCT-1 PICTURE 999 VALUE 123.
    ```cobol
    DATA-NAME
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    (line 5, character 23):
    __rootdir__/prog.cob:6.8-6.27:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----           ^^^^^^^^^^^^^^^^^^^
       7           01 STRUCT.
       8             02 STRUCT-1 PICTURE 999 VALUE 123.
    ```cobol
    DATA-NAME
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    (line 6, character 14):
    __rootdir__/prog.cob:7.11-7.17:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         01 STRUCT.
    ----              ^^^^^^
       8             02 STRUCT-1 PICTURE 999 VALUE 123.
       9             02 STRUCT-2 PICTURE X VALUE QUOTE.
    ```cobol
    STRUCT
    ```
    Group of 3 subfields
    Size: 80 bits
    (line 7, character 31):
    __rootdir__/prog.cob:8.10-8.44:
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7           01 STRUCT.
       8 >           02 STRUCT-1 PICTURE 999 VALUE 123.
    ----             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       9             02 STRUCT-2 PICTURE X VALUE QUOTE.
      10             02 STRUCT-3 PICTURE X(6) VALUE "ABC456".
    ```cobol
    STRUCT-1 IN STRUCT
    ```
    ```cobol
    PIC 999 USAGE DISPLAY
    ```
    NUMERIC(digits = 3, scale = 0, with_sign = false)
    *e.g,* [`000`] (0), [`123`] (123)
    VALUE 123
    (line 8, character 16):
    __rootdir__/prog.cob:9.13-9.21:
       6           01 DATA-NAME PIC X.
       7           01 STRUCT.
       8             02 STRUCT-1 PICTURE 999 VALUE 123.
       9 >           02 STRUCT-2 PICTURE X VALUE QUOTE.
    ----                ^^^^^^^^
      10             02 STRUCT-3 PICTURE X(6) VALUE "ABC456".
      11           01 BIG PIC X(38) VALUE "************************************".
    ```cobol
    STRUCT-2 IN STRUCT
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    VALUE QUOTE
    (line 9, character 38):
    __rootdir__/prog.cob:10.10-10.50:
       7           01 STRUCT.
       8             02 STRUCT-1 PICTURE 999 VALUE 123.
       9             02 STRUCT-2 PICTURE X VALUE QUOTE.
      10 >           02 STRUCT-3 PICTURE X(6) VALUE "ABC456".
    ----             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      11           01 BIG PIC X(38) VALUE "************************************".
      12           PROCEDURE DIVISION.
    ```cobol
    STRUCT-3 IN STRUCT
    ```
    ```cobol
    PIC X(6) USAGE DISPLAY
    ```
    ALPHANUMERIC(6)
    VALUE "ABC456"
    (line 10, character 14):
    __rootdir__/prog.cob:11.11-11.14:
       8             02 STRUCT-1 PICTURE 999 VALUE 123.
       9             02 STRUCT-2 PICTURE X VALUE QUOTE.
      10             02 STRUCT-3 PICTURE X(6) VALUE "ABC456".
      11 >         01 BIG PIC X(38) VALUE "************************************".
    ----              ^^^
      12           PROCEDURE DIVISION.
      13             DISPLAY DATA-NAME STRUCT STRUCT-1 STRUCT-2 STRUCT-3
    ```cobol
    BIG
    ```
    ```cobol
    PIC X(38) USAGE DISPLAY
    ```
    ALPHANUMERIC(38)
    VALUE "************************************"
    (line 12, character 18):
    __rootdir__/prog.cob:13.18-13.27:
      10             02 STRUCT-3 PICTURE X(6) VALUE "ABC456".
      11           01 BIG PIC X(38) VALUE "************************************".
      12           PROCEDURE DIVISION.
      13 >           DISPLAY DATA-NAME STRUCT STRUCT-1 STRUCT-2 STRUCT-3
    ----                     ^^^^^^^^^
      14             STOP RUN.
      15
    ```cobol
    DATA-NAME
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    (line 12, character 33):
    __rootdir__/prog.cob:13.28-13.34:
      10             02 STRUCT-3 PICTURE X(6) VALUE "ABC456".
      11           01 BIG PIC X(38) VALUE "************************************".
      12           PROCEDURE DIVISION.
      13 >           DISPLAY DATA-NAME STRUCT STRUCT-1 STRUCT-2 STRUCT-3
    ----                               ^^^^^^
      14             STOP RUN.
      15
    ```cobol
    STRUCT
    ```
    Group of 3 subfields
    Size: 80 bits
    (line 12, character 36):
    __rootdir__/prog.cob:13.35-13.43:
      10             02 STRUCT-3 PICTURE X(6) VALUE "ABC456".
      11           01 BIG PIC X(38) VALUE "************************************".
      12           PROCEDURE DIVISION.
      13 >           DISPLAY DATA-NAME STRUCT STRUCT-1 STRUCT-2 STRUCT-3
    ----                                      ^^^^^^^^
      14             STOP RUN.
      15
    ```cobol
    STRUCT-1 IN STRUCT
    ```
    ```cobol
    PIC 999 USAGE DISPLAY
    ```
    NUMERIC(digits = 3, scale = 0, with_sign = false)
    *e.g,* [`000`] (0), [`123`] (123)
    VALUE 123
    (line 12, character 47):
    __rootdir__/prog.cob:13.44-13.52:
      10             02 STRUCT-3 PICTURE X(6) VALUE "ABC456".
      11           01 BIG PIC X(38) VALUE "************************************".
      12           PROCEDURE DIVISION.
      13 >           DISPLAY DATA-NAME STRUCT STRUCT-1 STRUCT-2 STRUCT-3
    ----                                               ^^^^^^^^
      14             STOP RUN.
      15
    ```cobol
    STRUCT-2 IN STRUCT
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    VALUE QUOTE
    (line 12, character 56):
    __rootdir__/prog.cob:13.53-13.61:
      10             02 STRUCT-3 PICTURE X(6) VALUE "ABC456".
      11           01 BIG PIC X(38) VALUE "************************************".
      12           PROCEDURE DIVISION.
      13 >           DISPLAY DATA-NAME STRUCT STRUCT-1 STRUCT-2 STRUCT-3
    ----                                                        ^^^^^^^^
      14             STOP RUN.
      15
    ```cobol
    STRUCT-3 IN STRUCT
    ```
    ```cobol
    PIC X(6) USAGE DISPLAY
    ```
    ALPHANUMERIC(6)
    VALUE "ABC456" |}];;

let%expect_test "hover-typedef-vars-usage" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 _|_VAR PIC -BZZZ,ZZ9.99.
        01 _|_VAR1 PIC 9 USAGE BINARY.
        01 _|_VAR4 USAGE BINARY-SHORT.
        01 _|_VAR3 USAGE BINARY-C-LONG.
        01 _|_VAR6 PIC 111 USAGE BIT.
        01 _|_VAR7 USAGE POINTER.
        01 _|_VAR8 PIC 9 USAGE PACKED-DECIMAL.
        01 _|_VAR9 PIC $++/+.+B+.
        PROCEDURE DIVISION.
          STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 5, character 11):
    __rootdir__/prog.cob:6.11-6.14:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 VAR PIC -BZZZ,ZZ9.99.
    ----              ^^^
       7           01 VAR1 PIC 9 USAGE BINARY.
       8           01 VAR4 USAGE BINARY-SHORT.
    ```cobol
    VAR
    ```
    ```cobol
    PIC -BZZZ,ZZ9.99 USAGE DISPLAY
    ```
    NUMERIC(digits = 8, scale = 2, with_sign = false)
    *e.g,* [`        0.00`] (0), [`  123,456.78`] (123456.78)
    (line 6, character 11):
    __rootdir__/prog.cob:7.11-7.15:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 VAR PIC -BZZZ,ZZ9.99.
       7 >         01 VAR1 PIC 9 USAGE BINARY.
    ----              ^^^^
       8           01 VAR4 USAGE BINARY-SHORT.
       9           01 VAR3 USAGE BINARY-C-LONG.
    ```cobol
    VAR1
    ```
    ```cobol
    PIC 9 USAGE BINARY
    ```
    NUMERIC(digits = 1, scale = 0, with_sign = false)
    *e.g,* [`0`] (0), [`1`] (1)
    (line 7, character 11):
    __rootdir__/prog.cob:8.11-8.15:
       5           WORKING-STORAGE SECTION.
       6           01 VAR PIC -BZZZ,ZZ9.99.
       7           01 VAR1 PIC 9 USAGE BINARY.
       8 >         01 VAR4 USAGE BINARY-SHORT.
    ----              ^^^^
       9           01 VAR3 USAGE BINARY-C-LONG.
      10           01 VAR6 PIC 111 USAGE BIT.
    ```cobol
    VAR4
    ```
    ```cobol
    USAGE BINARY-SHORT SIGNED
    ```
    (line 8, character 11):
    __rootdir__/prog.cob:9.11-9.15:
       6           01 VAR PIC -BZZZ,ZZ9.99.
       7           01 VAR1 PIC 9 USAGE BINARY.
       8           01 VAR4 USAGE BINARY-SHORT.
       9 >         01 VAR3 USAGE BINARY-C-LONG.
    ----              ^^^^
      10           01 VAR6 PIC 111 USAGE BIT.
      11           01 VAR7 USAGE POINTER.
    ```cobol
    VAR3
    ```
    ```cobol
    USAGE BINARY-C-LONG SIGNED
    ```
    (line 9, character 11):
    __rootdir__/prog.cob:10.11-10.15:
       7           01 VAR1 PIC 9 USAGE BINARY.
       8           01 VAR4 USAGE BINARY-SHORT.
       9           01 VAR3 USAGE BINARY-C-LONG.
      10 >         01 VAR6 PIC 111 USAGE BIT.
    ----              ^^^^
      11           01 VAR7 USAGE POINTER.
      12           01 VAR8 PIC 9 USAGE PACKED-DECIMAL.
    ```cobol
    VAR6
    ```
    ```cobol
    PIC 111 USAGE BIT
    ```
    BOOLEAN(3)
    (line 10, character 11):
    __rootdir__/prog.cob:11.11-11.15:
       8           01 VAR4 USAGE BINARY-SHORT.
       9           01 VAR3 USAGE BINARY-C-LONG.
      10           01 VAR6 PIC 111 USAGE BIT.
      11 >         01 VAR7 USAGE POINTER.
    ----              ^^^^
      12           01 VAR8 PIC 9 USAGE PACKED-DECIMAL.
      13           01 VAR9 PIC $++/+.+B+.
    ```cobol
    VAR7
    ```
    Pointer
    (line 11, character 11):
    __rootdir__/prog.cob:12.11-12.15:
       9           01 VAR3 USAGE BINARY-C-LONG.
      10           01 VAR6 PIC 111 USAGE BIT.
      11           01 VAR7 USAGE POINTER.
      12 >         01 VAR8 PIC 9 USAGE PACKED-DECIMAL.
    ----              ^^^^
      13           01 VAR9 PIC $++/+.+B+.
      14           PROCEDURE DIVISION.
    ```cobol
    VAR8
    ```
    ```cobol
    PIC 9 USAGE PACKED-DECIMAL
    ```
    NUMERIC(digits = 1, scale = 0, with_sign = false)
    *e.g,* [`0`] (0), [`1`] (1)
    (line 12, character 11):
    __rootdir__/prog.cob:13.11-13.15:
      10           01 VAR6 PIC 111 USAGE BIT.
      11           01 VAR7 USAGE POINTER.
      12           01 VAR8 PIC 9 USAGE PACKED-DECIMAL.
      13 >         01 VAR9 PIC $++/+.+B+.
    ----              ^^^^
      14           PROCEDURE DIVISION.
      15             STOP RUN.
    ```cobol
    VAR9
    ```
    ```cobol
    PIC $++/+.+B+ USAGE DISPLAY
    ```
    NUMERIC(digits = 4, scale = 2, with_sign = false)
    *e.g,* [`         `] (0), [`$+1/2.3 4`] (12.34) |}];;

let%expect_test "hover-typedef-filler-vars" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 FIL_|_LER PIC X.
        01 FIL_|_LER.
          02 F-1 PICTURE X.
        01 STRU_|_CT.
          02 FILLER PICTURE 9_|_99 VALUE 123.
          02 STRUCT-1 PICTURE X VALUE QUOTE.
        PROCEDURE DIVISION.
          STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 5, character 14):
    Hovering nothing worthy
    (line 6, character 14):
    Hovering nothing worthy
    (line 8, character 15):
    __rootdir__/prog.cob:9.11-9.17:
       6           01 FILLER PIC X.
       7           01 FILLER.
       8             02 F-1 PICTURE X.
       9 >         01 STRUCT.
    ----              ^^^^^^
      10             02 FILLER PICTURE 999 VALUE 123.
      11             02 STRUCT-1 PICTURE X VALUE QUOTE.
    ```cobol
    STRUCT
    ```
    Group of 2 subfields
    Size: 32 bits
    (line 9, character 29):
    Hovering nothing worthy |}];;

(* Hover typedef cond *)

let%expect_test "hover-typedef-simple-condition" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 VAL_|_ PIC X.
          88 C_|_OND VALUE "A".
          88 CO_|_NDTHRU VALUE "a" THRU "z".
        PROCEDURE DIVISION.
          SET _|_COND TO TRUE
          SET CONDTHRU_|_ TO TRUE
          STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 5, character 14):
    __rootdir__/prog.cob:6.11-6.14:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 VAL PIC X.
    ----              ^^^
       7             88 COND VALUE "A".
       8             88 CONDTHRU VALUE "a" THRU "z".
    ```cobol
    VAL
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    (line 6, character 14):
    Hovering nothing worthy
    (line 7, character 15):
    Hovering nothing worthy
    (line 9, character 14):
    __rootdir__/prog.cob:10.14-10.18:
       7             88 COND VALUE "A".
       8             88 CONDTHRU VALUE "a" THRU "z".
       9           PROCEDURE DIVISION.
      10 >           SET COND TO TRUE
    ----                 ^^^^
      11             SET CONDTHRU TO TRUE
      12             STOP RUN.
    ```cobol
    88 COND VALUE "A".
    ```
    ```cobol
    VAL
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    (line 10, character 22):
    __rootdir__/prog.cob:11.14-11.22:
       8             88 CONDTHRU VALUE "a" THRU "z".
       9           PROCEDURE DIVISION.
      10             SET COND TO TRUE
      11 >           SET CONDTHRU TO TRUE
    ----                 ^^^^^^^^
      12             STOP RUN.
      13
    ```cobol
    88 CONDTHRU VALUE "a" THROUGH "z".
    ```
    ```cobol
    VAL
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1) |}];;

let%expect_test "hover-typedef-group-condition" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 STRUCT.
          02 VAL-1 PIC X.
            88 CO_|_ND-1 VALUE "a" THRU "z".
          02 VAL-2 PIC X.
            88 CO_|_ND-2 VALUE "a".
        PROCEDURE DIVISION.
          SET _|_COND-1 TO TRUE
          SET _|_COND-2 TO TRUE
          STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 7, character 17):
    __rootdir__/prog.cob:6.8-9.25:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 STRUCT.
    ----           ^^^^^^^^^^
       7 >           02 VAL-1 PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^
       8 >             88 COND-1 VALUE "a" THRU "z".
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       9 >           02 VAL-2 PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^
      10               88 COND-2 VALUE "a".
      11           PROCEDURE DIVISION.
    ```cobol
    STRUCT
    ```
    Group of 2 subfields
    Size: 16 bits
    (line 9, character 17):
    Hovering nothing worthy
    (line 11, character 14):
    __rootdir__/prog.cob:12.14-12.20:
       9             02 VAL-2 PIC X.
      10               88 COND-2 VALUE "a".
      11           PROCEDURE DIVISION.
      12 >           SET COND-1 TO TRUE
    ----                 ^^^^^^
      13             SET COND-2 TO TRUE
      14             STOP RUN.
    ```cobol
    88 COND-1 VALUE "a" THROUGH "z".
    ```
    ```cobol
    VAL-1 IN STRUCT
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    (line 12, character 14):
    __rootdir__/prog.cob:13.14-13.20:
      10               88 COND-2 VALUE "a".
      11           PROCEDURE DIVISION.
      12             SET COND-1 TO TRUE
      13 >           SET COND-2 TO TRUE
    ----                 ^^^^^^
      14             STOP RUN.
      15
    ```cobol
    88 COND-2 VALUE "a".
    ```
    ```cobol
    VAL-2 IN STRUCT
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1) |}];;

let%expect_test "hover-typedef-renames" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 X.
          05 Y PIC 9.
          05 YY PIC XX.
          66 Z_|_ R_|_ENAMES _|_Y.
          66 Y-THRU-YY_|_ RENAMES Y THRU YY.
        PROCEDURE DIVISION.
            DISPLAY _|_Z.
            STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 8, character 14):
    __rootdir__/prog.cob:9.13-9.14:
       6           01 X.
       7             05 Y PIC 9.
       8             05 YY PIC XX.
       9 >           66 Z RENAMES Y.
    ----                ^
      10             66 Y-THRU-YY RENAMES Y THRU YY.
      11           PROCEDURE DIVISION.
    ```cobol
    Z IN X
    RENAMES Y IN X
    ```
    ```cobol
    PIC 9 USAGE DISPLAY
    ```
    NUMERIC(digits = 1, scale = 0, with_sign = false)
    *e.g,* [`0`] (0), [`1`] (1)
    (line 8, character 16):
    __rootdir__/prog.cob:9.10-9.25:
       6           01 X.
       7             05 Y PIC 9.
       8             05 YY PIC XX.
       9 >           66 Z RENAMES Y.
    ----             ^^^^^^^^^^^^^^^
      10             66 Y-THRU-YY RENAMES Y THRU YY.
      11           PROCEDURE DIVISION.
    ```cobol
    Z IN X
    RENAMES Y IN X
    ```
    ```cobol
    PIC 9 USAGE DISPLAY
    ```
    NUMERIC(digits = 1, scale = 0, with_sign = false)
    *e.g,* [`0`] (0), [`1`] (1)
    (line 8, character 23):
    __rootdir__/prog.cob:9.23-9.24:
       6           01 X.
       7             05 Y PIC 9.
       8             05 YY PIC XX.
       9 >           66 Z RENAMES Y.
    ----                          ^
      10             66 Y-THRU-YY RENAMES Y THRU YY.
      11           PROCEDURE DIVISION.
    ```cobol
    Y IN X
    ```
    ```cobol
    PIC 9 USAGE DISPLAY
    ```
    NUMERIC(digits = 1, scale = 0, with_sign = false)
    *e.g,* [`0`] (0), [`1`] (1)
    (line 9, character 22):
    __rootdir__/prog.cob:10.13-10.22:
       7             05 Y PIC 9.
       8             05 YY PIC XX.
       9             66 Z RENAMES Y.
      10 >           66 Y-THRU-YY RENAMES Y THRU YY.
    ----                ^^^^^^^^^
      11           PROCEDURE DIVISION.
      12               DISPLAY Z.
    ```cobol
    Y-THRU-YY IN X
    RENAMES Y IN X
    THRU YY IN X
    ```
    ```cobol
    PIC XXX USAGE DISPLAY
    ```
    ALPHANUMERIC(3)
    (line 11, character 20):
    __rootdir__/prog.cob:12.20-12.21:
       9             66 Z RENAMES Y.
      10             66 Y-THRU-YY RENAMES Y THRU YY.
      11           PROCEDURE DIVISION.
      12 >             DISPLAY Z.
    ----                       ^
      13               STOP RUN.
      14
    ```cobol
    Z IN X
    RENAMES Y IN X
    ```
    ```cobol
    PIC 9 USAGE DISPLAY
    ```
    NUMERIC(digits = 1, scale = 0, with_sign = false)
    *e.g,* [`0`] (0), [`1`] (1) |}];;

let%expect_test "hover-typedef-redefines" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 X.
          05 Y PIC 9.
          05 _|_Z REDEFINES Y_|_
        PROCEDURE DIVISION.
            DISPLAY _|_Z.
            STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[{"message":"Missing .","range":{"end":{"character":26,"line":7},"start":{"character":26,"line":7}},"severity":4},{"message":"Missing PICTURE clause for item 'Z'","range":{"end":{"character":26,"line":7},"start":{"character":10,"line":7}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 7, character 13):
    __rootdir__/prog.cob:8.13-8.14:
       5           WORKING-STORAGE SECTION.
       6           01 X.
       7             05 Y PIC 9.
       8 >           05 Z REDEFINES Y
    ----                ^
       9           PROCEDURE DIVISION.
      10               DISPLAY Z.
    ```cobol
    Z IN X
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    Redefines:
    ```cobol
    Y IN X
    ```
    (line 7, character 26):
    __rootdir__/prog.cob:8.25-8.26:
       5           WORKING-STORAGE SECTION.
       6           01 X.
       7             05 Y PIC 9.
       8 >           05 Z REDEFINES Y
    ----                            ^
       9           PROCEDURE DIVISION.
      10               DISPLAY Z.
    ```cobol
    Y IN X
    ```
    ```cobol
    PIC 9 USAGE DISPLAY
    ```
    NUMERIC(digits = 1, scale = 0, with_sign = false)
    *e.g,* [`0`] (0), [`1`] (1)
    (line 9, character 20):
    __rootdir__/prog.cob:10.20-10.21:
       7             05 Y PIC 9.
       8             05 Z REDEFINES Y
       9           PROCEDURE DIVISION.
      10 >             DISPLAY Z.
    ----                       ^
      11               STOP RUN.
      12
    ```cobol
    Z IN X
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    Redefines:
    ```cobol
    Y IN X
    ``` |}];;

let%expect_test "hover-typedef-table-and-index" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        77 IDX USA_|_GE IS INDEX.
        01 CNT PIC 99.
        01 T1_|_ PIC X OCCURS 10 TIMES INDEXED BY INDEX_|_1.
        01 T2 OCCURS 10 TIMES INDEXED BY IND_|_EX2,I_|_3.
          02 SUB-FIELD pic x.
        01 FILLER OCCURS 10 TO 20 TIMES DEPENDING CNT INDEXED BY I_|_4.
          02 SUB-FIELD pic 9.
        PROCEDURE DIVISION.
            SET INDEX1 TO IDX.
            MOVE T1 (IND_|_EX1) TO T2(1).
            STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 5, character 18):
    __rootdir__/prog.cob:6.8-6.30:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         77 IDX USAGE IS INDEX.
    ----           ^^^^^^^^^^^^^^^^^^^^^^
       7           01 CNT PIC 99.
       8           01 T1 PIC X OCCURS 10 TIMES INDEXED BY INDEX1.
    ```cobol
    IDX
    ```
    Index
    (line 7, character 13):
    __rootdir__/prog.cob:8.11-8.13:
       5           WORKING-STORAGE SECTION.
       6           77 IDX USAGE IS INDEX.
       7           01 CNT PIC 99.
       8 >         01 T1 PIC X OCCURS 10 TIMES INDEXED BY INDEX1.
    ----              ^^
       9           01 T2 OCCURS 10 TIMES INDEXED BY INDEX2,I3.
      10             02 SUB-FIELD pic x.
    ```cobol
    T1
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    (line 7, character 52):
    __rootdir__/prog.cob:8.47-8.53:
       5           WORKING-STORAGE SECTION.
       6           77 IDX USAGE IS INDEX.
       7           01 CNT PIC 99.
       8 >         01 T1 PIC X OCCURS 10 TIMES INDEXED BY INDEX1.
    ----                                                  ^^^^^^
       9           01 T2 OCCURS 10 TIMES INDEXED BY INDEX2,I3.
      10             02 SUB-FIELD pic x.
    Table
    ```cobol
    OCCURS 10 TIMES
    INDEXED BY INDEX1 IN T1
    ```
    Fields:
    ```cobol
    T1
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    (line 8, character 44):
    __rootdir__/prog.cob:9.41-9.47:
       6           77 IDX USAGE IS INDEX.
       7           01 CNT PIC 99.
       8           01 T1 PIC X OCCURS 10 TIMES INDEXED BY INDEX1.
       9 >         01 T2 OCCURS 10 TIMES INDEXED BY INDEX2,I3.
    ----                                            ^^^^^^
      10             02 SUB-FIELD pic x.
      11           01 FILLER OCCURS 10 TO 20 TIMES DEPENDING CNT INDEXED BY I4.
    Table
    ```cobol
    OCCURS 10 TIMES
    INDEXED BY INDEX2 IN T2, I3 IN T2
    ```
    Fields:
    ```cobol
    T2
    ```
    Group of 1 subfield
    Size: 8 bits
    (line 8, character 49):
    __rootdir__/prog.cob:9.48-9.50:
       6           77 IDX USAGE IS INDEX.
       7           01 CNT PIC 99.
       8           01 T1 PIC X OCCURS 10 TIMES INDEXED BY INDEX1.
       9 >         01 T2 OCCURS 10 TIMES INDEXED BY INDEX2,I3.
    ----                                                   ^^
      10             02 SUB-FIELD pic x.
      11           01 FILLER OCCURS 10 TO 20 TIMES DEPENDING CNT INDEXED BY I4.
    Table
    ```cobol
    OCCURS 10 TIMES
    INDEXED BY INDEX2 IN T2, I3 IN T2
    ```
    Fields:
    ```cobol
    T2
    ```
    Group of 1 subfield
    Size: 8 bits
    (line 10, character 66):
    __rootdir__/prog.cob:11.65-11.67:
       8           01 T1 PIC X OCCURS 10 TIMES INDEXED BY INDEX1.
       9           01 T2 OCCURS 10 TIMES INDEXED BY INDEX2,I3.
      10             02 SUB-FIELD pic x.
      11 >         01 FILLER OCCURS 10 TO 20 TIMES DEPENDING CNT INDEXED BY I4.
    ----                                                                    ^^
      12             02 SUB-FIELD pic 9.
      13           PROCEDURE DIVISION.
    Table
    ```cobol
    OCCURS 10 TO 20 TIMES DEPENDING ON CNT
    INDEXED BY I4
    ```
    Fields:
    ```cobol
    FILLER
    ```
    Group of 1 subfield
    Size: 8 bits
    (line 14, character 24):
    __rootdir__/prog.cob:15.21-15.27:
      12             02 SUB-FIELD pic 9.
      13           PROCEDURE DIVISION.
      14               SET INDEX1 TO IDX.
      15 >             MOVE T1 (INDEX1) TO T2(1).
    ----                        ^^^^^^
      16               STOP RUN.
      17
    Table
    ```cobol
    OCCURS 10 TIMES
    INDEXED BY INDEX1 IN T1
    ```
    Fields:
    ```cobol
    T1
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1) |}];;

let%expect_test "hover-typedef-communication-section" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 VAR PIC XX.
        COMMUNICATION SECTION.
        CD CM FOR INPUT
          STATUS K_|_EY IS STAT_|_US-KEY.
        PROCEDURE DIVISION.
            MOVE _|_STATUS-KEY TO VAR.
            STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 8, character 18):
    Hovering nothing worthy
    (line 8, character 28):
    Hovering nothing worthy
    (line 10, character 17):
    Hovering nothing worthy |}];;

let%expect_test "hover-comment" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
>>source format is fixed
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * full line comment
       01 STRUCT. *> inline comment
         02 VAL-1 PIC X. *> val1 only inline comment
      * val2 only line comment
         02 VAL-2 PIC X.
       PROCEDURE DIVISION.
         DISPLAY S_|_TRUCT V_|_AL-1 V_|_AL-2.
         STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 12, character 18):
    __rootdir__/prog.cob:13.17-13.23:
      10         * val2 only line comment
      11            02 VAL-2 PIC X.
      12          PROCEDURE DIVISION.
      13 >          DISPLAY STRUCT VAL-1 VAL-2.
    ----                    ^^^^^^
      14            STOP RUN.
      15
    ```cobol
    STRUCT
    ```
    Group of 2 subfields
    Size: 16 bits
    ---
     inline comment
    (line 12, character 25):
    __rootdir__/prog.cob:13.24-13.29:
      10         * val2 only line comment
      11            02 VAL-2 PIC X.
      12          PROCEDURE DIVISION.
      13 >          DISPLAY STRUCT VAL-1 VAL-2.
    ----                           ^^^^^
      14            STOP RUN.
      15
    ```cobol
    VAL-1 IN STRUCT
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    ---
     val1 only inline comment
    (line 12, character 31):
    __rootdir__/prog.cob:13.30-13.35:
      10         * val2 only line comment
      11            02 VAL-2 PIC X.
      12          PROCEDURE DIVISION.
      13 >          DISPLAY STRUCT VAL-1 VAL-2.
    ----                                 ^^^^^
      14            STOP RUN.
      15
    ```cobol
    VAL-2 IN STRUCT
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1)
    ---
     val2 only line comment |}];;


let%expect_test "hover-comment-copy" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  let server,    _ = add_cobol_doc server ~projdir "lib.cpy" {cobol|
      * copy full line comment
       01 FIELD PIC X. *> copy inline comment
  |cobol} in
  print_hovered server ~projdir @@ extract_position_markers {cobol|
  >>source format is fixed
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * full line comment
       COPY lib. *> inline comment
       PROCEDURE DIVISION.
          DISPLAY F_|_IELD
          STOP RUN.
    |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/lib.cpy appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib.cpy"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 9, character 19):
    __rootdir__/prog.cob:10.18-10.23:
       7         * full line comment
       8          COPY lib. *> inline comment
       9          PROCEDURE DIVISION.
      10 >           DISPLAY FIELD
    ----                     ^^^^^
      11             STOP RUN.
      12
    ```cobol
    FIELD
    ```
    ```cobol
    PIC X USAGE DISPLAY
    ```
    ALPHANUMERIC(1) |}]
