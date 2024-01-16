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

open EzCompat                                                    (* StringMap *)
open Lsp.Types
open Lsp_testing


let print_definitions ~projdir server (doc, positions) : unit =
  let server, prog = add_cobol_doc server ~projdir "prog.cob" doc in
  let location_as_srcloc = new srcloc_resuscitator_cache in
  StringMap.iter begin fun position_name position ->
    let params = DefinitionParams.create ~position ~textDocument:prog () in
    Pretty.out "%s (line %d, character %d):@."
      position_name position.line position.character;
    match LSP.Request.lookup_definition server params with
    | None | Some (`Location []) ->
        Pretty.out "No definition found@."
    | Some (`Location locs) ->
        List.iter location_as_srcloc#print locs
    (* Yojson.Safe.to_channel Stdlib.stdout @@ *)
    (* Lsp.Client_request.yojson_of_result *)
    (*   (Lsp.Client_request.TextDocumentDefinition params) *)
    (*   (LSP.Request.lookup_definition server params); *)
  end positions.pos_map
;;


let doc =
    extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 DATA-_|1-data-name-in-def|_NAME PIC X.
        PROCEDURE DIVISION.
          DISPLAY _|2-data-name-in-display|_DATA-NAME
          STOP RUN.
    |cobol}
;;

let%expect_test "simple-definition-requests" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server doc;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-data-name-in-def (line 5, character 16):
    __rootdir__/prog.cob:6.11-6.20:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^^^^^^^^^
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
    2-data-name-in-display (line 7, character 18):
    __rootdir__/prog.cob:6.11-6.20:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^^^^^^^^^
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME |}]


let doc =
    extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        0_|1-data-name-in-def|_1 DATA-_|2-data-name-in-def|_NAME P_|3-data-name-in-def|_IC X.
        PROCEDURE DIVISION.
          DISPLAY DATA-NAME
          STOP RUN.
    |cobol}
;;

let%expect_test "simple-definition-requests-2" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server doc;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-data-name-in-def (line 5, character 9):
    __rootdir__/prog.cob:6.11-6.20:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^^^^^^^^^
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
    2-data-name-in-def (line 5, character 16):
    __rootdir__/prog.cob:6.11-6.20:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^^^^^^^^^
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
    3-data-name-in-def (line 5, character 22):
    __rootdir__/prog.cob:6.11-6.20:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^^^^^^^^^
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME |}]



let%expect_test "simple-definition-requests-3" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 X.
            05 Y PIC 9.
        PROCEDURE DIVISION.
            DISPLAY _|1-data-name-in-display|_Y of _|2-data-name-in-display|_X.
            STOP RUN.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-data-name-in-display (line 8, character 20):
    __rootdir__/prog.cob:7.15-7.16:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 X.
       7 >             05 Y PIC 9.
    ----                  ^
       8           PROCEDURE DIVISION.
       9               DISPLAY Y of X.
    2-data-name-in-display (line 8, character 25):
    __rootdir__/prog.cob:6.11-6.12:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 X.
    ----              ^
       7               05 Y PIC 9.
       8           PROCEDURE DIVISION. |}]


let doc =
  extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 X.
            05 Y PIC 9.
            66 Z R_|3-in-renames-item|_ENAMES _|1-data-name-renamed|_Y.
        PROCEDURE DIVISION.
            DISPLAY _|2-data-name-in-display|_Z.
            STOP RUN.
  |cobol}
;;

let%expect_test "definition-requests-renames" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server doc;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-data-name-renamed (line 7, character 25):
    __rootdir__/prog.cob:7.15-7.16:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 X.
       7 >             05 Y PIC 9.
    ----                  ^
       8               66 Z RENAMES Y.
       9           PROCEDURE DIVISION.
    2-data-name-in-display (line 9, character 20):
    __rootdir__/prog.cob:8.15-8.16:
       5           WORKING-STORAGE SECTION.
       6           01 X.
       7               05 Y PIC 9.
       8 >             66 Z RENAMES Y.
    ----                  ^
       9           PROCEDURE DIVISION.
      10               DISPLAY Z.
    3-in-renames-item (line 7, character 18):
    __rootdir__/prog.cob:8.15-8.16:
       5           WORKING-STORAGE SECTION.
       6           01 X.
       7               05 Y PIC 9.
       8 >             66 Z RENAMES Y.
    ----                  ^
       9           PROCEDURE DIVISION.
      10               DISPLAY Z. |}]


let doc =
  extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 Y PIC XXX.
        01 X.
            05 Y.
                10 Z PIC 999999.
            05 FILLER REDEFINES _|1-data-name-redefined|_Y.
                10 A PIC 9 OCCURS 6 TIMES.
            05 STH REDEFINES _|2-data-name-redefined|_Y.
                10 B PIC 99 OCCURS 3 TIMES.
            05 FIL_|4-data-name-redefined|_LER REDEFINES _|3-data-name-redefined|_Y.
                10 C PIC 999 OCCURS 2 TIMES.
        PROCEDURE DIVISION.
            DISPLAY _|5-data-name-in-display|_A.
  |cobol}
;;

let%expect_test "definition-requests-redefines" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server doc;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-data-name-redefined (line 9, character 32):
    __rootdir__/prog.cob:8.15-8.16:
       5           WORKING-STORAGE SECTION.
       6           01 Y PIC XXX.
       7           01 X.
       8 >             05 Y.
    ----                  ^
       9                   10 Z PIC 999999.
      10               05 FILLER REDEFINES Y.
    2-data-name-redefined (line 11, character 29):
    __rootdir__/prog.cob:8.15-8.16:
       5           WORKING-STORAGE SECTION.
       6           01 Y PIC XXX.
       7           01 X.
       8 >             05 Y.
    ----                  ^
       9                   10 Z PIC 999999.
      10               05 FILLER REDEFINES Y.
    3-data-name-redefined (line 13, character 32):
    __rootdir__/prog.cob:8.15-8.16:
       5           WORKING-STORAGE SECTION.
       6           01 Y PIC XXX.
       7           01 X.
       8 >             05 Y.
    ----                  ^
       9                   10 Z PIC 999999.
      10               05 FILLER REDEFINES Y.
    4-data-name-redefined (line 13, character 18):
    __rootdir__/prog.cob:14.12-15.44:
      11                   10 A PIC 9 OCCURS 6 TIMES.
      12               05 STH REDEFINES Y.
      13                   10 B PIC 99 OCCURS 3 TIMES.
      14 >             05 FILLER REDEFINES Y.
    ----               ^^^^^^^^^^^^^^^^^^^^^^
      15 >                 10 C PIC 999 OCCURS 2 TIMES.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      16           PROCEDURE DIVISION.
      17               DISPLAY A.
    5-data-name-in-display (line 16, character 20):
    __rootdir__/prog.cob:11.19-11.20:
       8               05 Y.
       9                   10 Z PIC 999999.
      10               05 FILLER REDEFINES Y.
      11 >                 10 A PIC 9 OCCURS 6 TIMES.
    ----                      ^
      12               05 STH REDEFINES Y.
      13                   10 B PIC 99 OCCURS 3 TIMES. |}]


let doc =
    extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01.
          05 H1 PIC 999.
          05.
            10 _|1-data-name-in-def|_H PIC 999.
          05 H2 PIC 999.
        01 X.
          05 W PIC 999.
          05 FILLER.
            10 _|2-data-name-in-def|_Z PIC 999.
        01.
          05 _|3-data-name-in-def|_T PIC 999.
        PROCEDURE DIVISION.
          DISPLAY _|4-data-name-in-display|_Z OF X.
          STOP RUN.
    |cobol}
;;

let%expect_test "definition-requests-filler" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server doc;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-data-name-in-def (line 8, character 15):
    __rootdir__/prog.cob:9.15-9.16:
       6           01.
       7             05 H1 PIC 999.
       8             05.
       9 >             10 H PIC 999.
    ----                  ^
      10             05 H2 PIC 999.
      11           01 X.
    2-data-name-in-def (line 13, character 15):
    __rootdir__/prog.cob:14.15-14.16:
      11           01 X.
      12             05 W PIC 999.
      13             05 FILLER.
      14 >             10 Z PIC 999.
    ----                  ^
      15           01.
      16             05 T PIC 999.
    3-data-name-in-def (line 15, character 13):
    __rootdir__/prog.cob:16.13-16.14:
      13             05 FILLER.
      14               10 Z PIC 999.
      15           01.
      16 >           05 T PIC 999.
    ----                ^
      17           PROCEDURE DIVISION.
      18             DISPLAY Z OF X.
    4-data-name-in-display (line 17, character 18):
    __rootdir__/prog.cob:14.15-14.16:
      11           01 X.
      12             05 W PIC 999.
      13             05 FILLER.
      14 >             10 Z PIC 999.
    ----                  ^
      15           01.
      16             05 T PIC 999. |}]


let%expect_test "definition-requests-subscripted-1" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server @@ extract_position_markers {cobol|
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AA OCCURS 5.
         02 BB PIC X.
       77 TMP PIC X.
       PROCEDURE DIVISION.
           DISPLAY B_|1-bb|_B (TMP).
           MOVE _|2-bb|_BB (T_|3-tmp|_MP _|4-nowhere|_)_|5-nowhere|_ TO TMP.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-bb (line 8, character 20):
    __rootdir__/prog.cob:6.12-6.14:
       3          DATA DIVISION.
       4          WORKING-STORAGE SECTION.
       5          01 AA OCCURS 5.
       6 >          02 BB PIC X.
    ----               ^^
       7          77 TMP PIC X.
       8          PROCEDURE DIVISION.
    2-bb (line 9, character 16):
    __rootdir__/prog.cob:6.12-6.14:
       3          DATA DIVISION.
       4          WORKING-STORAGE SECTION.
       5          01 AA OCCURS 5.
       6 >          02 BB PIC X.
    ----               ^^
       7          77 TMP PIC X.
       8          PROCEDURE DIVISION.
    3-tmp (line 9, character 21):
    __rootdir__/prog.cob:7.10-7.13:
       4          WORKING-STORAGE SECTION.
       5          01 AA OCCURS 5.
       6            02 BB PIC X.
       7 >        77 TMP PIC X.
    ----             ^^^
       8          PROCEDURE DIVISION.
       9              DISPLAY BB (TMP).
    4-nowhere (line 9, character 24):
    No definition found
    5-nowhere (line 9, character 25):
    No definition found |}]


let%expect_test "definition-requests-refmod" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server @@ extract_position_markers {cobol|
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BB OCCURS 5 PIC 9.
       77 TMP PIC X.
       PROCEDURE DIVISION.
           DISPLAY B_|1-bb|_B (TMP_|2-tmp|_:_|3-nowhere|_).
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-bb (line 7, character 20):
    __rootdir__/prog.cob:5.10-5.12:
       2          PROGRAM-ID. prog.
       3          DATA DIVISION.
       4          WORKING-STORAGE SECTION.
       5 >        01 BB OCCURS 5 PIC 9.
    ----             ^^
       6          77 TMP PIC X.
       7          PROCEDURE DIVISION.
    2-tmp (line 7, character 26):
    __rootdir__/prog.cob:6.10-6.13:
       3          DATA DIVISION.
       4          WORKING-STORAGE SECTION.
       5          01 BB OCCURS 5 PIC 9.
       6 >        77 TMP PIC X.
    ----             ^^^
       7          PROCEDURE DIVISION.
       8              DISPLAY BB (TMP:).
    3-nowhere (line 7, character 27):
    No definition found |}]


let%expect_test "definition-requests-goto-section" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server @@ extract_position_markers {cobol|
       PROGRAM-ID. prog.
       PROCEDURE DIVISI_|1-nowhere|_ON.
       MAIN SECTION.
           GO TO SUB-SE_|1-sub-section|_CTION.
       SUB-S_|2-sub-section|_ECTION SECTION.
           STOP R_|3-nowhere|_UN.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-nowhere (line 2, character 23):
    No definition found
    1-sub-section (line 4, character 23):
    __rootdir__/prog.cob:6.7-6.18:
       3          PROCEDURE DIVISION.
       4          MAIN SECTION.
       5              GO TO SUB-SECTION.
       6 >        SUB-SECTION SECTION.
    ----          ^^^^^^^^^^^
       7              STOP RUN.
       8
    2-sub-section (line 5, character 12):
    __rootdir__/prog.cob:6.7-6.18:
       3          PROCEDURE DIVISION.
       4          MAIN SECTION.
       5              GO TO SUB-SECTION.
       6 >        SUB-SECTION SECTION.
    ----          ^^^^^^^^^^^
       7              STOP RUN.
       8
    3-nowhere (line 6, character 17):
    No definition found |}];;


let%expect_test "definition-requests-goto-qualified-section/paragraphs" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server @@ extract_position_markers {cobol|
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
       MAIN SECTION.
           GO TO SUB-SE_|1-sub-section|_CTION IN _|2-main|_MAIN.
       SUB-SECTION_|3-sub-section|_.
           STOP RUN.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-sub-section (line 4, character 23):
    __rootdir__/prog.cob:6.7-6.18:
       3          PROCEDURE DIVISION.
       4          MAIN SECTION.
       5              GO TO SUB-SECTION IN MAIN.
       6 >        SUB-SECTION.
    ----          ^^^^^^^^^^^
       7              STOP RUN.
       8
    2-main (line 4, character 32):
    __rootdir__/prog.cob:4.7-4.11:
       1
       2          PROGRAM-ID. prog.
       3          PROCEDURE DIVISION.
       4 >        MAIN SECTION.
    ----          ^^^^
       5              GO TO SUB-SECTION IN MAIN.
       6          SUB-SECTION.
    3-sub-section (line 5, character 18):
    __rootdir__/prog.cob:6.7-6.18:
       3          PROCEDURE DIVISION.
       4          MAIN SECTION.
       5              GO TO SUB-SECTION IN MAIN.
       6 >        SUB-SECTION.
    ----          ^^^^^^^^^^^
       7              STOP RUN.
       8 |}];;

let%expect_test "definition-ambiguous-section/paragraphs" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server @@ extract_position_markers {cobol|
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
       MAIN SECTION.
          PERFORM SU_|_B-1.
       MAIN-1 SECTION.
       SUB-1.
          DISPLAY 1.
       MAIN-2 SECTION.
       SUB-1.
          DISPLAY 2.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {| {"params":{"diagnostics":[{"message":"Ambiguous procedure-name 'SUB-1'; known matching names are 'SUB-1 IN MAIN-2', 'SUB-1 IN MAIN-1'","range":{"end":{"character":23,"line":4},"start":{"character":18,"line":4}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;



let%expect_test "definition-malformed-qualifiers" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server @@ extract_position_markers {cobol|
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
       MAIN SECTION.
           GO TO _|1-a|_A IN
           PERFORM _|1-s|_S IN
           PERFORM _|2-s|_S IN_|3-s|_ IN M_|4-main|_AIN
           PERFORM _|5-s|_IN S_|6-s|_.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[{"message":"Unknown procedure-name 'S'","range":{"end":{"character":23,"line":7},"start":{"character":22,"line":7}},"severity":1},{"message":"Unknown procedure-name 'S IN MAIN'","range":{"end":{"character":31,"line":6},"start":{"character":19,"line":6}},"severity":1},{"message":"Unknown procedure-name 'S'","range":{"end":{"character":23,"line":5},"start":{"character":19,"line":5}},"severity":1},{"message":"Unknown procedure-name 'A'","range":{"end":{"character":21,"line":4},"start":{"character":17,"line":4}},"severity":1},{"message":"Invalid syntax","range":{"end":{"character":21,"line":7},"start":{"character":19,"line":7}},"severity":1},{"message":"Invalid syntax","range":{"end":{"character":26,"line":6},"start":{"character":24,"line":6}},"severity":1},{"message":"Invalid syntax","range":{"end":{"character":18,"line":6},"start":{"character":11,"line":6}},"severity":1},{"message":"Missing <qualified name>","range":{"end":{"character":23,"line":5},"start":{"character":23,"line":5}},"severity":4},{"message":"Invalid syntax","range":{"end":{"character":18,"line":5},"start":{"character":11,"line":5}},"severity":1},{"message":"Missing <qualified name>","range":{"end":{"character":21,"line":4},"start":{"character":21,"line":4}},"severity":4}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-a (line 4, character 17):
    {"params":{"message":"Unknown procedure-name 'A'","type":2},"method":"window/showMessage","jsonrpc":"2.0"}
    No definition found
    1-s (line 5, character 19):
    {"params":{"message":"Unknown procedure-name 'S'","type":2},"method":"window/showMessage","jsonrpc":"2.0"}
    No definition found
    2-s (line 6, character 19):
    {"params":{"message":"Unknown procedure-name 'S IN MAIN'","type":2},"method":"window/showMessage","jsonrpc":"2.0"}
    No definition found
    3-s (line 6, character 23):
    __rootdir__/prog.cob:4.7-4.11:
       1
       2          PROGRAM-ID. prog.
       3          PROCEDURE DIVISION.
       4 >        MAIN SECTION.
    ----          ^^^^
       5              GO TO A IN
       6              PERFORM S IN
    4-main (line 6, character 28):
    __rootdir__/prog.cob:4.7-4.11:
       1
       2          PROGRAM-ID. prog.
       3          PROCEDURE DIVISION.
       4 >        MAIN SECTION.
    ----          ^^^^
       5              GO TO A IN
       6              PERFORM S IN
    5-s (line 7, character 19):
    No definition found
    6-s (line 7, character 23):
    {"params":{"message":"Unknown procedure-name 'S'","type":2},"method":"window/showMessage","jsonrpc":"2.0"}
    No definition found |}];;



let%expect_test "definition-index" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server @@ extract_position_markers {cobol|
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 V-TAB PIC X OCCURS 5 INDEXED I _|1-j|_J.
       01 W.
         02 W-TAB PIC 9 OCCURS 42 INDEXED J_|2-j|_ K.
       PROCEDURE DIVISION.
           SET _|3-i|_I IN V-TAB TO 0
           SET _|4-j|_J IN W TO 0
           SET _|5-k|_K IN W-TAB IN W TO 0
           SET _|6-missing|_L IN W-TAB IN W TO 0
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[{"message":"Missing .","range":{"end":{"character":35,"line":11},"start":{"character":35,"line":11}},"severity":4}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-j (line 4, character 41):
    __rootdir__/prog.cob:5.41-5.42:
       2          PROGRAM-ID. prog.
       3          DATA DIVISION.
       4          WORKING-STORAGE SECTION.
       5 >        77 V-TAB PIC X OCCURS 5 INDEXED I J.
    ----                                            ^
       6          01 W.
       7            02 W-TAB PIC 9 OCCURS 42 INDEXED J K.
    2-j (line 6, character 43):
    __rootdir__/prog.cob:7.42-7.43:
       4          WORKING-STORAGE SECTION.
       5          77 V-TAB PIC X OCCURS 5 INDEXED I J.
       6          01 W.
       7 >          02 W-TAB PIC 9 OCCURS 42 INDEXED J K.
    ----                                             ^
       8          PROCEDURE DIVISION.
       9              SET I IN V-TAB TO 0
    3-i (line 8, character 15):
    __rootdir__/prog.cob:5.39-5.40:
       2          PROGRAM-ID. prog.
       3          DATA DIVISION.
       4          WORKING-STORAGE SECTION.
       5 >        77 V-TAB PIC X OCCURS 5 INDEXED I J.
    ----                                          ^
       6          01 W.
       7            02 W-TAB PIC 9 OCCURS 42 INDEXED J K.
    4-j (line 9, character 15):
    __rootdir__/prog.cob:7.42-7.43:
       4          WORKING-STORAGE SECTION.
       5          77 V-TAB PIC X OCCURS 5 INDEXED I J.
       6          01 W.
       7 >          02 W-TAB PIC 9 OCCURS 42 INDEXED J K.
    ----                                             ^
       8          PROCEDURE DIVISION.
       9              SET I IN V-TAB TO 0
    5-k (line 10, character 15):
    __rootdir__/prog.cob:7.44-7.45:
       4          WORKING-STORAGE SECTION.
       5          77 V-TAB PIC X OCCURS 5 INDEXED I J.
       6          01 W.
       7 >          02 W-TAB PIC 9 OCCURS 42 INDEXED J K.
    ----                                               ^
       8          PROCEDURE DIVISION.
       9              SET I IN V-TAB TO 0
    6-missing (line 11, character 15):
    No definition found |}];;
