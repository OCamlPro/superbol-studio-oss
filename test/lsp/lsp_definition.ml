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
    data_sections_visitor.ml:0:
      (Cobol_ptree__Data_sections_visitor.fold_data_clause): partial visitor
      implementation
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



let doc =
  extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 X.
            05 Y PIC 9.
        PROCEDURE DIVISION.
            DISPLAY _|1-data-name-in-display|_Y of _|2-data-name-in-display|_X.
            STOP RUN.
  |cobol}
;;

let%expect_test "simple-definition-requests-2" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_definitions ~projdir server doc;
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
            66 Z RENAMES _|1-data-name-renamed|_Y.
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
            05 FILLER REDEFINES _|3-data-name-redefined|_Y.
                10 C PIC 999 OCCURS 2 TIMES.
        PROCEDURE DIVISION.
            DISPLAY _|4-data-name-in-display|_A.
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
    4-data-name-in-display (line 16, character 20):
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
