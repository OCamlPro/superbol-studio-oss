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


let print_references ~projdir server (doc, positions) : unit =
  let server, prog = add_cobol_doc server ~projdir "prog.cob" doc in
  let location_as_srcloc = new srcloc_resuscitator_cache in
  Pretty.out "@.";
  StringMap.iter begin fun position_name position ->
    let params =
      (*includeDeclaration*)
      let context = ReferenceContext.create ~includeDeclaration:true in
      ReferenceParams.create ~position ~textDocument:prog ~context ()
    in
    Pretty.out "%s (line %d, character %d):@."
      position_name position.line position.character;
    match LSP.Request.lookup_references server params with
    | None | Some [] ->
        Pretty.out "No reference found@."
    | Some locs ->
        List.iter location_as_srcloc#print locs
  end positions.pos_map
;;

let doc =
    extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 DATA-_|1-data-name-in-def|_NAME PI_|2-data-name-in-def|_C X.
        PROCEDURE DIVISION.
          DISPLAY _|3-data-name-in-display|_DATA-NAME
          DISPLAY _|4-data-name-in-display|_X.
          STOP RUN.
    |cobol}
;;


let%expect_test "simple-references-requests" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_references ~projdir server doc;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/superbol.toml"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    src/lsp/cobol_ast/raw_data_sections_visitor.ml:231:
      (Cobol_ast__Raw_data_sections_visitor.fold_data_clause): partial visitor
      implementation
    {"params":{"diagnostics":[{"message":"Source format `auto` is not supported yet, using `fixed`","range":{"end":{"character":0,"line":0},"start":{"character":0,"line":0}},"severity":2}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-data-name-in-def (line 5, character 16):
    __rootdir__/prog.cob:6.11-6.20:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^^^^^^^^^
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
    __rootdir__/prog.cob:8.18-8.27:
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8 >           DISPLAY DATA-NAME
    ----                     ^^^^^^^^^
       9             DISPLAY X.
      10             STOP RUN.
    2-data-name-in-def (line 5, character 23):
    __rootdir__/prog.cob:6.11-6.20:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^^^^^^^^^
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
    __rootdir__/prog.cob:8.18-8.27:
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8 >           DISPLAY DATA-NAME
    ----                     ^^^^^^^^^
       9             DISPLAY X.
      10             STOP RUN.
    3-data-name-in-display (line 7, character 18):
    __rootdir__/prog.cob:6.11-6.20:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^^^^^^^^^
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
    __rootdir__/prog.cob:8.18-8.27:
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8 >           DISPLAY DATA-NAME
    ----                     ^^^^^^^^^
       9             DISPLAY X.
      10             STOP RUN.
    4-data-name-in-display (line 8, character 18):
    No reference found |}]


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

let%expect_test "references-requests-renames" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_references ~projdir server doc;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/superbol.toml"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[{"message":"Source format `auto` is not supported yet, using `fixed`","range":{"end":{"character":0,"line":0},"start":{"character":0,"line":0}},"severity":2}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-data-name-renamed (line 7, character 25):
    __rootdir__/prog.cob:7.15-7.16:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 X.
       7 >             05 Y PIC 9.
    ----                  ^
       8               66 Z RENAMES Y.
       9           PROCEDURE DIVISION.
    __rootdir__/prog.cob:8.25-8.26:
       5           WORKING-STORAGE SECTION.
       6           01 X.
       7               05 Y PIC 9.
       8 >             66 Z RENAMES Y.
    ----                            ^
       9           PROCEDURE DIVISION.
      10               DISPLAY Z.
    2-data-name-in-display (line 9, character 20):
    __rootdir__/prog.cob:8.15-8.16:
       5           WORKING-STORAGE SECTION.
       6           01 X.
       7               05 Y PIC 9.
       8 >             66 Z RENAMES Y.
    ----                  ^
       9           PROCEDURE DIVISION.
      10               DISPLAY Z.
    __rootdir__/prog.cob:10.20-10.21:
       7               05 Y PIC 9.
       8               66 Z RENAMES Y.
       9           PROCEDURE DIVISION.
      10 >             DISPLAY Z.
    ----                       ^
      11               STOP RUN.
      12 |}]


let doc =
  extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 X.
            05 Y.
                10 Z PIC 999999.
            05 FILLER REDEFINES Y.
                10 A PIC 9 OCCURS 6 TIMES.
            05 STH REDEFINES Y.
                10 B PIC 99 OCCURS 3 TIMES.
            05 FILLER REDEFINES Y.
                10 C PIC 999 OCCURS 2 TIMES.
        PROCEDURE DIVISION.
            DISPLAY _|1-data-name-in-display|_Y.
  |cobol}
;;

let%expect_test "references-requests-redefines" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_references ~projdir server doc;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/superbol.toml"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[{"message":"Source format `auto` is not supported yet, using `fixed`","range":{"end":{"character":0,"line":0},"start":{"character":0,"line":0}},"severity":2}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-data-name-in-display (line 15, character 20):
    __rootdir__/prog.cob:7.15-7.16:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 X.
       7 >             05 Y.
    ----                  ^
       8                   10 Z PIC 999999.
       9               05 FILLER REDEFINES Y.
    __rootdir__/prog.cob:9.32-9.33:
       6           01 X.
       7               05 Y.
       8                   10 Z PIC 999999.
       9 >             05 FILLER REDEFINES Y.
    ----                                   ^
      10                   10 A PIC 9 OCCURS 6 TIMES.
      11               05 STH REDEFINES Y.
    __rootdir__/prog.cob:11.29-11.30:
       8                   10 Z PIC 999999.
       9               05 FILLER REDEFINES Y.
      10                   10 A PIC 9 OCCURS 6 TIMES.
      11 >             05 STH REDEFINES Y.
    ----                                ^
      12                   10 B PIC 99 OCCURS 3 TIMES.
      13               05 FILLER REDEFINES Y.
    __rootdir__/prog.cob:13.32-13.33:
      10                   10 A PIC 9 OCCURS 6 TIMES.
      11               05 STH REDEFINES Y.
      12                   10 B PIC 99 OCCURS 3 TIMES.
      13 >             05 FILLER REDEFINES Y.
    ----                                   ^
      14                   10 C PIC 999 OCCURS 2 TIMES.
      15           PROCEDURE DIVISION.
    __rootdir__/prog.cob:16.20-16.21:
      13               05 FILLER REDEFINES Y.
      14                   10 C PIC 999 OCCURS 2 TIMES.
      15           PROCEDURE DIVISION.
      16 >             DISPLAY Y.
    ----                       ^
      17 |}]


let doc =
    extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 FILLER.
          05 X PIC 999.
          05 FILLER REDEFINES X.
            10 Z PIC 9 OCCURS 3 TIMES.
          66 Y RENAMES X.
        PROCEDURE DIVISION.
          DISPLAY _|1-data-name-in-display|_X .
          MOVE 1 TO X.
          STOP RUN.
    |cobol}
;;

let%expect_test "references-requests-filler" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  print_references ~projdir server doc;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/superbol.toml"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[{"message":"Source format `auto` is not supported yet, using `fixed`","range":{"end":{"character":0,"line":0},"start":{"character":0,"line":0}},"severity":2}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    1-data-name-in-display (line 11, character 18):
    __rootdir__/prog.cob:7.13-7.14:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 FILLER.
       7 >           05 X PIC 999.
    ----                ^
       8             05 FILLER REDEFINES X.
       9               10 Z PIC 9 OCCURS 3 TIMES.
    __rootdir__/prog.cob:8.30-8.31:
       5           WORKING-STORAGE SECTION.
       6           01 FILLER.
       7             05 X PIC 999.
       8 >           05 FILLER REDEFINES X.
    ----                                 ^
       9               10 Z PIC 9 OCCURS 3 TIMES.
      10             66 Y RENAMES X.
    __rootdir__/prog.cob:10.23-10.24:
       7             05 X PIC 999.
       8             05 FILLER REDEFINES X.
       9               10 Z PIC 9 OCCURS 3 TIMES.
      10 >           66 Y RENAMES X.
    ----                          ^
      11           PROCEDURE DIVISION.
      12             DISPLAY X .
    __rootdir__/prog.cob:12.18-12.19:
       9               10 Z PIC 9 OCCURS 3 TIMES.
      10             66 Y RENAMES X.
      11           PROCEDURE DIVISION.
      12 >           DISPLAY X .
    ----                     ^
      13             MOVE 1 TO X.
      14             STOP RUN.
    __rootdir__/prog.cob:13.20-13.21:
      10             66 Y RENAMES X.
      11           PROCEDURE DIVISION.
      12             DISPLAY X .
      13 >           MOVE 1 TO X.
    ----                       ^
      14             STOP RUN.
      15 |}]
