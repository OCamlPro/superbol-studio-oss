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


let inspect_positions (doc, positions) : string -> unit =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  let server, prog = add_cobol_doc server ~projdir "prog.cob" doc in
  let doc = LSP.Server.find_document prog server in
  let location_as_srcloc = new srcloc_resuscitator_cache in
  let inspect ?position_name (position: Position.t) =
    let location =
      let range = Range.create ~start:position ~end_:position in
      Location.create ~range ~uri:prog.uri
    in
    Pretty.error "%a%a(line %d, character %d): "
      location_as_srcloc#pp location
      Fmt.(option ~none:nop (string ++ sp)) position_name
      position.line position.character;
    match LSP.Document.inspect_at ~position doc with
    | None ->
        Pretty.error "failed inspection@."
    | Some Sink ->
        Pretty.error "accepting state@."
    | Some Env _ ->
        Pretty.error "transitory state@."
  in
  StringMap.iter (fun n p -> inspect ~position_name:n p) positions.pos_map;
  List.iter (fun p -> inspect p) positions.pos_anonymous;
  end_with_postproc
;;


let%expect_test "basic-inspection" =
  let gime_out = inspect_positions @@ extract_position_markers {cobol|
        _|_IDENTIFICATION D_|_IVISION_|_._|_
        PROGRAM_|_-ID. prog.
        DATA DIV_|_ISION.
        WORKING-STORAGE _|_SECTION.
        01 _|_DATA-NAME _|_PIC X.
        _|_PROCEDU_|_RE _|_DIVISION.
          _|_DISPLAY _|_DATA-NAME
          _|_STOP _|_RUN._|_
  |cobol} in
  gime_out [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    __rootdir__/prog.cob:2.8:
       1
       2 >         IDENTIFICATION DIVISION.
    ----           ^
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
    (line 1, character 8): transitory state
    __rootdir__/prog.cob:2.24:
       1
       2 >         IDENTIFICATION DIVISION.
    ----                           ^
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
    (line 1, character 24): accepting state
    __rootdir__/prog.cob:2.31:
       1
       2 >         IDENTIFICATION DIVISION.
    ----                                  ^
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
    (line 1, character 31): transitory state
    __rootdir__/prog.cob:2.32:
       1
       2 >         IDENTIFICATION DIVISION.
    ----                                   ^
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
    (line 1, character 32): transitory state
    __rootdir__/prog.cob:3.15:
       1
       2           IDENTIFICATION DIVISION.
       3 >         PROGRAM-ID. prog.
    ----                  ^
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
    (line 2, character 15): accepting state
    __rootdir__/prog.cob:4.16:
       1
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID. prog.
       4 >         DATA DIVISION.
    ----                   ^
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
    (line 3, character 16): accepting state
    __rootdir__/prog.cob:5.24:
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5 >         WORKING-STORAGE SECTION.
    ----                           ^
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
    (line 4, character 24): transitory state
    __rootdir__/prog.cob:6.11:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
    (line 5, character 11): transitory state
    __rootdir__/prog.cob:6.21:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----                        ^
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
    (line 5, character 21): transitory state
    __rootdir__/prog.cob:7.8:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         PROCEDURE DIVISION.
    ----           ^
       8             DISPLAY DATA-NAME
       9             STOP RUN.
    (line 6, character 8): transitory state
    __rootdir__/prog.cob:7.15:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         PROCEDURE DIVISION.
    ----                  ^
       8             DISPLAY DATA-NAME
       9             STOP RUN.
    (line 6, character 15): accepting state
    __rootdir__/prog.cob:7.18:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         PROCEDURE DIVISION.
    ----                     ^
       8             DISPLAY DATA-NAME
       9             STOP RUN.
    (line 6, character 18): transitory state
    __rootdir__/prog.cob:8.10:
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8 >           DISPLAY DATA-NAME
    ----             ^
       9             STOP RUN.
      10
    (line 7, character 10): transitory state
    __rootdir__/prog.cob:8.18:
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8 >           DISPLAY DATA-NAME
    ----                     ^
       9             STOP RUN.
      10
    (line 7, character 18): transitory state
    __rootdir__/prog.cob:9.10:
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
       9 >           STOP RUN.
    ----             ^
      10
    (line 8, character 10): transitory state
    __rootdir__/prog.cob:9.15:
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
       9 >           STOP RUN.
    ----                  ^
      10
    (line 8, character 15): transitory state
    __rootdir__/prog.cob:9.19:
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
       9 >           STOP RUN.
    ----                      ^
      10
    (line 8, character 19): transitory state |}];;
