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
    match LSP.Request.hover server params with
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

let lib = {cobol|
       01 FIELD PIC X.
  |cobol};;

let prog =
  extract_position_markers {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       _|_COP_|_Y "_|_li_|_b.cpy".
       PROCEDURE DIVISION.
          DISPLAY FI_|_ELD
          STOP RUN.
  |cobol};;

let%expect_test "hover-copy" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  let server,    _ = add_cobol_doc server ~projdir "lib.cpy" lib in
  print_hovered server ~projdir prog;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/superbol.toml"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib.cpy"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    data_sections_visitor.ml:0:
      (Cobol_ptree__Data_sections_visitor.fold_data_clause): partial visitor
      implementation
    {"params":{"diagnostics":[{"message":"Source format `auto` is not supported yet, using `fixed`","range":{"end":{"character":0,"line":0},"start":{"character":0,"line":0}},"severity":2}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
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
           01 FIELD PIC X.
    ```
    (line 7, character 20):
    Hovering nothing worthy |}];;

(* Hover replaced *)

let prog =
  extract_position_markers {cobol|
       REPLACE =="A"== BY =="B" "C"==.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
          DISPLAY "_|_A"
          STOP RUN.
  |cobol};;

let%expect_test "hover-replaced" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  print_hovered server ~projdir prog;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/superbol.toml"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[{"message":"Source format `auto` is not supported yet, using `fixed`","range":{"end":{"character":0,"line":0},"start":{"character":0,"line":0}},"severity":2}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 5, character 19):
    __rootdir__/prog.cob:6.18-6.21:
       3          IDENTIFICATION DIVISION.
       4          PROGRAM-ID. prog.
       5          PROCEDURE DIVISION.
       6 >           DISPLAY "A"
    ----                     ^^^
       7             STOP RUN.
       8
    ``"B" "C"`` |}];;
