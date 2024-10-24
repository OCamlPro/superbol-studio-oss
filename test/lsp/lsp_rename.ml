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

let pp_assoc_elem ppf ((uri, edits): DocumentUri.t * TextEdit.t list) =
  let pp_range ppf range =
  let location_as_srcloc = new srcloc_resuscitator_cache in
  location_as_srcloc#pp ppf @@ Location.create ~range ~uri
  in
  let pp_edit ppf (edit: TextEdit.t) =
    Fmt.pf ppf "%s at " edit.newText;
    pp_range ppf edit.range
  in
  Fmt.(list pp_edit) ppf edits

let count l =
  List.fold_left begin fun acc (_, t) -> acc + List.length t end 0 l

let rename_positions ?(abort_when_in_copybook = false) ?(copybooks = [])
    (doc, positions) : string -> unit =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  let server =
    List.fold_left begin fun server (name, document) ->
      fst @@ add_cobol_doc server ~projdir name document
    end server copybooks
  in
  let server, prog = add_cobol_doc server ~projdir "prog.cob" doc in
  let rename_at_position ?key (position: Position.t) =
    let params =
      RenameParams.create ~newName:"aNewName" ~position ~textDocument:prog ()
    in
    Pretty.out "%a(line %d, character %d):\n"
      Fmt.(option ~none:nop (string ++ sp)) key
      position.line position.character;
    match LSP.Request.rename ~abort_when_in_copybook server params with
    | { changes = None; _ } ->
      Pretty.out "No renames@."
    | { changes = Some assoc; _ } ->
      Pretty.out "@.@[<hv 4>%d rename entries:@;%a@]@\n"
        (count assoc)
        (Fmt.list ~sep:Fmt.sp pp_assoc_elem) assoc
  in
  StringMap.iter (fun n p -> rename_at_position ~key:n p) positions.pos_map;
  List.iter (fun p -> rename_at_position p) positions.pos_anonymous;
  end_with_postproc

let%expect_test "rename" =
  let end_with_postproc = rename_positions @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 O_|_LD PIC 9.
        PROCEDURE DIVISION.
          MOVE 1 TO old_|_.
          S_|_TOP RUN.
  |cobol}
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 5, character 12):
    2 rename entries:
        aNewName at __rootdir__/prog.cob:8.20-8.23:
           5           WORKING-STORAGE SECTION.
       6           01 OLD PIC 9.
       7           PROCEDURE DIVISION.
       8 >           MOVE 1 TO old.
    ----                       ^^^
       9             STOP RUN.
      10
    aNewName at __rootdir__/prog.cob:6.11-6.14:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 OLD PIC 9.
    ----              ^^^
       7           PROCEDURE DIVISION.
       8             MOVE 1 TO old.
    (line 7, character 23):
    2 rename entries:
        aNewName at __rootdir__/prog.cob:8.20-8.23:
           5           WORKING-STORAGE SECTION.
       6           01 OLD PIC 9.
       7           PROCEDURE DIVISION.
       8 >           MOVE 1 TO old.
    ----                       ^^^
       9             STOP RUN.
      10
    aNewName at __rootdir__/prog.cob:6.11-6.14:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 OLD PIC 9.
    ----              ^^^
       7           PROCEDURE DIVISION.
       8             MOVE 1 TO old.
    (line 8, character 11):
    0 rename entries: |}]

let%expect_test "rename-group" =
  let end_with_postproc = rename_positions @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 OLD.
          02 CHILD PIC 9.
        PROCEDURE DIVISION.
          DISPLAY _|_child in old_|_.
          STOP RUN.
  |cobol}
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 8, character 18):
    2 rename entries:
        aNewName at __rootdir__/prog.cob:9.18-9.23:
           6           01 OLD.
       7             02 CHILD PIC 9.
       8           PROCEDURE DIVISION.
       9 >           DISPLAY child in old.
    ----                     ^^^^^
      10             STOP RUN.
      11
    aNewName at __rootdir__/prog.cob:7.13-7.18:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 OLD.
       7 >           02 CHILD PIC 9.
    ----                ^^^^^
       8           PROCEDURE DIVISION.
       9             DISPLAY child in old.
    (line 8, character 30):
    2 rename entries:
        aNewName at __rootdir__/prog.cob:9.27-9.30:
           6           01 OLD.
       7             02 CHILD PIC 9.
       8           PROCEDURE DIVISION.
       9 >           DISPLAY child in old.
    ----                              ^^^
      10             STOP RUN.
      11
    aNewName at __rootdir__/prog.cob:6.11-6.14:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 OLD.
    ----              ^^^
       7             02 CHILD PIC 9.
       8           PROCEDURE DIVISION. |}]

let%expect_test "rename-with-a-ref-in-a-copybook" =
  let copybooks = [
    ("lib.cpy", {cobol|
        01 copied-var pic 9.|cobol})
  ] in
  let end_with_postproc = rename_positions ~copybooks @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY "lib.cpy".
        PROCEDURE DIVISION.
          MOVE 1 TO c_|_opied-var.
          STOP RUN.
  |cobol}
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/lib.cpy appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib.cpy"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"message":"Renamed reference that occurs in a copybook","type":2},"method":"window/showMessage","jsonrpc":"2.0"}
    (line 7, character 21):
    2 rename entries:
        aNewName at __rootdir__/lib.cpy:2.11-2.21:
           1
       2 >         01 copied-var pic 9.
    ----              ^^^^^^^^^^
    aNewName at __rootdir__/prog.cob:8.20-8.30:
       5           WORKING-STORAGE SECTION.
       6           COPY "lib.cpy".
       7           PROCEDURE DIVISION.
       8 >           MOVE 1 TO copied-var.
    ----                       ^^^^^^^^^^
       9             STOP RUN.
      10 |}]

let%expect_test "rename-with-a-ignored-ref-in-a-copybook" =
  let copybooks = [
    ("lib.cpy", {cobol|
        01 copied-var pic 9.|cobol})
  ] in
  let end_with_postproc = rename_positions ~abort_when_in_copybook:true ~copybooks
    @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY "lib.cpy".
        PROCEDURE DIVISION.
          MOVE 1 TO c_|_opied-var.
          STOP RUN.
  |cobol}
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/lib.cpy appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib.cpy"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"message":"Reference occurs in a copybook: not renaming","type":1},"method":"window/showMessage","jsonrpc":"2.0"}
    (line 7, character 21):
    No renames |}]

let%expect_test "rename-procedure" =
  let end_with_postproc = rename_positions @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        PROCEDURE DIVISION.
          s_|sec-simple|_ec SECTION.
          PERFORM sec.
          GO par_|para-simple|_a.
          para.
          PERFORM _|para-in-sec|_PARA IN _|sec-of-para|_SEC
          STOP RUN.
  |cobol}
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    para-in-sec
    (line 8, character 18):
    3 rename entries:
        aNewName at __rootdir__/prog.cob:9.18-9.22:
           6             PERFORM sec.
       7             GO para.
       8             para.
       9 >           PERFORM PARA IN SEC
    ----                     ^^^^
      10             STOP RUN.
      11
    aNewName at __rootdir__/prog.cob:7.13-7.17:
       4           PROCEDURE DIVISION.
       5             sec SECTION.
       6             PERFORM sec.
       7 >           GO para.
    ----                ^^^^
       8             para.
       9             PERFORM PARA IN SEC
    aNewName at __rootdir__/prog.cob:8.10-8.14:
       5             sec SECTION.
       6             PERFORM sec.
       7             GO para.
       8 >           para.
    ----             ^^^^
       9             PERFORM PARA IN SEC
      10             STOP RUN.
    para-simple
    (line 6, character 16):
    3 rename entries:
        aNewName at __rootdir__/prog.cob:9.18-9.22:
           6             PERFORM sec.
       7             GO para.
       8             para.
       9 >           PERFORM PARA IN SEC
    ----                     ^^^^
      10             STOP RUN.
      11
    aNewName at __rootdir__/prog.cob:7.13-7.17:
       4           PROCEDURE DIVISION.
       5             sec SECTION.
       6             PERFORM sec.
       7 >           GO para.
    ----                ^^^^
       8             para.
       9             PERFORM PARA IN SEC
    aNewName at __rootdir__/prog.cob:8.10-8.14:
       5             sec SECTION.
       6             PERFORM sec.
       7             GO para.
       8 >           para.
    ----             ^^^^
       9             PERFORM PARA IN SEC
      10             STOP RUN.
    sec-of-para
    (line 8, character 26):
    3 rename entries:
        aNewName at __rootdir__/prog.cob:9.26-9.29:
           6             PERFORM sec.
       7             GO para.
       8             para.
       9 >           PERFORM PARA IN SEC
    ----                             ^^^
      10             STOP RUN.
      11
    aNewName at __rootdir__/prog.cob:6.18-6.21:
       3           PROGRAM-ID. prog.
       4           PROCEDURE DIVISION.
       5             sec SECTION.
       6 >           PERFORM sec.
    ----                     ^^^
       7             GO para.
       8             para.
    aNewName at __rootdir__/prog.cob:5.10-5.13:
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID. prog.
       4           PROCEDURE DIVISION.
       5 >           sec SECTION.
    ----             ^^^
       6             PERFORM sec.
       7             GO para.
    sec-simple
    (line 4, character 11):
    3 rename entries:
        aNewName at __rootdir__/prog.cob:9.26-9.29:
           6             PERFORM sec.
       7             GO para.
       8             para.
       9 >           PERFORM PARA IN SEC
    ----                             ^^^
      10             STOP RUN.
      11
    aNewName at __rootdir__/prog.cob:6.18-6.21:
       3           PROGRAM-ID. prog.
       4           PROCEDURE DIVISION.
       5             sec SECTION.
       6 >           PERFORM sec.
    ----                     ^^^
       7             GO para.
       8             para.
    aNewName at __rootdir__/prog.cob:5.10-5.13:
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID. prog.
       4           PROCEDURE DIVISION.
       5 >           sec SECTION.
    ----             ^^^
       6             PERFORM sec.
       7             GO para. |}]
