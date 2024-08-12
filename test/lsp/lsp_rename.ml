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

let rename_positions ?(copybooks=[]) (doc, positions) : string -> unit =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  let server = List.fold_left begin fun server (name, document) ->
      add_cobol_doc server ~projdir name document
      |> fst
    end server copybooks in
  let server, prog = add_cobol_doc server ~projdir "prog.cob" doc in
  let rename_at_position ?key (position: Position.t) =
    let params = RenameParams.create ~newName:"aNewName" ~position ~textDocument:prog () in
    Pretty.out "%a(line %d, character %d):\n"
      Fmt.(option ~none:nop (string ++ sp)) key
      position.line position.character;
    begin
      try
      match LSP.Request.rename server params with
      | Error e ->
        Pretty.out "Renamed failed: %S@." e
      | Ok { changes = None; _ } ->
        Pretty.out "No renames@."
      | Ok { changes = Some assoc; _ } ->
        Pretty.out "@.@[<hv 4>%d rename entries:@;%a@]@\n"
          (count assoc)
          (Fmt.list ~sep:Fmt.sp pp_assoc_elem) assoc
      with _ -> Pretty.out "Failed rename@."
    end
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
        01 o_|_ld-name PIC 9.
        PROCEDURE DIVISION.
          MOVE 1 TO old-na_|_me.
          S_|_TOP RUN.
  |cobol}
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 5, character 12):
    2 rename entries:
        aNewName at __rootdir__/prog.cob:8.20-8.28:
           5           WORKING-STORAGE SECTION.
       6           01 old-name PIC 9.
       7           PROCEDURE DIVISION.
       8 >           MOVE 1 TO old-name.
    ----                       ^^^^^^^^
       9             STOP RUN.
      10
    aNewName at __rootdir__/prog.cob:6.11-6.19:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 old-name PIC 9.
    ----              ^^^^^^^^
       7           PROCEDURE DIVISION.
       8             MOVE 1 TO old-name.
    (line 7, character 26):
    2 rename entries:
        aNewName at __rootdir__/prog.cob:8.20-8.28:
           5           WORKING-STORAGE SECTION.
       6           01 old-name PIC 9.
       7           PROCEDURE DIVISION.
       8 >           MOVE 1 TO old-name.
    ----                       ^^^^^^^^
       9             STOP RUN.
      10
    aNewName at __rootdir__/prog.cob:6.11-6.19:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 old-name PIC 9.
    ----              ^^^^^^^^
       7           PROCEDURE DIVISION.
       8             MOVE 1 TO old-name.
    (line 8, character 11):
    0 rename entries: |}]

let%expect_test "rename-copybook" =
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
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib.cpy"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    (line 7, character 21):
    Renamed failed: "Reference of variable found in copybook, aborting rename" |}]

let%expect_test "rename-procedure" =
  let end_with_postproc = rename_positions @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        PROCEDURE DIVISION.
          s_|newSectionName|_ec SECTION.
          PERFORM sec.
          GO par_|_a.
          para.
          STOP RUN.
  |cobol}
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    newSectionName
    (line 4, character 11):
    2 rename entries:
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
    (line 6, character 16):
    2 rename entries:
        aNewName at __rootdir__/prog.cob:7.13-7.17:
           4           PROCEDURE DIVISION.
       5             sec SECTION.
       6             PERFORM sec.
       7 >           GO para.
    ----                ^^^^
       8             para.
       9             STOP RUN.
    aNewName at __rootdir__/prog.cob:8.10-8.14:
       5             sec SECTION.
       6             PERFORM sec.
       7             GO para.
       8 >           para.
    ----             ^^^^
       9             STOP RUN.
      10 |}]

