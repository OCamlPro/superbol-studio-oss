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

let codelens doc : string -> unit =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  let server, prog = add_cobol_doc server ~projdir "prog.cob" doc in
  let location_as_srcloc = new srcloc_resuscitator_cache in
  let params = CodeLensParams.create () ~textDocument:prog in
  LSP.Request.codelens server params |> List.rev
  |> List.iter begin fun (codelens: CodeLens.t) ->
    let location = Location.create ~range:codelens.range ~uri:prog.uri in
    codelens.command
    |> Option.iter begin fun (command: Command.t) ->
      Pretty.out "%a%s@."
        location_as_srcloc#pp location
        command.title end
  end;
  end_with_postproc
;;

let%expect_test "codelens" =
  let end_with_postproc = codelens {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 AA.
          02 BB PIC X.
          02 BBprime REDEFINES BB PIC 9.
          02 CC PIC X. 02 DD PIC X.
          66 ABCD RENAMES BB THRU DD.
        01 ZZ OCCURS 5 TIMES INDEXED BY INDEX1.
          02 YY PIC X.
          88 YYcond value "a".
        PROCEDURE DIVISION.
            MOVE aa TO aA.
            STOP RUN.
    |cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    __rootdir__/prog.cob:6.11:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 AA.
    ----              ^
       7             02 BB PIC X.
       8             02 BBprime REDEFINES BB PIC 9.
    3 references
    __rootdir__/prog.cob:7.13:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 AA.
       7 >           02 BB PIC X.
    ----                ^
       8             02 BBprime REDEFINES BB PIC 9.
       9             02 CC PIC X. 02 DD PIC X.
    3 references
    __rootdir__/prog.cob:8.13:
       5           WORKING-STORAGE SECTION.
       6           01 AA.
       7             02 BB PIC X.
       8 >           02 BBprime REDEFINES BB PIC 9.
    ----                ^
       9             02 CC PIC X. 02 DD PIC X.
      10             66 ABCD RENAMES BB THRU DD.
    1 reference
    __rootdir__/prog.cob:8.31:
       5           WORKING-STORAGE SECTION.
       6           01 AA.
       7             02 BB PIC X.
       8 >           02 BBprime REDEFINES BB PIC 9.
    ----                                  ^
       9             02 CC PIC X. 02 DD PIC X.
      10             66 ABCD RENAMES BB THRU DD.
    3 references
    __rootdir__/prog.cob:9.13:
       6           01 AA.
       7             02 BB PIC X.
       8             02 BBprime REDEFINES BB PIC 9.
       9 >           02 CC PIC X. 02 DD PIC X.
    ----                ^
      10             66 ABCD RENAMES BB THRU DD.
      11           01 ZZ OCCURS 5 TIMES INDEXED BY INDEX1.
    1 reference
    __rootdir__/prog.cob:9.26:
       6           01 AA.
       7             02 BB PIC X.
       8             02 BBprime REDEFINES BB PIC 9.
       9 >           02 CC PIC X. 02 DD PIC X.
    ----                             ^
      10             66 ABCD RENAMES BB THRU DD.
      11           01 ZZ OCCURS 5 TIMES INDEXED BY INDEX1.
    2 references
    __rootdir__/prog.cob:10.13:
       7             02 BB PIC X.
       8             02 BBprime REDEFINES BB PIC 9.
       9             02 CC PIC X. 02 DD PIC X.
      10 >           66 ABCD RENAMES BB THRU DD.
    ----                ^
      11           01 ZZ OCCURS 5 TIMES INDEXED BY INDEX1.
      12             02 YY PIC X.
    1 reference
    __rootdir__/prog.cob:10.26:
       7             02 BB PIC X.
       8             02 BBprime REDEFINES BB PIC 9.
       9             02 CC PIC X. 02 DD PIC X.
      10 >           66 ABCD RENAMES BB THRU DD.
    ----                             ^
      11           01 ZZ OCCURS 5 TIMES INDEXED BY INDEX1.
      12             02 YY PIC X.
    3 references
    __rootdir__/prog.cob:10.34:
       7             02 BB PIC X.
       8             02 BBprime REDEFINES BB PIC 9.
       9             02 CC PIC X. 02 DD PIC X.
      10 >           66 ABCD RENAMES BB THRU DD.
    ----                                     ^
      11           01 ZZ OCCURS 5 TIMES INDEXED BY INDEX1.
      12             02 YY PIC X.
    2 references
    __rootdir__/prog.cob:11.11:
       8             02 BBprime REDEFINES BB PIC 9.
       9             02 CC PIC X. 02 DD PIC X.
      10             66 ABCD RENAMES BB THRU DD.
      11 >         01 ZZ OCCURS 5 TIMES INDEXED BY INDEX1.
    ----              ^
      12             02 YY PIC X.
      13             88 YYcond value "a".
    1 reference
    __rootdir__/prog.cob:11.40:
       8             02 BBprime REDEFINES BB PIC 9.
       9             02 CC PIC X. 02 DD PIC X.
      10             66 ABCD RENAMES BB THRU DD.
      11 >         01 ZZ OCCURS 5 TIMES INDEXED BY INDEX1.
    ----                                           ^
      12             02 YY PIC X.
      13             88 YYcond value "a".
    1 reference
    __rootdir__/prog.cob:12.13:
       9             02 CC PIC X. 02 DD PIC X.
      10             66 ABCD RENAMES BB THRU DD.
      11           01 ZZ OCCURS 5 TIMES INDEXED BY INDEX1.
      12 >           02 YY PIC X.
    ----                ^
      13             88 YYcond value "a".
      14           PROCEDURE DIVISION.
    1 reference
    __rootdir__/prog.cob:13.13:
      10             66 ABCD RENAMES BB THRU DD.
      11           01 ZZ OCCURS 5 TIMES INDEXED BY INDEX1.
      12             02 YY PIC X.
      13 >           88 YYcond value "a".
    ----                ^
      14           PROCEDURE DIVISION.
      15               MOVE aa TO aA.
    0 reference |}];;

let%expect_test "codelens-procedure" =
  let end_with_postproc = codelens {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        PROCEDURE DIVISION.
            AA SECTION.
            BB.
            PERFORM BB.
            CC.
            DD SECTION.
            PERFORM AA.
            PERFORM BB.
            GO DD.
            STOP RUN.
    |cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    __rootdir__/prog.cob:5.12:
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID. prog.
       4           PROCEDURE DIVISION.
       5 >             AA SECTION.
    ----               ^
       6               BB.
       7               PERFORM BB.
    2 references
    __rootdir__/prog.cob:6.12:
       3           PROGRAM-ID. prog.
       4           PROCEDURE DIVISION.
       5               AA SECTION.
       6 >             BB.
    ----               ^
       7               PERFORM BB.
       8               CC.
    3 references
    __rootdir__/prog.cob:8.12:
       5               AA SECTION.
       6               BB.
       7               PERFORM BB.
       8 >             CC.
    ----               ^
       9               DD SECTION.
      10               PERFORM AA.
    1 reference
    __rootdir__/prog.cob:9.12:
       6               BB.
       7               PERFORM BB.
       8               CC.
       9 >             DD SECTION.
    ----               ^
      10               PERFORM AA.
      11               PERFORM BB.
    2 references |}];;
