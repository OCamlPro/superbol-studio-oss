(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Tests for [superbol/compilationUnitNames]. *)

open Lsp_testing

let names server uri =
  match LSP.Request.INTERNAL.compilation_unit_names server ~uri with
  | None -> "<none>"
  | Some names -> Pretty.to_string "[%a]" Fmt.(list ~sep:(any ", ") string) names

let names_in ?(filename = "prog.cob") text =
  let { projdir; _ }, server = make_lsp_project () in
  let server, prog = add_cobol_doc server ~projdir filename text in
  names server prog.uri

(** Same as [names_in], for a document that is not backed by any file. *)
let names_in_untitled text =
  let _, server = make_lsp_project () in
  let uri = Lsp.Uri.t_of_yojson (`String "untitled:Untitled-1") in
  let server =
    LSP.Server.did_open
      Lsp.Types.DidOpenTextDocumentParams.{
        textDocument = Lsp.Types.TextDocumentItem.{
            languageId = "cobol"; version = 0; text; uri;
          };
      } server
  in
  names server uri

let%expect_test "single-program" =
  let names = names_in {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello-world.
       PROCEDURE DIVISION.
           STOP RUN.
  |cobol} in
  ignore [%expect.output];                     (* drop server notifications *)
  print_endline names;
  [%expect {| [hello-world] |}]

let%expect_test "literal-and-sequence" =
  let names = names_in {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "prog1".
       PROCEDURE DIVISION.
           STOP RUN.
       END PROGRAM "prog1".
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog2.
       PROCEDURE DIVISION.
           GOBACK.
       END PROGRAM prog2.
  |cobol} in
  ignore [%expect.output];                     (* drop server notifications *)
  print_endline names;
  [%expect {| [prog1, prog2] |}]

let%expect_test "missing-name" =
  let names = names_in {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
  |cobol} in
  ignore [%expect.output];                     (* drop server notifications *)
  print_endline names;
  [%expect {| [] |}]

let%expect_test "nameless-unit" =
  (* Recovery picks the next word as the program name. *)
  let names = names_in {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
       PROCEDURE DIVISION.
           STOP RUN.
  |cobol} in
  ignore [%expect.output];                     (* drop server notifications *)
  print_endline names;
  [%expect {| [PROCEDURE] |}]

let%expect_test "untitled" =
  let names = names_in_untitled {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. untitled-prog.
       PROCEDURE DIVISION.
           STOP RUN.
  |cobol} in
  ignore [%expect.output];                     (* drop server notifications *)
  print_endline names;
  [%expect {| [untitled-prog] |}]
