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

let%expect_test "intrinsics-handling-in-multiunit-group" =
  Parser_testing.just_parse {|
       IDENTIFICATION DIVISION.
       FUNCTION-ID.   docalc.
       DATA           DIVISION.
       LINKAGE        SECTION.
       01 RES         PIC X(4).
       PROCEDURE      DIVISION RETURNING RES.
      *> Syntax error here (missing paragraph name).
      *> This triggers a parser recovery path that includes a reduction of
      *> procedure_division nonterminal, that is itself associated with a
      *> post-action.  Until the next commit in history, that led to an
      *> internal error due to a missed unregistration of intrinsics.
           MOVE "func" TO RES
           GOBACK.
       END FUNCTION docalc.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    test.
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       REPOSITORY.
       FUNCTION       TRIM INTRINSIC.
       PROCEDURE      DIVISION.
          STOP RUN.
       END PROGRAM test.
  |};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "Assert_failure src/lsp/cobol_parser/text_tokenizer.ml:755:2"
  Raised at Cobol_parser__Parser_diagnostics.add_exn in file "src/lsp/cobol_parser/parser_diagnostics.ml", line 146, characters 6-13
  Called from Cobol_parser__Parser_engine.add_exn in file "src/lsp/cobol_parser/parser_engine.ml", line 156, characters 40-74
  Called from Cobol_parser__Parser_engine.on_exn in file "src/lsp/cobol_parser/parser_engine.ml", line 492, characters 9-21
  Called from Cobol_parser__Parser_engine.full_parse in file "src/lsp/cobol_parser/parser_engine.ml", line 507, characters 57-68
  Called from Cobol_parser__Parser_engine.parse_once in file "src/lsp/cobol_parser/parser_engine.ml", line 529, characters 16-61
  Called from Parser_testing.just_parse in file "test/cobol_parsing/parser_testing.ml", line 63, characters 2-236
  Called from Test_cobol_parser__Test_intrinsics_registration.(fun) in file "test/cobol_parsing/test_intrinsics_registration.ml", line 15, characters 2-917
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}];;
