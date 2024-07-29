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
  [%expect{||}];;
