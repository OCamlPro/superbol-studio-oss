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

let%expect_test "auto-fixed-comment" =
  Preproc_testing.preprocess ~source_format:Auto {|
      * comment
  |};
  [%expect {||}];;


let%expect_test "auto-empty" =
  Preproc_testing.preprocess ~source_format:Auto {||};
  [%expect {||}];;


let%expect_test "auto-fixed-1" =
  Preproc_testing.preprocess ~source_format:Auto "\
\       IDENTIFICATION   DIVISION.
\       PROGRAM-ID.      prog.";
  [%expect {| IDENTIFICATION DIVISION . PROGRAM-ID . PROG . |}];;


let%expect_test "auto-fixed-2" =
  Preproc_testing.preprocess ~source_format:Auto "\
\
\
\      * comment
\
\       IDENTIFICATION   DIVISION.
\       PROGRAM-ID.      prog.";
  [%expect {| IDENTIFICATION DIVISION . PROGRAM-ID . PROG . |}];;


let%expect_test "auto-free-1" =
  Preproc_testing.preprocess ~source_format:Auto "\
IDENTIFICATION   DIVISION.
PROGRAM-ID.      prog.";
  [%expect {| IDENTIFICATION DIVISION . PROGRAM-ID . PROG . |}];;


let%expect_test "auto-free-2" =
  Preproc_testing.preprocess ~source_format:Auto "\
*> comment
IDENTIFICATION   DIVISION.
PROGRAM-ID.      prog.";
  [%expect {| IDENTIFICATION DIVISION . PROGRAM-ID . PROG . |}];;


let%expect_test "auto-free-3" =
  Preproc_testing.preprocess ~source_format:Auto "\
\t
*> comment
IDENTIFICATION   DIVISION.
PROGRAM-ID.      prog.";
  [%expect {| IDENTIFICATION DIVISION . PROGRAM-ID . PROG . |}];;


let%expect_test "auto-free-tab-only" =
  Preproc_testing.preprocess ~source_format:Auto "\t";
  [%expect {||}];;


let%expect_test "auto-free-tab-comment" =
  Preproc_testing.preprocess ~source_format:Auto "\t*> comment";
  [%expect {||}];;
