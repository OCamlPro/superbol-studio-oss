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
  Preproc_testing.preprocess_n_then_cut_n_paste_right_of_indicator "\
\      **> comment
  ";
  [%expect {|
    fixed: "      **> comment"
           "  "
     free: "*> comment"
           "  " |}];;


let%expect_test "auto-empty" =
  Preproc_testing.preprocess_n_then_cut_n_paste_right_of_indicator "";
  [%expect {|
    fixed:
     free: |}];;


let%expect_test "auto-fixed-1" =
  Preproc_testing.preprocess_n_then_cut_n_paste_right_of_indicator "\
\       IDENTIFICATION   DIVISION.
\       PROGRAM-ID.      prog.";
  [%expect {|
    fixed: "       IDENTIFICATION   DIVISION."
           "       PROGRAM-ID.      prog."
     free: "IDENTIFICATION   DIVISION."
           "PROGRAM-ID.      prog."
    IDENTIFICATION DIVISION .
    PROGRAM-ID . PROG . EOF
    IDENTIFICATION DIVISION .
    PROGRAM-ID . PROG . EOF
    IDENTIFICATION DIVISION .
    PROGRAM-ID . PROG . EOF |}];;


let%expect_test "auto-fixed-2" =
  Preproc_testing.preprocess_n_then_cut_n_paste_right_of_indicator "\
\
\
\      **> comment
\
\       IDENTIFICATION   DIVISION.
\       PROGRAM-ID.      prog.";
  [%expect {|
    fixed: "      **> comment"
           "       IDENTIFICATION   DIVISION."
           "       PROGRAM-ID.      prog."
     free: "*> comment"
           "IDENTIFICATION   DIVISION."
           "PROGRAM-ID.      prog."
    IDENTIFICATION DIVISION .
    PROGRAM-ID . PROG . EOF
    IDENTIFICATION DIVISION .
    PROGRAM-ID . PROG . EOF
    IDENTIFICATION DIVISION .
    PROGRAM-ID . PROG . EOF |}];;
