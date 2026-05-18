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

(** Tests for tab expansion in fixed-format COBOL source lines.

    In fixed format, column positions are visual (after tab expansion using
    the configured tab stops).  Columns 73-80 are the identification/comment
    area and are ignored by the preprocessor.  With a tab in the line, content
    that is before byte column 73 can still land at visual column 73+ and must
    be ignored. *)

(* --- Tab in the SNA area (bytes 0-5) --- *)

let%expect_test "fixed-format-tab-cut-at-col-sna" =
  (* A tab at byte 0 (SNA area, before the indicator) expands to visual
     column 7 (first explicit tab stop), shifting every subsequent column by 5.
     "IGNORED" starts at byte 67 = visual column 73 and must be dropped. *)
  Preproc_testing.preprocess
    "\t STOP RUN.                                                        IGNORED";
  [%expect {| STOP RUN . |}]

(* --- Tab in the code area --- *)

let%expect_test "fixed-format-tab-cut-at-col-code" =
  (* A tab at byte 8 (column 9, 1-indexed, code area) expands to column 16
     (next explicit tab stop after 8), shifting subsequent content right by 6
     visual columns.  "IGNORED" starts at byte 66 = visual column 73 and must
     be dropped. *)
  Preproc_testing.preprocess
    "       A\tCOMPUTE WS-RES = WS-ORIG * 12.                           IGNORED";
  [%expect {| A COMPUTE WS-RES = WS-ORIG * 12 . |}]

(* --- Contrast: no tab, same byte position stays in the code area --- *)

let%expect_test "fixed-format-no-tab-byte-65-in-code-area" =
  (* Without a tab the column boundary is at byte 73.  A word starting at
     byte 65 (visual column 66) is still in the code area and appears in the
     output. *)
  Preproc_testing.preprocess
    "       A COMPUTE WS-RES = WS-ORIG * 12.                          INCODE";
  [%expect {| A COMPUTE WS-RES = WS-ORIG * 12 . INCODE |}]
