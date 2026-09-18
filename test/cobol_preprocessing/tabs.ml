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

(* --- Tab used as indentation on a continuation line --- *)

let%expect_test "fixed-format-tab-after-continuation-indicator" =
  (* The literal on the first line is deliberately left open (no closing
     quote, and long enough to overflow past column 72, so it is genuinely
     continued rather than padded).  The continuation line uses a real '-'
     indicator at column 7, followed by a *tab* (instead of a space) before
     the resuming quote.  The tab must only shift columns, not disturb the
     pending continuation: the two halves are expected to merge into a
     single Alphanum literal. *)
  Preproc_testing.preprocess
    "       MOVE \"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n\
    \      -\t\"BBB\" TO X.";
  [%expect {| MOVE "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABBB" TO X . |}]

(* --- Tab stops landing *before* the indicator column ---------------------

   These exercise the third dispatch case of [Src_lexing.sna_tab]: a tab whose
   expansion lands strictly inside the SNA area (0-indexed column < 6), after
   which the remaining SNA columns must still be consumed before the indicator
   is read.

   With [tab-width: 4] the stops are at (1-indexed) columns 5, 9, 13...  A tab
   at column 1 therefore leaves the next character at column 5, i.e. two SNA
   columns (5 and 6) still to consume before the indicator at column 7.

   Each tabbed case is paired with the tab-free line it must lex exactly like.
   Column rulers below count from 1; `|' marks the indicator column.
                          123456|8
                                                                          *)

let%expect_test "tab-free-indicator-comment" =
  (* Reference for "tab-lands-in-sna-comment": `*' in the indicator column. *)
  Preproc_testing.preprocess
    "      * comment";
  [%expect {| |}]

let%expect_test "tab-lands-in-sna-comment" =
  (* TAB -> column 5, two spaces fill columns 5-6, `*' lands on the indicator
     column 7: this is a comment line, exactly like the reference above. *)
  Preproc_testing.preprocess ~tab_width:[4]
    "\t  * comment";
  [%expect {| |}]

let%expect_test "tab-free-star-in-sna" =
  (* Reference: `*' in the last SNA column (6), indicator column is blank. *)
  Preproc_testing.preprocess
    "     * comment";
  [%expect {| comment |}]

let%expect_test "tab-lands-in-sna-star-still-in-sna" =
  (* TAB -> column 5, one space fills column 5, `*' lands on column 6, which is
     still the SNA area: NOT a comment.  Off-by-one guard: a [k_sna] argument
     of [7 - next_stop] would wrongly turn this into a comment. *)
  Preproc_testing.preprocess ~tab_width:[4]
    "\t * comment";
  [%expect {| comment |}]

let%expect_test "tab-free-star-in-area-a" =
  (* Reference: blank indicator, `*' is the first character of area A. *)
  Preproc_testing.preprocess
    "       * comment";
  [%expect {| * comment |}]

let%expect_test "tab-lands-in-sna-star-in-area-a" =
  (* TAB -> column 5, three spaces fill columns 5-7 (the last one being the
     indicator, hence blank), `*' lands on column 8, in area A.  Off-by-one
     guard: a [k_sna] argument of [5 - next_stop] would wrongly consume one SNA
     column too few and read `*' as the indicator. *)
  Preproc_testing.preprocess ~tab_width:[4]
    "\t   * comment";
  [%expect {| * comment |}]

let%expect_test "tab-lands-exactly-on-indicator-comment" =
  (* [tab-width: 6] puts the first stop on column 7: the tab lands exactly on
     the indicator column, which is the [k_indicator] case, and `*' is the
     indicator itself. *)
  Preproc_testing.preprocess ~tab_width:[6]
    "\t* comment";
  [%expect {| |}]

let%expect_test "tab-jumps-past-indicator-default-width" =
  (* With the default [tab-width: 8] the first stop is column 9: the tab jumps
     past the indicator column ([k_nominal]), and `*' is plain text.  This is
     the pre-existing behaviour and must not change. *)
  Preproc_testing.preprocess
    "\t* comment";
  [%expect {| * comment |}]

let%expect_test "two-tabs-jump-past-indicator" =
  (* [tab-width: 4]: the first tab lands on column 5 ([k_sna]), the second one
     is then at column 5 and jumps to column 9 ([k_nominal]), past the
     indicator: `*' is plain text. *)
  Preproc_testing.preprocess ~tab_width:[4]
    "\t\t* comment";
  [%expect {| * comment |}]

let%expect_test "tab-free-continuation-indicator" =
  (* Reference for "tab-lands-in-sna-continuation". *)
  Preproc_testing.preprocess
    "       MOVE \"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n\
    \      -\"BBB\" TO X.";
  [%expect {| MOVE "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABBB" TO X . |}]

let%expect_test "tab-lands-in-sna-continuation" =
  (* TAB -> column 5, two spaces fill columns 5-6, `-' lands on the indicator
     column: the pending literal must still be continued.  Guards that
     [flush_continued] is *not* applied on the new [k_sna] path. *)
  Preproc_testing.preprocess ~tab_width:[4]
    "       MOVE \"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n\
    \t  -\"BBB\" TO X.";
  [%expect {| MOVE "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABBB" TO X . |}]
