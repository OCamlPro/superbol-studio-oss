(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

let%expect_test "alphanumeric-with-utf8-items" =
  Prog_preproc.show_text ~source_format:(SF SFFixed) {cobol|
     2                                                               "AB|
     3-                                                             "CD"|
     4* Missing continuation not reported (cf Src_lexing.flush_continued)
     5                                                               "AB|
     6                                                               "AB|
     7-                                                             "CD"|
     8 "α"                                                              |
     9                                              "β"                 |
    10                                                           "γδ"   |
    11* Missing continuation not reported (cf Src_lexing.flush_continued)
    12                                                               "εζ|
    13                                                               "ηθ|
    14-                                                             "ικ"|
    15                                                               "λμ|
    16-                                                             "νξ"|
    17                                                               "o |
    18-                                                             "πρ |
    19-                                                              "σ"|
    20                                                               "λμ"
    21-                                                             "νξ"
    22                                                               "o
    23-                                                             "πρ
    24-                                                              "σ"
  |cobol};
  [%expect {|
    "ABCD"@Cat { left = <prog.cob:2-69|2-72>; right = <prog.cob:3-68|3-72> }
    "AB@<prog.cob:5-69|5-72>
    "ABCD"@Cat { left = <prog.cob:6-69|6-72>; right = <prog.cob:7-68|7-72> }
    "α"@<prog.cob:8-7|8-10>
    "β"@<prog.cob:9-52|9-55>
    "γδ"@<prog.cob:10-65|10-69>
    "εζ@<prog.cob:12-69|12-72>
    "ηθικ"@Cat { left = <prog.cob:13-69|13-72>;
                     right = <prog.cob:14-68|14-72> }
    "λμνξ"@Cat { left = <prog.cob:15-69|15-72>;
                     right = <prog.cob:16-68|16-72> }
    "o πρ σ"@Cat { left = Cat { left = <prog.cob:17-69|17-72>;
                                   right = <prog.cob:18-68|18-72> };
                      right = <prog.cob:19-69|19-72> }
    "λμνξ"@Cat { left = <prog.cob:20-69|20-72>;
                     right = <prog.cob:21-68|21-72> }
    "o πρ σ"@Cat { left = Cat { left = <prog.cob:22-69|22-72>;
                                   right = <prog.cob:23-68|23-72> };
                      right = <prog.cob:24-69|24-72> } |}]

let%expect_test "alphanum-tab-in-code-area-with-utf8" =
  (* Tab at the start of the code area (byte 7, visual col 8 → 16, shift +7).
     Source locations report codepoint-adjusted byte offsets, so a tab never
     changes the byte offsets in the location output — it only shifts the
     visual columns used internally for the column-72 cutoff.

     Lines 2-3: complete UTF-8 literals (tab shifts their visual start to col 16)
     Line 4*:   separator comment
     Line 5:    AlphanumPrefix — no closing quote; tab shifts the opening to col 16
     Line 6*:   missing continuation (flush of line 5's prefix)
     Lines 7-8: tab shifts prefix to col 16; continuation (no tab) closes it
     Line 9*:   flush comment *)
  Prog_preproc.show_text ~source_format:(SF SFFixed) {cobol|
     2 	"α"                                                              |
     3 	"αβ"                                                             |
     4* complete literals above; prefix and continuation below
     5 	"εζ                                                              |
     6* Missing continuation
     7 	"αβ                                                              |
     8-                                                             "γδ"|
     9* flush
  |cobol};
  [%expect {|
    "α"@<prog.cob:2-8|2-11>
    "αβ"@<prog.cob:3-8|3-12>
    "εζ                                                      @<prog.cob:5-8|5-65>
    "αβ                                                      γδ"@Cat {
    left = <prog.cob:7-8|7-65>; right = <prog.cob:8-68|8-72> } |}]

let%expect_test "alphanum-tab-in-sna-with-utf8" =
  (* Tab at byte 0 expands past the indicator column (visual col 7), so the
     indicator is treated as a space and nominal processing starts at col 8.
     UTF-8 literals therefore begin two bytes into the line (byte 2) instead
     of the usual eight.

     Comment lines (indicator `*`) use the standard 6-byte SNA so the `*`
     lands at the indicator column and is not misread as a code-area token.

     Lines 2-3: complete UTF-8 literals (tab in SNA shifts code area start)
     Line 4*:   separator comment
     Line 5:    AlphanumPrefix — tab in SNA, no closing quote
     Line 6*:   missing continuation (flush of line 5's prefix)
     Lines 7:   tab in SNA, prefix; standard continuation closes it
     Line 9*:   flush comment *)
  Prog_preproc.show_text ~source_format:(SF SFFixed) {cobol|
	 "α"                                                              |
	 "αβ"                                                             |
     4* complete literals above; prefix and continuation below
	 "εζ                                                              |
     6* Missing continuation
	 "αβ                                                              |
     8-                                                             "γδ"|
     9* flush
  |cobol};
  [%expect {|
    "α"@<prog.cob:2-2|2-5>
    "αβ"@<prog.cob:3-2|3-6>
    "εζ                                                             @<prog.cob:5-2|5-66>
    "αβ                                                             γδ"@Cat {
    left = <prog.cob:7-2|7-66>; right = <prog.cob:8-68|8-72> } |}]

let%expect_test "alphanum-tab-inside-literal-with-utf8" =
  Prog_preproc.show_text ~source_format:(SF SFFixed) {cobol|
       "α	β"
       "	αβ"
       "αβ	"
       "α	β	γ"
  |cobol};
  [%expect {|
    prog.cob:2.7-2.12:
    >> Warning: Tab character in alphanumeric literal: visual column alignment
                may differ from character column in fixed-format source

    prog.cob:3.7-3.12:
    >> Warning: Tab character in alphanumeric literal: visual column alignment
                may differ from character column in fixed-format source

    prog.cob:4.7-4.12:
    >> Warning: Tab character in alphanumeric literal: visual column alignment
                may differ from character column in fixed-format source

    prog.cob:5.7-5.14:
    >> Warning: Tab character in alphanumeric literal: visual column alignment
                may differ from character column in fixed-format source

    "α	β"@<prog.cob:2-7|2-12>
    "	αβ"@<prog.cob:3-7|3-12>
    "αβ	"@<prog.cob:4-7|4-12>
    "α	β	γ"@<prog.cob:5-7|5-14> |}]

let%expect_test "alphanum-outer-tab-and-inner-tab-with-utf8" =
  Prog_preproc.show_text ~source_format:(SF SFFixed) {cobol|
     2 	"α	β"
  |cobol};
  [%expect {|
    prog.cob:2.8-2.13:
    >> Warning: Tab character in alphanumeric literal: visual column alignment
                may differ from character column in fixed-format source

    "α	β"@<prog.cob:2-8|2-13> |}]
