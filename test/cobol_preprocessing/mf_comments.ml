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

(** Tests for Micro Focus-style comment lines (GnuCOBOL's `-fmfcomment`): when
    enabled, lines with an asterisk in the first column are comments in FIXED,
    COBOL85, and VARIABLE formats. *)

let prog = {|
      * nominal comment
* MF-STYLE COMMENT.
*     DISPLAY "commented out".
       DISPLAY "kept".
|}

let%expect_test "mf-comments-disabled" =
  Preproc_testing.show_text prog;
  [%expect {|
    prog.cob:3.6-3.7:
    >> Error: Unexpected indicator: `T'

    YLE@<prog.cob:3-7|3-10>
    COMMENT@<prog.cob:3-11|3-18>
    .@<prog.cob:3-18|3-19>
    DISPLAY@<prog.cob:5-7|5-14>
    "kept"@<prog.cob:5-15|5-21>
    .@<prog.cob:5-21|5-22>
    |}]

let%expect_test "mf-comments-fixed" =
  Preproc_testing.show_text ~mf_comments:true prog;
  [%expect {|
    DISPLAY@<prog.cob:5-7|5-14>
    "kept"@<prog.cob:5-15|5-21>
    .@<prog.cob:5-21|5-22> |}]

let%expect_test "mf-comments-cobol85" =
  Preproc_testing.show_text ~mf_comments:true prog
    ~source_format:Cobol_config.(SF SFCOBOL85);
  [%expect {|
    DISPLAY@<prog.cob:5-7|5-14>
    "kept"@<prog.cob:5-15|5-21>
    .@<prog.cob:5-21|5-22> |}]

let%expect_test "mf-comments-variable" =
  Preproc_testing.show_text ~mf_comments:true prog
    ~source_format:Cobol_config.(SF SFVariable);
  [%expect {|
    DISPLAY@<prog.cob:5-7|5-14>
    "kept"@<prog.cob:5-15|5-21>
    .@<prog.cob:5-21|5-22> |}]

let%expect_test "mf-comments-xcard" =
  Preproc_testing.show_text ~mf_comments:true prog
    ~source_format:Cobol_config.(SF SFxCard);
  [%expect {|
    prog.cob:3.6-3.7:
    >> Error: Unexpected indicator: `T'

    YLE@<prog.cob:3-7|3-10>
    COMMENT@<prog.cob:3-11|3-18>
    .@<prog.cob:3-18|3-19>
    DISPLAY@<prog.cob:5-7|5-14>
    "kept"@<prog.cob:5-15|5-21>
    .@<prog.cob:5-21|5-22> |}]

let%expect_test "mf-comments-free" =
  Preproc_testing.show_text ~mf_comments:true ~source_format:Cobol_config.(SF SFFree) {|
* in free format, this is not a comment line.
DISPLAY "kept".
|};
  [%expect {|
    *@<prog.cob:2-0|2-1>
    in@<prog.cob:2-2|2-4>
    free@<prog.cob:2-5|2-9>
    format@<prog.cob:2-10|2-16>
    ,@<prog.cob:2-16|2-17>
    this@<prog.cob:2-18|2-22>
    is@<prog.cob:2-23|2-25>
    not@<prog.cob:2-26|2-29>
    a@<prog.cob:2-30|2-31>
    comment@<prog.cob:2-32|2-39>
    line@<prog.cob:2-40|2-44>
    .@<prog.cob:2-44|2-45>
    DISPLAY@<prog.cob:3-0|3-7>
    "kept"@<prog.cob:3-8|3-14>
    .@<prog.cob:3-14|3-15> |}]

let%expect_test "mf-comments-within-continuation" =
  Preproc_testing.show_text ~mf_comments:true {|
       DISPLAY "abc
*  interrupting comment
      -    "def".
|};
  [%expect {|
    DISPLAY@<prog.cob:2-7|2-14>
    "abc                                                     def"@Cat { left = <prog.cob:2-15|2-72>;
                                                                        right = <prog.cob:4-11|4-16> }
    .@<prog.cob:4-16|4-17> |}]

let%expect_test "mf-comments-after-source-format-change" =
  Preproc_testing.show_text ~mf_comments:true {|
       >>SOURCE FORMAT IS FREE
* not a comment.
>>SOURCE FORMAT IS FIXED
* comment again.
       DISPLAY "kept".
|};
  [%expect {|
    *@<prog.cob:3-0|3-1>
    not@<prog.cob:3-2|3-5>
    a@<prog.cob:3-6|3-7>
    comment@<prog.cob:3-8|3-15>
    .@<prog.cob:3-15|3-16>
    DISPLAY@<prog.cob:6-7|6-14>
    "kept"@<prog.cob:6-15|6-21>
    .@<prog.cob:6-21|6-22> |}]

let%expect_test "mf-comments-in-copybooks" =
  Preproc_testing.show_text ~mf_comments:true
    ~copybooks:["cpy", {|
* comment in copybook.
       DISPLAY "from copybook".
|}] {|
       COPY cpy.
|};
  [%expect {|
    DISPLAY@Cpy { copied = <cpy:3-7|3-14> }
    "from copybook"@Cpy { copied = <cpy:3-15|3-30> }
    .@Cpy { copied = <cpy:3-30|3-31> } |}]
