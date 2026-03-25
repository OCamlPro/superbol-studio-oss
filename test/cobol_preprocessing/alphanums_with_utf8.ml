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
    "α"@<prog.cob:8-7|8-11>
    "β"@<prog.cob:9-52|9-56>
    "γδ"@<prog.cob:10-65|10-71>
    "εζ@<prog.cob:12-69|12-74>
    "ηθικ"@Cat { left = <prog.cob:13-69|13-74>;
                     right = <prog.cob:14-68|14-74> }
    "λμνξ"@Cat { left = <prog.cob:15-69|15-74>;
                     right = <prog.cob:16-68|16-74> }
    "o πρ σ"@Cat { left = Cat { left = <prog.cob:17-69|17-72>;
                                   right = <prog.cob:18-68|18-74> };
                      right = <prog.cob:19-69|19-73> }
    "λμνξ"@Cat { left = <prog.cob:20-69|20-74>;
                     right = <prog.cob:21-68|21-74> }
    "o πρ σ"@Cat { left = Cat { left = <prog.cob:22-69|22-72>;
                                   right = <prog.cob:23-68|23-74> };
                      right = <prog.cob:24-69|24-73> } |}]
