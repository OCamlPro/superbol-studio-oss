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

let%expect_test "fixed-format-cdirs" =
  Preproc_testing.show_source_lines {|
       >>SET A
      >>SET B
      $ SET B
  |};
  [%expect {|
    >>SET A
    >>SET B
    $SET B
|}];;

let%expect_test "hybrid-format-cdirs" =
  Preproc_testing.show_source_lines {|
      >>SOURCE FORMAT IS FREE
>>SOURCE FORMAT IS FIXED
       >> SET SOURCEFORMAT "COBOLX"
* comment line
                *> floating comment
>>    Source format free
  *> another floating comment
   >> SET                               SOURCEFORMAT                                     "FIXED"
      * fixed comment
      $ SET SOURCEFORMAT "XOpen"
/ comment line
>>SET SOURCEFORMAT "CRT"
/ still comment line
>>   SOURCE IS FREE
                        *> ok let's terminate here
  .
  |};
  [%expect {|
    >>SOURCE FORMAT IS FREE
    >>SOURCE FORMAT IS FIXED
    >>SET SOURCEFORMAT "COBOLX"


    >>Source format free

    >>SET SOURCEFORMAT "FIXED"

    $SET SOURCEFORMAT "XOpen"

    >>SET SOURCEFORMAT "CRT"

    >>SOURCE IS FREE

    .
|}];;

let%expect_test "hybrid-format-cdirs-with-cdir-markers" =
  Preproc_testing.show_source_lines
    ~with_source_cdir_markers:true
    ~with_line_numbers:true {|
      >>SOURCE FORMAT IS FREE
>>SOURCE FORMAT IS FIXED
       >> SET SOURCEFORMAT "COBOLX"
* comment line
                *> floating comment
>>    Source format free
  *> another floating comment
   >> SET                               SOURCEFORMAT                                     "FIXED"
      * fixed comment
      $ SET SOURCEFORMAT "XOpen"
/ comment line
>>SET SOURCEFORMAT "CRT"
/ still comment line
>>   SOURCE IS FREE
                        *> ok let's terminate here
  .
  |};
  [%expect {|
    1:
    2: >>SOURCE FORMAT IS FREE
    2: |new source format|
    3: >>SOURCE FORMAT IS FIXED
    3: |new source format|
    4: >>SET SOURCEFORMAT "COBOLX"
    4: |new source format|
    5:
    6:
    7: >>Source format free
    7: |new source format|
    8:
    9: >>SET SOURCEFORMAT "FIXED"
    9: |new source format|
    10:
    11: $SET SOURCEFORMAT "XOpen"
    11: |new source format|
    12:
    13: >>SET SOURCEFORMAT "CRT"
    13: |new source format|
    14:
    15: >>SOURCE IS FREE
    15: |new source format|
    16:
    17: .
|}];;

let%expect_test "hybrid-format-cdirs-with-cdir-markers-bis" =
  Preproc_testing.show_source_lines
    ~with_source_cdir_markers:true
    ~with_compiler_directives_text:false
    ~with_line_numbers:true {|
      >>SOURCE FORMAT IS FREE
>>SOURCE FORMAT IS FIXED
       >> SET SOURCEFORMAT "COBOLX"
* comment line
                *> floating comment
>>    Source format free
  *> another floating comment
   >> SET                               SOURCEFORMAT                                     "FIXED"
      * fixed comment
      $ SET SOURCEFORMAT "XOpen"
/ comment line
>>SET SOURCEFORMAT "CRT"
/ still comment line
>>   SOURCE IS FREE
                        *> ok let's terminate here
  .
  |};
  [%expect {|
    1:
    2: |new source format|
    3: |new source format|
    4: |new source format|
    5:
    6:
    7: |new source format|
    8:
    9: |new source format|
    10:
    11: |new source format|
    12:
    13: |new source format|
    14:
    15: |new source format|
    16:
    17: .
|}];;
