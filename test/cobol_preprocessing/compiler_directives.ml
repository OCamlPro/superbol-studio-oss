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
  Preproc_testing.preprocess {|
       >>SET A
      >>SET B
      $ SET B
  |};
  [%expect {|
    prog.cob:2.7-2.14:
    >> Error: Malformed compiler directive

    prog.cob:3.6-3.13:
    >> Error: Malformed compiler directive

    prog.cob:4.6-4.13:
    >> Error: Malformed compiler directive |}];;

let%expect_test "hybrid-format-cdirs" =
  Preproc_testing.preprocess {|
      >>SOURCE FORMAT IS FREE
>>SOURCE FORMAT IS FIXED
       >> SET SOURCEFORMAT "COBOLX"
* comment line
                *> floating comment
>>  Source format free
  *> another floating comment
   >> SET                               SOURCEFORMAT                                     "FIXED"
      * fixed comment
      $ SET SOURCEFORMAT "XOpen"
/ comment line
>>SET SOURCEFORMAT "CRT"
/ still comment line
>>  SOURCE IS FREE
                        *> ok let's terminate here
  |};
  [%expect {||}];;

let%expect_test "malformed-cdirs" =
  (* TODO: what should we do with the lonesome `>>`? *)
  Preproc_testing.preprocess {|
      >>foo
      >>
      >>*> empty one?
      $*> another empty one?
      >>   SOURCE IS FREE
>> ?
$
  |};
  [%expect {|
    prog.cob:2.6-2.11:
    >> Error: Invalid >>foo compiler directive

    prog.cob:3.6-3.8:
    >> Error: Invalid >> compiler directive

    prog.cob:4.6-4.8:
    >> Error: Invalid >> compiler directive

    prog.cob:5.6-5.7:
    >> Error: Invalid $ compiler directive

    prog.cob:7.0-7.4:
    >> Error: Invalid >> compiler directive

    prog.cob:8.0-8.1:
    >> Error: Invalid $ compiler directive
|}];;
