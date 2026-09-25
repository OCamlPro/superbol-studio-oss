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

let%expect_test "performs" =
  Cfg_testing.show_ascii_cfgs Cfg_basic.performs;
  [%expect {|
           PROGRAM-ID. performs.
           DATA DIVISION.
           PROCEDURE DIVISION.
           A SECTION.
              PERFORM B.
              GOBACK.
           B. PERFORM C.
              GOBACK.
           C. PERFORM D.
              GOBACK.
           D SECTION.
              PERFORM A.
              GOBACK.

    ---

    #========#
    H Entry  H
    H point  H
    #========#
      :
      :
      v
    +--------+
    |   A    | <+
    +--------+  '
      '         '
      '         '
      v         '
    +--------+  '
    | B IN A |  '
    +--------+  '
      '         '
      '         '
      v         '
    +--------+  '
    | C IN A |  '
    +--------+  '
      '         '
      '         '
      v         '
    +--------+  '
    |   D    |  +
    +--------+ |}];;

let%expect_test "ambiguous-n-anonymous" =
  Cfg_testing.show_ascii_cfgs Cfg_basic.ambiguous_n_anonymous;
  [%expect {|
           PROGRAM-ID. ambiguous-n-anonymous.
           DATA DIVISION.
           PROCEDURE DIVISION.
           PERFORM A.
              PERFORM B.
              GOBACK.
           B. PERFORM A.
              GOBACK.
           A SECTION.
              PERFORM A.
              PERFORM B.
              GOBACK.
           B. PERFORM A.
              GOBACK.
          *A. GOBACK.  *> forbidden redefinition of A

    ---

    #===========#
    H   Entry   H
    H paragraph H -+
    #===========#  '
      '            '
      '            '
      v            '
    +-----------+  '
    |     B     |  '
    +-----------+  '
      '            '
      '            '
      v            v
    +----------------+
    |                |  - +
    |       A        |    '
    |                | <- +
    +----------------+
      '            ^
      '            '
      v            '
    +-----------+  '
    |  B IN A   | -+
    +-----------+ |}];;
