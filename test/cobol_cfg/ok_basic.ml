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

open Prog_printer

let dotest = Cfg_testing.show_cfg

let performs =
  prog "performs"
      ~procedure:{|
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
    |}

let%expect_test "performs" =
  dotest performs;
  [%expect {|
    performs:
      digraph G {
        node [shape=box, ];
        0 [shape=doubleoctagon, label="Entry\npoint", ];
        1 [label="A", ];
        2 [label="B IN A", ];
        3 [label="C IN A", ];
        4 [label="D", ];


        0 -> 1 [style="dotted", ];
        1 -> 2 [style="dashed", ];
        2 -> 3 [style="dashed", ];
        3 -> 4 [style="dashed", ];
        4 -> 1 [style="dashed", ];

        } |}];;

let ambiguous_n_anonymous =
  prog "ambiguous-n-anonymous"
    ~procedure:{|
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
    |}

let%expect_test "ambiguous-n-anonymous" =
  dotest ambiguous_n_anonymous;
  [%expect {|
    ambiguous-n-anonymous:
      digraph G {
        node [shape=box, ];
        0 [shape=doubleoctagon, label="Entry\nparagraph", ];
        2 [label="B", ];
        3 [label="A", ];
        4 [label="B IN A", ];


        0 -> 2 [style="dashed", ];
        0 -> 3 [style="dashed", ];
        2 -> 3 [style="dashed", ];
        3 -> 3 [style="dashed", ];
        3 -> 4 [style="dashed", ];
        4 -> 3 [style="dashed", ];

        } |}];;
