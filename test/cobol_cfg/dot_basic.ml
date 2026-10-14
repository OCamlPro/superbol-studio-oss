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
  Cfg_testing.show_dot_cfgs Cfg_basic.performs;
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

let%expect_test "ambiguous-n-anonymous" =
  Cfg_testing.show_dot_cfgs Cfg_basic.ambiguous_n_anonymous;
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

let%expect_test "section-starts-with-paragraph" =
  Cfg_testing.show_dot_cfgs Cfg_basic.section_starts_with_paragraph;
  [%expect {|
           PROGRAM-ID. section-starts-with-paragraph.
           DATA DIVISION.
           PROCEDURE DIVISION.
           PERFORM A.
              GOBACK.
           A SECTION.
           B. PERFORM C.
              GOBACK.
           C. GOBACK.

    ---

    section-starts-with-paragraph:
      digraph G {
        node [shape=box, ];
        0 [shape=doubleoctagon, label="Entry\nparagraph", ];
        2 [label="B IN A", ];
        3 [label="C IN A", ];


        0 -> 2 [style="dashed", ];
        2 -> 3 [style="dashed", ];

        } |}];;

let%expect_test "section-starts-with-paragraph-section-cfg" =
  Cfg_testing.show_dot_section_cfg Cfg_basic.section_starts_with_paragraph
    ~name:"A (section-starts-with-paragraph)";
  [%expect {|
    A (section-starts-with-paragraph):
      digraph G {
        node [shape=box, ];
        1 [label="B", ];
        2 [label="C", ];
        3 [shape=doubleoctagon, label="A", ];


        1 -> 2 [style="dashed", ];
        3 -> 1 [style="dotted", ];

        } |}];;

let%expect_test "performs-section-cfg" =
  Cfg_testing.show_dot_section_cfg Cfg_basic.performs
    ~name:"A (performs)";
  [%expect {|
    A (performs):
      digraph G {
        node [shape=box, ];
        1 [shape=doubleoctagon, label="A", ];
        2 [label="B", ];
        3 [label="C", ];
        4 [shape=plaintext, label="D", ];


        1 -> 2 [style="dashed", ];
        2 -> 3 [style="dashed", ];
        3 -> 4 [style="dashed", ];

        } |}];;
