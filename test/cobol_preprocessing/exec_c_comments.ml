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


(** Tests for C-style comments ([/* ... */]) in EXEC SQL blocks. *)

let show ?(source_format = Cobol_config.(SF SFFixed)) contents =
  let pp =
    Cobol_preproc.Input.string ~filename:"prog.cob" contents |>
    Cobol_preproc.preprocessor
      ~options:(Preproc_testing.options ~source_format ()) |>
    Preproc_testing.show_all_text
  in
  let comments = EzCompat.StringMap.find "" (Cobol_preproc.rev_comments pp) in
  List.iter begin fun Cobol_preproc.Text.{ comment_loc = (s, e);
                                           comment_contents; _ } ->
    Pretty.out "comment %d.%d-%d.%d: %S@\n"
      s.pos_lnum (s.pos_cnum - s.pos_bol)
      e.pos_lnum (e.pos_cnum - e.pos_bol) comment_contents
  end (List.rev comments)

let%expect_test "single-line-comments" =
  show {|
           EXEC SQL
             /* simple comment */ SELECT A /* another one */ FROM T
             /*x*/SELECT/* glued */B
           END-EXEC.
           DISPLAY "after".
|};
  [%expect {|
    EXEC[EXEC SQL SELECT A FROM T SELECT B END-EXEC]
    . DISPLAY "after" .
    comment 3.13-3.33: "/* simple comment */"
    comment 3.43-3.60: "/* another one */"
    comment 4.13-4.18: "/*x*/"
    comment 4.24-4.35: "/* glued */" |}]

let%expect_test "multi-line-comment-with-cobol-delimiters" =
  show {|
           EXEC SQL
             /* it's tricky. END-EXEC
                "unclosed quote
      *     a COBOL comment line inside the C-style comment
                and a period. */ SELECT B FROM T
           END-EXEC.
           DISPLAY "after".
|};
  [%expect {|
    EXEC[EXEC SQL SELECT B FROM T END-EXEC]
    . DISPLAY "after" .
    comment 3.13-3.37: "/* it's tricky. END-EXEC"
    comment 4.7-4.31: "         \"unclosed quote"
    comment 5.6-5.59: "*     a COBOL comment line inside the C-style comment"
    comment 6.7-6.32: "         and a period. */" |}]

let%expect_test "no-comments-outside-exec-sql" =
  show {|
           COMPUTE X = A / B.
           MOVE /*A TO B.
           EXEC CICS /* not a comment */ END-EXEC.
           EXEC SQL SELECT 1 FROM T END-EXEC /* not a comment either */.
|};
  [%expect {|
    COMPUTE X = A / B .
    MOVE /*A TO B .
    EXEC[EXEC CICS /* not a comment */ END-EXEC]
    . EXEC[EXEC SQL SELECT 1 FROM T END-EXEC]
    /* not a comment either */ . EOF |}]

let%expect_test "comment-delimiters-across-cut-column" =
  (* `/*' fits in columns 71-72 on line 2; `*/' spans columns 72-73 on line 3
     so it does not close the comment; `/*' spans columns 72-73 on line 4, so
     it is a text word truncated to `/'. *)
  show {|
           EXEC SQL                                                   /*SEQNUM01
             SELECT A FROM T                                           */EQNUM02
             still in comment */ SELECT B FROM T                       /*EQNUM03
           END-EXEC.
|};
  [%expect {|
    EXEC[EXEC SQL SELECT B FROM T / END-EXEC]
    . EOF
    comment 2.70-2.72: "/*"
    comment 3.7-3.72: "      SELECT A FROM T                                           *"
    comment 4.7-4.32: "      still in comment */" |}]

let%expect_test "unterminated-comment" =
  show {|
           EXEC SQL
             /* never closed
           END-EXEC.
           DISPLAY "lost".
|};
  [%expect {|
    EXEC[EXEC SQL EOF]
    prog.cob:3.13-3.15:
    >> Error: Unterminated comment

    prog.cob:2.11-6.0:
    >> Error: Unterminated EXEC/END-EXEC block

    comment 3.13-3.28: "/* never closed"
    comment 4.7-4.20: "    END-EXEC."
    comment 5.7-5.26: "    DISPLAY \"lost\"." |}]

let%expect_test "free-format" =
  show ~source_format:Cobol_config.(SF SFFree) {|
EXEC SQL /* it's
  a comment */ SELECT A FROM T /* another */ END-EXEC.
DISPLAY "after" /* not a comment */.
|};
  [%expect {|
    EXEC[EXEC SQL SELECT A FROM T END-EXEC]
    . DISPLAY "after" /* not a comment */ .
    comment 2.9-2.16: "/* it's"
    comment 3.0-3.14: "  a comment */"
    comment 3.31-3.44: "/* another */" |}]
