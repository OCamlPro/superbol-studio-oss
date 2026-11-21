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

let%expect_test "exec-block-with-cobol-separators" =
  Parser_testing.show_parsed_tokens @@
  {|
       PROGRAM-ID.        prog.
       PROCEDURE          DIVISION.
           EXEC SQL
            BEGIN
              SELECT something_1, something_2 FROM some_table
                WHERE condition > 0;
              EXCEPTION
            END;
           END-EXEC.
           STOP RUN.
  |};
  [%expect{|
    PROGRAM-ID, ., INFO_WORD[prog], ., PROCEDURE, DIVISION, .,
    EXEC_BLOCK[EXEC SQL BEGIN SELECT something_1, something_2  FROM some_table WHERE condition > 0; EXCEPTION  END; END-EXEC],
    ., STOP, RUN, ., EOF |}];;

let%expect_test "unparsed-exec-block" =
  Parser_testing.show_parsed_tokens {|
       PROGRAM-ID.        prog.
       PROCEDURE          DIVISION.
           EXEC CICS
              SEND MAP('A') MAPSET('B') ERASE
           END-EXEC.
           STOP RUN.
  |};
  [%expect {|
    PROGRAM-ID, ., INFO_WORD[prog], ., PROCEDURE, DIVISION, ., EXEC_BLOCK[EXEC
    CICS SEND MAP( 'A' ) MAPSET( 'B' ) ERASE END-EXEC], ., STOP, RUN, ., EOF |}];;

let%expect_test "exec-block-sequence-without-separator-periods" =
  Parser_testing.show_parsed_tokens {|
       PROGRAM-ID.         prog.
       PROCEDURE           DIVISION.
           EXEC SQL A END-EXEC
           EXEC SQL B END-EXEC
           DISPLAY 'C'
           EXEC SQL D END-EXEC.
  |};
  [%expect {|
    PROGRAM-ID, ., INFO_WORD[prog], ., PROCEDURE, DIVISION, .,
    EXEC_BLOCK[EXEC SQL A END-EXEC], EXEC_BLOCK[EXEC SQL B END-EXEC], DISPLAY,
    'C', EXEC_BLOCK[EXEC SQL D END-EXEC], ., EOF |}];;

let show_exec_blocks prog =
  let { result = WithArtifacts (_, { tokens; _ }); _ } =
    Parser_testing.preproc prog |>
    Cobol_parser.parse_with_artifacts ~options:(Parser_testing.options ())
  in
  List.iter begin function
    | Cobol_common.Srcloc.{ payload = Cobol_parser.Tokens.EXEC_BLOCK b; _ } ->
        Pretty.out "%s: %a@."
          (match b with Esql _ -> "Esql" | Unparsed _ -> "Unparsed")
          Cobol_ptree.pp_exec_block b
    | _ ->
        ()
  end (Lazy.force tokens)

let%expect_test "unparsable-exec-sql-blocks" =
  show_exec_blocks {|
       PROGRAM-ID.        prog.
       PROCEDURE          DIVISION.
           EXEC SQL IGNORE whatever ! END-EXEC.
           EXEC SQL SELECT FROM FROM END-EXEC.
           EXEC SQL SELECT a FROM t WHERE b ! 0 END-EXEC.
           STOP RUN.
  |};
  [%expect {|
    Esql: EXEC SQL IGNORE  END-EXEC
    Unparsed: EXEC SQL SELECT FROM FROM
    END-EXEC
    Unparsed: EXEC SQL SELECT a FROM t WHERE b ! 0
    END-EXEC |}];;

let%expect_test "exec-sql-block-with-c-comments" =
  show_exec_blocks {|
       PROGRAM-ID.        prog.
       PROCEDURE          DIVISION.
           EXEC SQL
             /* it's tricky. END-EXEC
                multiline */
             SELECT a FROM t /* where */ WHERE b > 0
           END-EXEC.
           STOP RUN.
  |};
  [%expect {| Esql: EXEC SQL SELECT a  FROM t WHERE b > 0 END-EXEC |}];;
