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
  Parser_testing.show_parsed_tokens {|
       PROGRAM-ID.        prog.
       PROCEDURE          DIVISION.
           EXEC SQL
              SELECT something_1, something_2 FROM some_table
                 WHERE condition > 0;
           END-EXEC.
           STOP RUN.
  |};
  [%expect {|
    PROGRAM-ID, ., INFO_WORD[prog], ., PROCEDURE, DIVISION, .,
    EXEC_BLOCK[EXEC SQL SELECT something_1 , something_2 FROM some_table WHERE
               condition > 0 ; END-EXEC],
    ., STOP, RUN, ., EOF
|}];;

let%expect_test "exec-block-with-invalid-percentage-character" =
  let exec_scanners =
    Superbol_preprocs.more [
      "NO-%", Superbol_preprocs.No_percentage_toy.scanner;
    ]
  in
  Parser_testing.show_diagnostics ~exec_scanners {|
       PROGRAM-ID.        prog.
       PROCEDURE          DIVISION.
           EXEC NO-%
              a % b
           END-EXEC.
           STOP RUN.
  |};
  [%expect {|
    prog.cob:5.16-5.17:
       2          PROGRAM-ID.        prog.
       3          PROCEDURE          DIVISION.
       4              EXEC NO-%
       5 >               a % b
    ----                   ^
       6              END-EXEC.
       7              STOP RUN.
    >> Error: Unexpected character `%' in no-percentage-allowed EXEC/END-EXEC
              block |}];;
