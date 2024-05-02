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
