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

(* Check how cobol_typeck behaves in the presence of syntax errors *)

open Prog_printer

let dotest = Typeck_testing.show_diagnostics

let%expect_test "bad-qualifiers" =
  dotest @@ prog "bad-qualifiers"
    ~working_storage:{|
       77 A PIC X.
    |}
    ~procedure:{|
       MAIN.
           DISPLAY A IN
           PERFORM S IN.
           PERFORM S IN IN MAIN
           PERFORM IN S.
    |};
  [%expect {|
    prog.cob:7.23:
       4          77 A PIC X.
       5          PROCEDURE DIVISION.
       6          MAIN.
       7 >            DISPLAY A IN
    ----                          ^
       8              PERFORM S IN.
       9              PERFORM S IN IN MAIN
    >> Hint: Missing <qualified name>

    prog.cob:8.11-8.18:
       5          PROCEDURE DIVISION.
       6          MAIN.
       7              DISPLAY A IN
       8 >            PERFORM S IN.
    ----              ^^^^^^^
       9              PERFORM S IN IN MAIN
      10              PERFORM IN S.
    >> Error: Invalid syntax

    prog.cob:8.23:
       5          PROCEDURE DIVISION.
       6          MAIN.
       7              DISPLAY A IN
       8 >            PERFORM S IN.
    ----                          ^
       9              PERFORM S IN IN MAIN
      10              PERFORM IN S.
    >> Hint: Missing <qualified name>

    prog.cob:8.23-8.24:
       5          PROCEDURE DIVISION.
       6          MAIN.
       7              DISPLAY A IN
       8 >            PERFORM S IN.
    ----                          ^
       9              PERFORM S IN IN MAIN
      10              PERFORM IN S.
    >> Error: Invalid syntax

    prog.cob:9.24-9.26:
       6          MAIN.
       7              DISPLAY A IN
       8              PERFORM S IN.
       9 >            PERFORM S IN IN MAIN
    ----                           ^^
      10              PERFORM IN S.
      11
    >> Error: Invalid syntax

    prog.cob:10.19-10.21:
       7              DISPLAY A IN
       8              PERFORM S IN.
       9              PERFORM S IN IN MAIN
      10 >            PERFORM IN S.
    ----                      ^^
      11
    >> Error: Invalid syntax

    prog.cob:8.19-8.23:
       5          PROCEDURE DIVISION.
       6          MAIN.
       7              DISPLAY A IN
       8 >            PERFORM S IN.
    ----                      ^^^^
       9              PERFORM S IN IN MAIN
      10              PERFORM IN S.
    >> Error: Unknown procedure-name 'S'

    prog.cob:9.19-9.31:
       6          MAIN.
       7              DISPLAY A IN
       8              PERFORM S IN.
       9 >            PERFORM S IN IN MAIN
    ----                      ^^^^^^^^^^^^
      10              PERFORM IN S.
      11
    >> Error: Unknown procedure-name 'S IN MAIN'

    prog.cob:10.22-10.23:
       7              DISPLAY A IN
       8              PERFORM S IN.
       9              PERFORM S IN IN MAIN
      10 >            PERFORM IN S.
    ----                         ^
      11
    >> Error: Unknown procedure-name 'S' |}];;
