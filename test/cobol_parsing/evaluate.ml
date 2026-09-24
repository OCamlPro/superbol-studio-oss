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

(* Each test shows the diagnostics, then the structure of every EVALUATE.

   The structure is dumped explicitly because the pretty-printer cannot show how
   WHEN clauses are grouped into branches: it prints the same text whether two
   clauses share a branch or sit in two branches. *)

let prog stmts =
  Prog_printer.prog "prog"
    ~working_storage:{|
       01 A PIC 9 VALUE 1.
       01 B PIC 9 VALUE 2.
    |}
    ~procedure:stmts

let show_evaluates contents =
  match
    Prog_parser.parse ~source_format:(Cobol_config.SF SFFree) (prog contents) |>
    Cobol_parser.Outputs.show_n_forget ~set_status:false ~ppf:Fmt.stdout
      ~platform:Prog_common.platform
  with
  | Only None ->
      Pretty.out "Parse error@."
  | Only (Some cg) ->
      Cobol_ptree.Visitor.fold_compilation_group
        (object
          inherit [unit] Cobol_ptree.Visitor.folder
          method! fold_evaluate' { payload = ev; _ } acc =
            Pretty.out "@[<v>EVALUATE %a@,"
              Fmt.(list ~sep:(any " ALSO ") Cobol_ptree.pp_selection_subject)
              ev.eval_subjects;
            List.iter begin fun { Cobol_ptree.eval_selection; eval_actions } ->
              Pretty.out "  branch: %d clause(s) [%a], %d statement(s)@,"
                (List.length eval_selection)
                Fmt.(list ~sep:(any " | ")
                       (list ~sep:(any " ALSO ")
                          Cobol_ptree.pp_selection_object))
                eval_selection
                (List.length eval_actions)
            end ev.eval_branches;
            Pretty.out "  otherwise: %d statement(s)@,@]@."
              (List.length ev.eval_otherwise);
            Cobol_common.Visitor.skip_children acc
        end) cg ()

(* Standard COBOL: consecutive WHENs share the statements that follow, and so
   belong to a single branch.  This must not change. *)
let%expect_test "evaluate-fall-through-grouping" =
  show_evaluates
    {|EVALUATE A ALSO B
        WHEN 1 ALSO 2
        WHEN 3 ALSO 4
          DISPLAY "grouped"
        WHEN OTHER
          DISPLAY "other"
      END-EVALUATE.|};
  [%expect {|
    EVALUATE A ALSO B
      branch: 2 clause(s) [1 ALSO 2 | 3 ALSO 4], 1 statement(s)
      otherwise: 1 statement(s) |}];;

(* MF: the last WHEN phrase may carry no imperative statement. *)
let%expect_test "evaluate-empty-last-when" =
  show_evaluates
    {|EVALUATE A
        WHEN 1
          DISPLAY "one"
        WHEN 2
      END-EVALUATE.|};
  [%expect {|
    EVALUATE A
      branch: 1 clause(s) [1], 1 statement(s)
      branch: 1 clause(s) [2], 0 statement(s)
      otherwise: 0 statement(s) |}];;

(* MF: so may WHEN OTHER. *)
let%expect_test "evaluate-empty-when-other" =
  show_evaluates
    {|EVALUATE A
        WHEN 1
          DISPLAY "one"
        WHEN OTHER
      END-EVALUATE.|};
  [%expect {|
    EVALUATE A
      branch: 1 clause(s) [1], 1 statement(s)
      otherwise: 0 statement(s) |}];;

(* MF: WHEN phrases without imperative statement that directly precede WHEN
   OTHER are dropped -- they must NOT become empty branches, and must NOT fall
   through into the WHEN OTHER statements. *)
let%expect_test "evaluate-when-dropped-before-when-other" =
  show_evaluates
    {|EVALUATE A
        WHEN 1
          DISPLAY "one"
        WHEN 2
        WHEN 3
        WHEN OTHER
          DISPLAY "other"
      END-EVALUATE.|};
  [%expect {|
    prog.cob:10.8-10.14:
       7          EVALUATE A
       8           WHEN 1
       9             DISPLAY "one"
      10 >         WHEN 2
    ----           ^^^^^^
      11           WHEN 3
      12           WHEN OTHER
    >> Warning: Fall-through to WHEN OTHER: this WHEN phrase has no imperative
                statement and is ignored

    prog.cob:11.8-11.14:
       8           WHEN 1
       9             DISPLAY "one"
      10           WHEN 2
      11 >         WHEN 3
    ----           ^^^^^^
      12           WHEN OTHER
      13             DISPLAY "other"
    >> Warning: Fall-through to WHEN OTHER: this WHEN phrase has no imperative
                statement and is ignored

    EVALUATE A
      branch: 1 clause(s) [1], 1 statement(s)
      otherwise: 1 statement(s) |}];;

(* MF: an EVALUATE whose only phrase is WHEN OTHER. *)
let%expect_test "evaluate-only-when-other" =
  show_evaluates
    {|EVALUATE A
        WHEN OTHER
          DISPLAY "only-other"
      END-EVALUATE.|};
  [%expect {|
    prog.cob:8.8-8.18:
       5          01 B PIC 9 VALUE 2.
       6          PROCEDURE DIVISION.
       7          EVALUATE A
       8 >         WHEN OTHER
    ----           ^^^^^^^^^^
       9             DISPLAY "only-other"
      10         END-EVALUATE.
    >> Warning: No WHEN branch before WHEN OTHER

    EVALUATE A
      otherwise: 1 statement(s) |}];;

(* The two relaxations combined, with the EVALUATE closed by a period rather
   than END-EVALUATE. *)
let%expect_test "evaluate-dropped-when-then-empty-when-other-no-terminator" =
  show_evaluates
    {|EVALUATE A
        WHEN 1
        WHEN OTHER.|};
  [%expect {|
    prog.cob:8.8-8.14:
       5          01 B PIC 9 VALUE 2.
       6          PROCEDURE DIVISION.
       7          EVALUATE A
       8 >         WHEN 1
    ----           ^^^^^^
       9           WHEN OTHER.
      10
    >> Warning: Fall-through to WHEN OTHER: this WHEN phrase has no imperative
                statement and is ignored

    prog.cob:9.8-9.18:
       6          PROCEDURE DIVISION.
       7          EVALUATE A
       8           WHEN 1
       9 >         WHEN OTHER.
    ----           ^^^^^^^^^^
      10
    >> Warning: No WHEN branch before WHEN OTHER

    EVALUATE A
      otherwise: 0 statement(s) |}];;
