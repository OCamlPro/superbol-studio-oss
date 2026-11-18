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

open Prog_printer

let dotest = Typeck_testing.show_diagnostics

let%expect_test "error-group-with-pic" =
  dotest @@ prog "error-group-with-pic"
    ~working_storage:{|
       01 A.
         02 A-2 PIC 9 VALUE 5.
         02 A-2 REDEFINES A-2 PIC 9.
           03 A-3 PIC 9.
    |};
  [%expect {|
    prog.cob:6.30-6.35:
       3          WORKING-STORAGE SECTION.
       4          01 A.
       5            02 A-2 PIC 9 VALUE 5.
       6 >          02 A-2 REDEFINES A-2 PIC 9.
    ----                                 ^^^^^
       7              03 A-3 PIC 9.
       8          PROCEDURE DIVISION.
    >> Error: Unexpected PICTURE clause for group item 'A-2'
  |}];;

let%expect_test "unexpected-redefines-target" =
  dotest @@ prog "unexpected-redefines-target"
    ~working_storage:{|
       01 A.
         02 A-1 PIC X.
         02 A-2 PIC 9 VALUE 5.
         02 A-24 REDEFINES A-3 PIC 9.
    |};
  [%expect {|
    prog.cob:7.27-7.30:
       4          01 A.
       5            02 A-1 PIC X.
       6            02 A-2 PIC 9 VALUE 5.
       7 >          02 A-24 REDEFINES A-3 PIC 9.
    ----                              ^^^
       8          PROCEDURE DIVISION.
       9
    >> Error: Unexpected target name 'A-3' in REDEFINES clause for item 'A-24';
              expected 'A-2'
  |}];;

let%expect_test "redefines-proc-arg" =
  dotest @@ prog "redefines-proc-arg"
    ~linkage:{|
       01 A.
         02 A-1 PIC X.
         02 A-2 REDEFINES A-1 PIC 9.
       01 B REDEFINES A PIC X.
    |}
    ~using:"A-2 B";
  [%expect {|
    prog.cob:8.32-8.35:
       5            02 A-1 PIC X.
       6            02 A-2 REDEFINES A-1 PIC 9.
       7          01 B REDEFINES A PIC X.
       8 >        PROCEDURE DIVISION USING A-2 B.
    ----                                   ^^^
       9
    >> Error: Data item 'A-2' with a REDEFINES clause is not allowed here
              (expecting a not-redefined 01- or 77-level data item)

    prog.cob:8.36-8.37:
       5            02 A-1 PIC X.
       6            02 A-2 REDEFINES A-1 PIC 9.
       7          01 B REDEFINES A PIC X.
       8 >        PROCEDURE DIVISION USING A-2 B.
    ----                                       ^
       9
    >> Error: Data item 'B' with a REDEFINES clause is not allowed here
              (expecting a not-redefined 01- or 77-level data item) |}];;

let%expect_test "duplicate-clause-in-redefinition" =
  dotest @@ prog "duplicate-clause-in-redefinition"
    ~working_storage:{|
       01 A PIC 9.
       01 B REDEFINES A PIC 9 PIC 9.
    |};
  [%expect {|
    prog.cob:5.30-5.35:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 A PIC 9.
       5 >        01 B REDEFINES A PIC 9 PIC 9.
    ----                                 ^^^^^
       6          PROCEDURE DIVISION.
       7
    >> Warning: Duplicate PICTURE clause |}];;

let options_with_free_redefines_position ?(dialect = Cobol_dialect.Id.default)
    level =
  let default_options = Prog_parser.options () in
  { default_options with
    config =
      let origin = Cobol_dialect.Origin.of_id dialect in
      { default_options.config with
        origin;
        free_redefines_position =
          Cobol_dialect.Feature.bind ~origin
            Cobol_config.Options.free_redefines_position level } }

let free_redefines_prog =
  prog "free-redefines"
    ~working_storage:{|
       01 A PIC 9.
       01 B PIC 9 REDEFINES A.
    |}

let%expect_test "free-redefines-position-default" =
  dotest free_redefines_prog;
  [%expect {|
    prog.cob:5.18-5.29:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 A PIC 9.
       5 >        01 B PIC 9 REDEFINES A.
    ----                     ^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    >> Warning: REDEFINES clause not following entry-name in definition used |}];;

let%expect_test "free-redefines-position-ok" =
  dotest free_redefines_prog
    ~parser_options:(options_with_free_redefines_position (Ok ()));
  [%expect {| |}];;

let%expect_test "free-redefines-position-unconformable" =
  dotest free_redefines_prog
    ~parser_options:(options_with_free_redefines_position Unconformable
                       ~dialect:Cobol_dialect.Id.cobol85);
  [%expect {|
    prog.cob:5.18-5.29:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          01 A PIC 9.
       5 >        01 B PIC 9 REDEFINES A.
    ----                     ^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    >> Error: REDEFINES clause not following entry-name in definition does not
              conform to cobol85 |}];;

let%expect_test "free-redefines-position-diagnostics-can-be-marshalled" =
  let parser_options =
    options_with_free_redefines_position Unconformable
      ~dialect:Cobol_dialect.Id.cobol85 in
  let options =
    Cobol_typeck.Config.from_cobol_config parser_options.config
      ~target_byte_order:Big_endian in
  let { result = ptree; _ }: _ Cobol_common.Diagnostics.with_diags =
    Prog_parser.parse ~parser_options free_redefines_prog |>
    Cobol_parser.Outputs.translate_diags in
  let Cobol_typeck.Results.{ diags; _ } =
    Cobol_typeck.compilation_group ptree ~options
      ~fold_exec_block':(fun ~data_definitions:_ _ acc -> acc) in
  Printf.printf "diagnostics: %d, marshalled: %b\n"
    (List.length diags)
    (String.length (Marshal.to_string diags []) > 0);
  [%expect {| diagnostics: 1, marshalled: true |}];;
