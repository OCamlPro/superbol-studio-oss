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

let pp_ptree =
  Fmt.option Cobol_ptree.pp_compilation_group
    ~none:(Fmt.any "None")

let show_ptree _i _ ptree _diags =
  Pretty.out "@[<v 2>Parse-tree:@;%a@]@\n@." pp_ptree ptree

let%expect_test "invalid-intrinsics-reregistration-on-reparse" =
  Parser_testing.iteratively_append_chunks ~f:show_ptree
    ~ignore_last_chunk:true @@
  Parser_testing.extract_position_markers
    ~with_start_pos:false ~with_end_pos:false @@
  {|
       >>SOURCE FORMAT IS FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. testintrinsic.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
       FUNCTION TRIM INTRINSIC.
       DATA DIVISION.
       PROCEDURE DIVISION.
       _|_ _|_ _|_ _|_ _|_ _|_ _|_ _|_
       STOP RUN.
  |};
  [%expect {|
    Appending chunk 1/6 @ 10:7-10:8 (" ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. testintrinsic.

      ENVIRONMENT DIVISION.
      CONFIGURATION SECTION.
      REPOSITORY.
      FUNCTION
      TRIM
      INTRINSIC.
      DATA DIVISION.
      PROCEDURE DIVISION.
        STOP RUN.

    Appending chunk 2/6 @ 10:8-10:9 (" ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. testintrinsic.

      ENVIRONMENT DIVISION.
      CONFIGURATION SECTION.
      REPOSITORY.
      FUNCTION
      TRIM
      INTRINSIC.
      DATA DIVISION.
      PROCEDURE DIVISION.
        STOP RUN.

    Appending chunk 3/6 @ 10:9-10:10 (" ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. testintrinsic.

      ENVIRONMENT DIVISION.
      CONFIGURATION SECTION.
      REPOSITORY.
      FUNCTION
      TRIM
      INTRINSIC.
      DATA DIVISION.
      PROCEDURE DIVISION.
        STOP RUN.

    Appending chunk 4/6 @ 10:10-10:11 (" ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. testintrinsic.

      ENVIRONMENT DIVISION.
      CONFIGURATION SECTION.
      REPOSITORY.
      FUNCTION
      TRIM
      INTRINSIC.
      DATA DIVISION.
      PROCEDURE DIVISION.
        STOP RUN.

    Appending chunk 5/6 @ 10:11-10:12 (" ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. testintrinsic.

      ENVIRONMENT DIVISION.
      CONFIGURATION SECTION.
      REPOSITORY.
      FUNCTION
      TRIM
      INTRINSIC.
      DATA DIVISION.
      PROCEDURE DIVISION.
        STOP RUN.

    Appending chunk 6/6 @ 10:12-10:13 (" ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. testintrinsic.

      ENVIRONMENT DIVISION.
      CONFIGURATION SECTION.
      REPOSITORY.
      FUNCTION
      TRIM
      INTRINSIC.
      DATA DIVISION.
      PROCEDURE DIVISION.
        STOP RUN. |}];;
