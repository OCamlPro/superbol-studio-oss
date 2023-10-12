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

let show_ptree _i _n ptree (* _art *) _diags =
  (* let Cobol_parser.Outputs.{ tokens; _ } = art in *)
  (* Pretty.out "@[<h 2>%a@]@." Cobol_parser.INTERNAL.pp_tokens (Lazy.force tokens); *)
  Pretty.out "@[<v 2>Parse-tree:@;%a@]@\n@." pp_ptree ptree

(* --- *)

let%expect_test "line-by-line-incremental-small-1" =
  Parser_testing.iteratively_append_chunks ~f:show_ptree @@
  Parser_testing.extract_position_markers {|
_|_        IDENTIFICATION DIVISION.
_|_        PROGRAM-ID. prog.
_|_        PROCEDURE DIVISION.
_|_            STOP RUN.
  |};
  [%expect  {|
    Appending chunk 1/5 @ 0:0-1:0 ("\n")
    Parse-tree:


    Appending chunk 2/5 @ 1:0-2:0 ("        IDENTIFICATION DIV...)
    Parse-tree:
      None

    Appending chunk 3/5 @ 2:0-3:0 ("        PROGRAM-ID. prog.\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Appending chunk 4/5 @ 3:0-4:0 ("        PROCEDURE DIVISION...)
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 5/5 @ 4:0-5:2 ("            STOP RUN.\n  ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN.
|}];;

let%expect_test "line-by-line-incremental-small-2" =
  Parser_testing.iteratively_append_chunks ~f:show_ptree @@
  Parser_testing.extract_position_markers {|
       _|_ IDENTIFICATION DIVISION.
       _|_ PROGRAM-ID. prog.
       _|_ PROCEDURE DIVISION.
       _|_     STOP RUN.
  |};
  [%expect  {|
    Appending chunk 1/5 @ 0:0-1:7 ("\n       ")
    Parse-tree:


    Appending chunk 2/5 @ 1:7-2:7 (" IDENTIFICATION DIVISION.\...)
    Parse-tree:
      None

    Appending chunk 3/5 @ 2:7-3:7 (" PROGRAM-ID. prog.\n       ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Appending chunk 4/5 @ 3:7-4:7 (" PROCEDURE DIVISION.\n    ...)
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 5/5 @ 4:7-5:2 ("     STOP RUN.\n  ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN.
|}];;

let%expect_test "line-by-line-incremental-small-3" =
  Parser_testing.iteratively_append_chunks ~f:show_ptree @@
  Parser_testing.extract_position_markers {|
_|_        IDENTIFICATION DIVISION._|_
_|_        PROGRAM-ID. prog._|_
_|_        PROCEDURE DIVISION._|_
_|_            STOP RUN._|_
  |};
  [%expect  {|
    Appending chunk 1/9 @ 0:0-1:0 ("\n")
    Parse-tree:


    Appending chunk 2/9 @ 1:0-1:32 ("        IDENTIFICATION DIV...)
    Parse-tree:
      None

    Appending chunk 3/9 @ 1:32-2:0 ("\n")
    Parse-tree:
      None

    Appending chunk 4/9 @ 2:0-2:25 ("        PROGRAM-ID. prog.")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Appending chunk 5/9 @ 2:25-3:0 ("\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Appending chunk 6/9 @ 3:0-3:27 ("        PROCEDURE DIVISION.")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 7/9 @ 3:27-4:0 ("\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 8/9 @ 4:0-4:21 ("            STOP RUN.")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN.

    Appending chunk 9/9 @ 4:21-5:2 ("\n  ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN.
|}];;

let%expect_test "line-by-line-incremental-small-4" =
  Parser_testing.iteratively_append_chunks ~f:show_ptree @@
  Parser_testing.extract_position_markers @@
  Parser_testing.insert_periodic_position_markers ~period:5 {|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        PROCEDURE DIVISION.
            STOP RUN.
  |};
  [%expect  {|
    Appending chunk 1/23 @ 0:0-1:4 ("\n    ")
    Parse-tree:


    Appending chunk 2/23 @ 1:4-1:9 ("    I")
    Parse-tree:
      None

    Appending chunk 3/23 @ 1:9-1:14 ("DENTI")
    Parse-tree:
      None

    Appending chunk 4/23 @ 1:14-1:19 ("FICAT")
    Parse-tree:
      None

    Appending chunk 5/23 @ 1:19-1:24 ("ION D")
    Parse-tree:
      None

    Appending chunk 6/23 @ 1:24-1:29 ("IVISI")
    Parse-tree:
      None

    Appending chunk 7/23 @ 1:29-2:1 ("ON.\n ")
    Parse-tree:
      None

    Appending chunk 8/23 @ 2:1-2:6 ("     ")
    Parse-tree:
      None

    Appending chunk 9/23 @ 2:6-2:11 ("  PRO")
    Parse-tree:
      None

    Appending chunk 10/23 @ 2:11-2:16 ("GRAM-")
    Parse-tree:
      None

    Appending chunk 11/23 @ 2:16-2:21 ("ID. p")
    Parse-tree:
      None

    Appending chunk 12/23 @ 2:21-3:0 ("rog.\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Appending chunk 13/23 @ 3:0-3:5 ("     ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Appending chunk 14/23 @ 3:5-3:10 ("   PR")
    Parse-tree:
      None

    Appending chunk 15/23 @ 3:10-3:15 ("OCEDU")
    Parse-tree:
      None

    Appending chunk 16/23 @ 3:15-3:20 ("RE DI")
    Parse-tree:
      None

    Appending chunk 17/23 @ 3:20-3:25 ("VISIO")
    Parse-tree:
      None

    Appending chunk 18/23 @ 3:25-4:2 ("N.\n  ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 19/23 @ 4:2-4:7 ("     ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 20/23 @ 4:7-4:12 ("     ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 21/23 @ 4:12-4:17 ("STOP ")
    Parse-tree:
      None

    Appending chunk 22/23 @ 4:17-5:0 ("RUN.\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN.

    Appending chunk 23/23 @ 5:0-5:2 ("  ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN.
|}];;
