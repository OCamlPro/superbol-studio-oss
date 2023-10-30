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

(** This set of tests exercises the rewinder by iteratively appending chunks of
    text in a "stuttering way". Each chunk consists of two potions of text that
    span between three successive position markers "_|_" below.  Chuck is first
    appended whole, then cut back to the intermediate marker.  The parse-tree at
    the end of should correspond to the input program text. *)

open Test_appending

(* --- *)

let%expect_test "line-by-line-incremental-small-1" =
  Parser_testing.iteratively_append_chunks_stuttering ~f:show_ptree @@
  Parser_testing.extract_position_markers {|
_|_        IDENTIFICATION DIVISION.
_|_        PROGRAM-ID. prog.
_|_        PROCEDURE DIVISION.
_|_            STOP RUN.
  |};
  [%expect  {|
    Appending chunk 1/4 @ 0:0-2:0 ("\n        IDENTIFICATION D...)
    Parse-tree:
      None

    Cutting chunk 1/4 back @ 0:0-1:0 ("\n")
    Parse-tree:


    Appending chunk 2/4 @ 1:0-3:0 ("        IDENTIFICATION DIV...)
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Cutting chunk 2/4 back @ 1:0-2:0 ("        IDENTIFICATION DIV...)
    Parse-tree:
      None

    Appending chunk 3/4 @ 2:0-4:0 ("        PROGRAM-ID. prog.\...)
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 3/4 back @ 2:0-3:0 ("        PROGRAM-ID. prog.\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Appending chunk 4/4 @ 3:0-5:2 ("        PROCEDURE DIVISION...)
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN.
|}];;

let%expect_test "line-by-line-incremental-small-2" =
  Parser_testing.iteratively_append_chunks_stuttering ~f:show_ptree @@
  Parser_testing.extract_position_markers {|
       _|_ IDENTIFICATION DIVISION.
       _|_ PROGRAM-ID. prog.
       _|_ PROCEDURE DIVISION.
       _|_     STOP RUN.
  |};
  [%expect  {|
    Appending chunk 1/4 @ 0:0-2:7 ("\n        IDENTIFICATION D...)
    Parse-tree:
      None

    Cutting chunk 1/4 back @ 0:0-1:7 ("\n       ")
    Parse-tree:


    Appending chunk 2/4 @ 1:7-3:7 (" IDENTIFICATION DIVISION.\...)
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Cutting chunk 2/4 back @ 1:7-2:7 (" IDENTIFICATION DIVISION.\...)
    Parse-tree:
      None

    Appending chunk 3/4 @ 2:7-4:7 (" PROGRAM-ID. prog.\n      ...)
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 3/4 back @ 2:7-3:7 (" PROGRAM-ID. prog.\n       ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Appending chunk 4/4 @ 3:7-5:2 (" PROCEDURE DIVISION.\n    ...)
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN.
|}];;

let%expect_test "line-by-line-incremental-small-3" =
  Parser_testing.iteratively_append_chunks_stuttering ~f:show_ptree @@
  Parser_testing.extract_position_markers {|
_|_        IDENTIFICATION DIVISION._|_
_|_        PROGRAM-ID. prog._|_
_|_        PROCEDURE DIVISION._|_
_|_            STOP RUN._|_
  |};
  [%expect  {|
    Appending chunk 1/8 @ 0:0-1:32 ("\n        IDENTIFICATION D...)
    Parse-tree:
      None

    Cutting chunk 1/8 back @ 0:0-1:0 ("\n")
    Parse-tree:


    Appending chunk 2/8 @ 1:0-2:0 ("        IDENTIFICATION DIV...)
    Parse-tree:
      None

    Cutting chunk 2/8 back @ 1:0-1:32 ("        IDENTIFICATION DIV...)
    Parse-tree:
      None

    Appending chunk 3/8 @ 1:32-2:25 ("\n        PROGRAM-ID. prog.")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Cutting chunk 3/8 back @ 1:32-2:0 ("\n")
    Parse-tree:
      None

    Appending chunk 4/8 @ 2:0-3:0 ("        PROGRAM-ID. prog.\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Cutting chunk 4/8 back @ 2:0-2:25 ("        PROGRAM-ID. prog.")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Appending chunk 5/8 @ 2:25-3:27 ("\n        PROCEDURE DIVISI...)
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 5/8 back @ 2:25-3:0 ("\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Appending chunk 6/8 @ 3:0-4:0 ("        PROCEDURE DIVISION...)
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 6/8 back @ 3:0-3:27 ("        PROCEDURE DIVISION.")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 7/8 @ 3:27-4:21 ("\n            STOP RUN.")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN.

    Cutting chunk 7/8 back @ 3:27-4:0 ("\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 8/8 @ 4:0-5:2 ("            STOP RUN.\n  ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN.
|}];;

let%expect_test "line-by-line-incremental-small-4" =
  Parser_testing.iteratively_append_chunks_stuttering ~f:show_ptree @@
  Parser_testing.extract_position_markers @@
  Parser_testing.insert_periodic_position_markers ~period:5 {|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        PROCEDURE DIVISION.
            STOP RUN.
  |};
  [%expect  {|
    Appending chunk 1/22 @ 0:0-1:9 ("\n        I")
    Parse-tree:
      None

    Cutting chunk 1/22 back @ 0:0-1:4 ("\n    ")
    Parse-tree:


    Appending chunk 2/22 @ 1:4-1:14 ("    IDENTI")
    Parse-tree:
      None

    Cutting chunk 2/22 back @ 1:4-1:9 ("    I")
    Parse-tree:
      None

    Appending chunk 3/22 @ 1:9-1:19 ("DENTIFICAT")
    Parse-tree:
      None

    Cutting chunk 3/22 back @ 1:9-1:14 ("DENTI")
    Parse-tree:
      None

    Appending chunk 4/22 @ 1:14-1:24 ("FICATION D")
    Parse-tree:
      None

    Cutting chunk 4/22 back @ 1:14-1:19 ("FICAT")
    Parse-tree:
      None

    Appending chunk 5/22 @ 1:19-1:29 ("ION DIVISI")
    Parse-tree:
      None

    Cutting chunk 5/22 back @ 1:19-1:24 ("ION D")
    Parse-tree:
      None

    Appending chunk 6/22 @ 1:24-2:1 ("IVISION.\n ")
    Parse-tree:
      None

    Cutting chunk 6/22 back @ 1:24-1:29 ("IVISI")
    Parse-tree:
      None

    Appending chunk 7/22 @ 1:29-2:6 ("ON.\n      ")
    Parse-tree:
      None

    Cutting chunk 7/22 back @ 1:29-2:1 ("ON.\n ")
    Parse-tree:
      None

    Appending chunk 8/22 @ 2:1-2:11 ("       PRO")
    Parse-tree:
      None

    Cutting chunk 8/22 back @ 2:1-2:6 ("     ")
    Parse-tree:
      None

    Appending chunk 9/22 @ 2:6-2:16 ("  PROGRAM-")
    Parse-tree:
      None

    Cutting chunk 9/22 back @ 2:6-2:11 ("  PRO")
    Parse-tree:
      None

    Appending chunk 10/22 @ 2:11-2:21 ("GRAM-ID. p")
    Parse-tree:
      None

    Cutting chunk 10/22 back @ 2:11-2:16 ("GRAM-")
    Parse-tree:
      None

    Appending chunk 11/22 @ 2:16-3:0 ("ID. prog.\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Cutting chunk 11/22 back @ 2:16-2:21 ("ID. p")
    Parse-tree:
      None

    Appending chunk 12/22 @ 2:21-3:5 ("rog.\n     ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Cutting chunk 12/22 back @ 2:21-3:0 ("rog.\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Appending chunk 13/22 @ 3:0-3:10 ("        PR")
    Parse-tree:
      None

    Cutting chunk 13/22 back @ 3:0-3:5 ("     ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.


    Appending chunk 14/22 @ 3:5-3:15 ("   PROCEDU")
    Parse-tree:
      None

    Cutting chunk 14/22 back @ 3:5-3:10 ("   PR")
    Parse-tree:
      None

    Appending chunk 15/22 @ 3:10-3:20 ("OCEDURE DI")
    Parse-tree:
      None

    Cutting chunk 15/22 back @ 3:10-3:15 ("OCEDU")
    Parse-tree:
      None

    Appending chunk 16/22 @ 3:15-3:25 ("RE DIVISIO")
    Parse-tree:
      None

    Cutting chunk 16/22 back @ 3:15-3:20 ("RE DI")
    Parse-tree:
      None

    Appending chunk 17/22 @ 3:20-4:2 ("VISION.\n  ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 17/22 back @ 3:20-3:25 ("VISIO")
    Parse-tree:
      None

    Appending chunk 18/22 @ 3:25-4:7 ("N.\n       ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 18/22 back @ 3:25-4:2 ("N.\n  ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 19/22 @ 4:2-4:12 ("          ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 19/22 back @ 4:2-4:7 ("     ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 20/22 @ 4:7-4:17 ("     STOP ")
    Parse-tree:
      None

    Cutting chunk 20/22 back @ 4:7-4:12 ("     ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 21/22 @ 4:12-5:0 ("STOP RUN.\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN.

    Cutting chunk 21/22 back @ 4:12-4:17 ("STOP ")
    Parse-tree:
      None

    Appending chunk 22/22 @ 4:17-5:2 ("RUN.\n  ")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN.
|}];;
