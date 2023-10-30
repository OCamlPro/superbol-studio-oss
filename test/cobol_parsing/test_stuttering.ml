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

let%expect_test "chunk-by-chunk-incremental-small-5" =
  Parser_testing.iteratively_append_chunks_stuttering ~f:show_ptree @@
  Parser_testing.extract_position_markers @@
  Parser_testing.insert_periodic_position_markers ~period:1
    "   \n\
    \ \n\
    \\tIDENTIFICATION DIVISION.\n\
    \       PROGRAM-ID. prog.\n\
    \       PROCEDURE\tDIVISION.\n\
    \          STOP RUN.";
  [%expect{|
    Appending chunk 1/103 @ 0:0-0:2 ("  ")
    Parse-tree:


    Cutting chunk 1/103 back @ 0:0-0:1 (" ")
    Parse-tree:


    Appending chunk 2/103 @ 0:1-0:3 ("  ")
    Parse-tree:


    Cutting chunk 2/103 back @ 0:1-0:2 (" ")
    Parse-tree:


    Appending chunk 3/103 @ 0:2-1:0 (" \n")
    Parse-tree:


    Cutting chunk 3/103 back @ 0:2-0:3 (" ")
    Parse-tree:


    Appending chunk 4/103 @ 0:3-1:1 ("\n ")
    Parse-tree:


    Cutting chunk 4/103 back @ 0:3-1:0 ("\n")
    Parse-tree:


    Appending chunk 5/103 @ 1:0-2:0 (" \n")
    Parse-tree:


    Cutting chunk 5/103 back @ 1:0-1:1 (" ")
    Parse-tree:


    Appending chunk 6/103 @ 1:1-2:1 ("\n\\")
    Parse-tree:


    Cutting chunk 6/103 back @ 1:1-2:0 ("\n")
    Parse-tree:


    Appending chunk 7/103 @ 2:0-2:2 ("\\t")
    Parse-tree:


    Cutting chunk 7/103 back @ 2:0-2:1 ("\\")
    Parse-tree:


    Appending chunk 8/103 @ 2:1-2:3 ("tI")
    Parse-tree:


    Cutting chunk 8/103 back @ 2:1-2:2 ("t")
    Parse-tree:


    Appending chunk 9/103 @ 2:2-2:4 ("ID")
    Parse-tree:


    Cutting chunk 9/103 back @ 2:2-2:3 ("I")
    Parse-tree:


    Appending chunk 10/103 @ 2:3-2:5 ("DE")
    Parse-tree:


    Cutting chunk 10/103 back @ 2:3-2:4 ("D")
    Parse-tree:


    Appending chunk 11/103 @ 2:4-2:6 ("EN")
    Parse-tree:


    Cutting chunk 11/103 back @ 2:4-2:5 ("E")
    Parse-tree:


    Appending chunk 12/103 @ 2:5-2:7 ("NT")
    Parse-tree:


    Cutting chunk 12/103 back @ 2:5-2:6 ("N")
    Parse-tree:


    Appending chunk 13/103 @ 2:6-2:8 ("TI")
    Parse-tree:
      None

    Cutting chunk 13/103 back @ 2:6-2:7 ("T")
    Parse-tree:


    Appending chunk 14/103 @ 2:7-2:9 ("IF")
    Parse-tree:
      None

    Cutting chunk 14/103 back @ 2:7-2:8 ("I")
    Parse-tree:
      None

    Appending chunk 15/103 @ 2:8-2:10 ("FI")
    Parse-tree:
      None

    Cutting chunk 15/103 back @ 2:8-2:9 ("F")
    Parse-tree:
      None

    Appending chunk 16/103 @ 2:9-2:11 ("IC")
    Parse-tree:
      None

    Cutting chunk 16/103 back @ 2:9-2:10 ("I")
    Parse-tree:
      None

    Appending chunk 17/103 @ 2:10-2:12 ("CA")
    Parse-tree:
      None

    Cutting chunk 17/103 back @ 2:10-2:11 ("C")
    Parse-tree:
      None

    Appending chunk 18/103 @ 2:11-2:13 ("AT")
    Parse-tree:
      None

    Cutting chunk 18/103 back @ 2:11-2:12 ("A")
    Parse-tree:
      None

    Appending chunk 19/103 @ 2:12-2:14 ("TI")
    Parse-tree:
      None

    Cutting chunk 19/103 back @ 2:12-2:13 ("T")
    Parse-tree:
      None

    Appending chunk 20/103 @ 2:13-2:15 ("IO")
    Parse-tree:
      None

    Cutting chunk 20/103 back @ 2:13-2:14 ("I")
    Parse-tree:
      None

    Appending chunk 21/103 @ 2:14-2:16 ("ON")
    Parse-tree:
      None

    Cutting chunk 21/103 back @ 2:14-2:15 ("O")
    Parse-tree:
      None

    Appending chunk 22/103 @ 2:15-2:17 ("N ")
    Parse-tree:
      None

    Cutting chunk 22/103 back @ 2:15-2:16 ("N")
    Parse-tree:
      None

    Appending chunk 23/103 @ 2:16-2:18 (" D")
    Parse-tree:
      None

    Cutting chunk 23/103 back @ 2:16-2:17 (" ")
    Parse-tree:
      None

    Appending chunk 24/103 @ 2:17-2:19 ("DI")
    Parse-tree:
      None

    Cutting chunk 24/103 back @ 2:17-2:18 ("D")
    Parse-tree:
      None

    Appending chunk 25/103 @ 2:18-2:20 ("IV")
    Parse-tree:
      None

    Cutting chunk 25/103 back @ 2:18-2:19 ("I")
    Parse-tree:
      None

    Appending chunk 26/103 @ 2:19-2:21 ("VI")
    Parse-tree:
      None

    Cutting chunk 26/103 back @ 2:19-2:20 ("V")
    Parse-tree:
      None

    Appending chunk 27/103 @ 2:20-2:22 ("IS")
    Parse-tree:
      None

    Cutting chunk 27/103 back @ 2:20-2:21 ("I")
    Parse-tree:
      None

    Appending chunk 28/103 @ 2:21-2:23 ("SI")
    Parse-tree:
      None

    Cutting chunk 28/103 back @ 2:21-2:22 ("S")
    Parse-tree:
      None

    Appending chunk 29/103 @ 2:22-2:24 ("IO")
    Parse-tree:
      None

    Cutting chunk 29/103 back @ 2:22-2:23 ("I")
    Parse-tree:
      None

    Appending chunk 30/103 @ 2:23-2:25 ("ON")
    Parse-tree:
      None

    Cutting chunk 30/103 back @ 2:23-2:24 ("O")
    Parse-tree:
      None

    Appending chunk 31/103 @ 2:24-2:26 ("N.")
    Parse-tree:
      None

    Cutting chunk 31/103 back @ 2:24-2:25 ("N")
    Parse-tree:
      None

    Appending chunk 32/103 @ 2:25-3:0 (".\n")
    Parse-tree:
      None

    Cutting chunk 32/103 back @ 2:25-2:26 (".")
    Parse-tree:
      None

    Appending chunk 33/103 @ 2:26-3:1 ("\n ")
    Parse-tree:
      None

    Cutting chunk 33/103 back @ 2:26-3:0 ("\n")
    Parse-tree:


    Appending chunk 34/103 @ 3:0-3:2 ("  ")
    Parse-tree:


    Cutting chunk 34/103 back @ 3:0-3:1 (" ")
    Parse-tree:
      None

    Appending chunk 35/103 @ 3:1-3:3 ("  ")
    Parse-tree:


    Cutting chunk 35/103 back @ 3:1-3:2 (" ")
    Parse-tree:
      None

    Appending chunk 36/103 @ 3:2-3:4 ("  ")
    Parse-tree:


    Cutting chunk 36/103 back @ 3:2-3:3 (" ")
    Parse-tree:
      None

    Appending chunk 37/103 @ 3:3-3:5 ("  ")
    Parse-tree:


    Cutting chunk 37/103 back @ 3:3-3:4 (" ")
    Parse-tree:
      None

    Appending chunk 38/103 @ 3:4-3:6 ("  ")
    Parse-tree:


    Cutting chunk 38/103 back @ 3:4-3:5 (" ")
    Parse-tree:
      None

    Appending chunk 39/103 @ 3:5-3:7 ("  ")
    Parse-tree:


    Cutting chunk 39/103 back @ 3:5-3:6 (" ")
    Parse-tree:
      None

    Appending chunk 40/103 @ 3:6-3:8 (" P")
    Parse-tree:
      None

    Cutting chunk 40/103 back @ 3:6-3:7 (" ")
    Parse-tree:
      None

    Appending chunk 41/103 @ 3:7-3:9 ("PR")
    Parse-tree:
      None

    Cutting chunk 41/103 back @ 3:7-3:8 ("P")
    Parse-tree:
      None

    Appending chunk 42/103 @ 3:8-3:10 ("RO")
    Parse-tree:
      None

    Cutting chunk 42/103 back @ 3:8-3:9 ("R")
    Parse-tree:
      None

    Appending chunk 43/103 @ 3:9-3:11 ("OG")
    Parse-tree:
      None

    Cutting chunk 43/103 back @ 3:9-3:10 ("O")
    Parse-tree:
      None

    Appending chunk 44/103 @ 3:10-3:12 ("GR")
    Parse-tree:
      None

    Cutting chunk 44/103 back @ 3:10-3:11 ("G")
    Parse-tree:
      None

    Appending chunk 45/103 @ 3:11-3:13 ("RA")
    Parse-tree:
      None

    Cutting chunk 45/103 back @ 3:11-3:12 ("R")
    Parse-tree:
      None

    Appending chunk 46/103 @ 3:12-3:14 ("AM")
    Parse-tree:
      None

    Cutting chunk 46/103 back @ 3:12-3:13 ("A")
    Parse-tree:
      None

    Appending chunk 47/103 @ 3:13-3:15 ("M-")
    Parse-tree:
      None

    Cutting chunk 47/103 back @ 3:13-3:14 ("M")
    Parse-tree:
      None

    Appending chunk 48/103 @ 3:14-3:16 ("-I")
    Parse-tree:
      None

    Cutting chunk 48/103 back @ 3:14-3:15 ("-")
    Parse-tree:
      None

    Appending chunk 49/103 @ 3:15-3:17 ("ID")
    Parse-tree:
      None

    Cutting chunk 49/103 back @ 3:15-3:16 ("I")
    Parse-tree:
      None

    Appending chunk 50/103 @ 3:16-3:18 ("D.")
    Parse-tree:
      None

    Cutting chunk 50/103 back @ 3:16-3:17 ("D")
    Parse-tree:
      None

    Appending chunk 51/103 @ 3:17-3:19 (". ")
    Parse-tree:
      None

    Cutting chunk 51/103 back @ 3:17-3:18 (".")
    Parse-tree:
      None

    Appending chunk 52/103 @ 3:18-3:20 (" p")
    Parse-tree:
      None

    Cutting chunk 52/103 back @ 3:18-3:19 (" ")
    Parse-tree:
      None

    Appending chunk 53/103 @ 3:19-3:21 ("pr")
    Parse-tree:
      None

    Cutting chunk 53/103 back @ 3:19-3:20 ("p")
    Parse-tree:
      None

    Appending chunk 54/103 @ 3:20-3:22 ("ro")
    Parse-tree:
      None

    Cutting chunk 54/103 back @ 3:20-3:21 ("r")
    Parse-tree:
      None

    Appending chunk 55/103 @ 3:21-3:23 ("og")
    Parse-tree:
      None

    Cutting chunk 55/103 back @ 3:21-3:22 ("o")
    Parse-tree:
      None

    Appending chunk 56/103 @ 3:22-3:24 ("g.")
    Parse-tree:
      PROGRAM-ID. PROG.


    Cutting chunk 56/103 back @ 3:22-3:23 ("g")
    Parse-tree:
      None

    Appending chunk 57/103 @ 3:23-4:0 (".\n")
    Parse-tree:
      PROGRAM-ID. PROG.


    Cutting chunk 57/103 back @ 3:23-3:24 (".")
    Parse-tree:
      None

    Appending chunk 58/103 @ 3:24-4:1 ("\n ")
    Parse-tree:
      PROGRAM-ID. PROG.


    Cutting chunk 58/103 back @ 3:24-4:0 ("\n")
    Parse-tree:
      PROGRAM-ID. PROG.


    Appending chunk 59/103 @ 4:0-4:2 ("  ")
    Parse-tree:
      PROGRAM-ID. PROG.


    Cutting chunk 59/103 back @ 4:0-4:1 (" ")
    Parse-tree:
      None

    Appending chunk 60/103 @ 4:1-4:3 ("  ")
    Parse-tree:
      PROGRAM-ID. PROG.


    Cutting chunk 60/103 back @ 4:1-4:2 (" ")
    Parse-tree:
      PROGRAM-ID. PROG.


    Appending chunk 61/103 @ 4:2-4:4 ("  ")
    Parse-tree:
      None

    Cutting chunk 61/103 back @ 4:2-4:3 (" ")
    Parse-tree:
      PROGRAM-ID. PROG.


    Appending chunk 62/103 @ 4:3-4:5 ("  ")
    Parse-tree:
      PROGRAM-ID. PROG.


    Cutting chunk 62/103 back @ 4:3-4:4 (" ")
    Parse-tree:
      None

    Appending chunk 63/103 @ 4:4-4:6 ("  ")
    Parse-tree:
      PROGRAM-ID. PROG.


    Cutting chunk 63/103 back @ 4:4-4:5 (" ")
    Parse-tree:
      PROGRAM-ID. PROG.


    Appending chunk 64/103 @ 4:5-4:7 ("  ")
    Parse-tree:
      None

    Cutting chunk 64/103 back @ 4:5-4:6 (" ")
    Parse-tree:
      PROGRAM-ID. PROG.


    Appending chunk 65/103 @ 4:6-4:8 (" P")
    Parse-tree:
      None

    Cutting chunk 65/103 back @ 4:6-4:7 (" ")
    Parse-tree:
      None

    Appending chunk 66/103 @ 4:7-4:9 ("PR")
    Parse-tree:
      None

    Cutting chunk 66/103 back @ 4:7-4:8 ("P")
    Parse-tree:
      None

    Appending chunk 67/103 @ 4:8-4:10 ("RO")
    Parse-tree:
      None

    Cutting chunk 67/103 back @ 4:8-4:9 ("R")
    Parse-tree:
      None

    Appending chunk 68/103 @ 4:9-4:11 ("OC")
    Parse-tree:
      None

    Cutting chunk 68/103 back @ 4:9-4:10 ("O")
    Parse-tree:
      None

    Appending chunk 69/103 @ 4:10-4:12 ("CE")
    Parse-tree:
      None

    Cutting chunk 69/103 back @ 4:10-4:11 ("C")
    Parse-tree:
      None

    Appending chunk 70/103 @ 4:11-4:13 ("ED")
    Parse-tree:
      None

    Cutting chunk 70/103 back @ 4:11-4:12 ("E")
    Parse-tree:
      None

    Appending chunk 71/103 @ 4:12-4:14 ("DU")
    Parse-tree:
      None

    Cutting chunk 71/103 back @ 4:12-4:13 ("D")
    Parse-tree:
      None

    Appending chunk 72/103 @ 4:13-4:15 ("UR")
    Parse-tree:
      None

    Cutting chunk 72/103 back @ 4:13-4:14 ("U")
    Parse-tree:
      None

    Appending chunk 73/103 @ 4:14-4:16 ("RE")
    Parse-tree:
      None

    Cutting chunk 73/103 back @ 4:14-4:15 ("R")
    Parse-tree:
      None

    Appending chunk 74/103 @ 4:15-4:17 ("E\t")
    Parse-tree:
      None

    Cutting chunk 74/103 back @ 4:15-4:16 ("E")
    Parse-tree:
      None

    Appending chunk 75/103 @ 4:16-4:18 ("\tD")
    Parse-tree:
      None

    Cutting chunk 75/103 back @ 4:16-4:17 ("\t")
    Parse-tree:
      None

    Appending chunk 76/103 @ 4:17-4:19 ("DI")
    Parse-tree:
      None

    Cutting chunk 76/103 back @ 4:17-4:18 ("D")
    Parse-tree:
      None

    Appending chunk 77/103 @ 4:18-4:20 ("IV")
    Parse-tree:
      None

    Cutting chunk 77/103 back @ 4:18-4:19 ("I")
    Parse-tree:
      None

    Appending chunk 78/103 @ 4:19-4:21 ("VI")
    Parse-tree:
      None

    Cutting chunk 78/103 back @ 4:19-4:20 ("V")
    Parse-tree:
      None

    Appending chunk 79/103 @ 4:20-4:22 ("IS")
    Parse-tree:
      None

    Cutting chunk 79/103 back @ 4:20-4:21 ("I")
    Parse-tree:
      None

    Appending chunk 80/103 @ 4:21-4:23 ("SI")
    Parse-tree:
      None

    Cutting chunk 80/103 back @ 4:21-4:22 ("S")
    Parse-tree:
      None

    Appending chunk 81/103 @ 4:22-4:24 ("IO")
    Parse-tree:
      None

    Cutting chunk 81/103 back @ 4:22-4:23 ("I")
    Parse-tree:
      None

    Appending chunk 82/103 @ 4:23-4:25 ("ON")
    Parse-tree:
      None

    Cutting chunk 82/103 back @ 4:23-4:24 ("O")
    Parse-tree:
      None

    Appending chunk 83/103 @ 4:24-4:26 ("N.")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 83/103 back @ 4:24-4:25 ("N")
    Parse-tree:
      None

    Appending chunk 84/103 @ 4:25-5:0 (".\n")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 84/103 back @ 4:25-4:26 (".")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 85/103 @ 4:26-5:1 ("\n ")
    Parse-tree:
      None

    Cutting chunk 85/103 back @ 4:26-5:0 ("\n")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 86/103 @ 5:0-5:2 ("  ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 86/103 back @ 5:0-5:1 (" ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 87/103 @ 5:1-5:3 ("  ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 87/103 back @ 5:1-5:2 (" ")
    Parse-tree:
      None

    Appending chunk 88/103 @ 5:2-5:4 ("  ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 88/103 back @ 5:2-5:3 (" ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 89/103 @ 5:3-5:5 ("  ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 89/103 back @ 5:3-5:4 (" ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 90/103 @ 5:4-5:6 ("  ")
    Parse-tree:
      None

    Cutting chunk 90/103 back @ 5:4-5:5 (" ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 91/103 @ 5:5-5:7 ("  ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 91/103 back @ 5:5-5:6 (" ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 92/103 @ 5:6-5:8 ("  ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 92/103 back @ 5:6-5:7 (" ")
    Parse-tree:
      None

    Appending chunk 93/103 @ 5:7-5:9 ("  ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 93/103 back @ 5:7-5:8 (" ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 94/103 @ 5:8-5:10 ("  ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Cutting chunk 94/103 back @ 5:8-5:9 (" ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 95/103 @ 5:9-5:11 (" S")
    Parse-tree:
      None

    Cutting chunk 95/103 back @ 5:9-5:10 (" ")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.


    Appending chunk 96/103 @ 5:10-5:12 ("ST")
    Parse-tree:
      None

    Cutting chunk 96/103 back @ 5:10-5:11 ("S")
    Parse-tree:
      None

    Appending chunk 97/103 @ 5:11-5:13 ("TO")
    Parse-tree:
      None

    Cutting chunk 97/103 back @ 5:11-5:12 ("T")
    Parse-tree:
      None

    Appending chunk 98/103 @ 5:12-5:14 ("OP")
    Parse-tree:
      None

    Cutting chunk 98/103 back @ 5:12-5:13 ("O")
    Parse-tree:
      None

    Appending chunk 99/103 @ 5:13-5:15 ("P ")
    Parse-tree:
      None

    Cutting chunk 99/103 back @ 5:13-5:14 ("P")
    Parse-tree:
      None

    Appending chunk 100/103 @ 5:14-5:16 (" R")
    Parse-tree:
      None

    Cutting chunk 100/103 back @ 5:14-5:15 (" ")
    Parse-tree:
      None

    Appending chunk 101/103 @ 5:15-5:17 ("RU")
    Parse-tree:
      None

    Cutting chunk 101/103 back @ 5:15-5:16 ("R")
    Parse-tree:
      None

    Appending chunk 102/103 @ 5:16-5:18 ("UN")
    Parse-tree:
      None

    Cutting chunk 102/103 back @ 5:16-5:17 ("U")
    Parse-tree:
      None

    Appending chunk 103/103 @ 5:17-5:19 ("N.")
    Parse-tree:
      PROGRAM-ID. PROG.

      PROCEDURE DIVISION.
        STOP RUN. |}];;
