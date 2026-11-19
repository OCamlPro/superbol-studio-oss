(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Tests for comment toggling ([superbol/editionsToToggleComments]). *)

open Lsp_testing

let toml ?(mf_comment = false) source_format = Printf.sprintf {|
[cobol]
dialect = "default"
source-format = "%s"
mf-comment = %b
|} source_format mf_comment

(** Successive pairs of anonymous markers delimit selections; a trailing
    lone marker is a cursor. *)
let ranges_of_markers { pos_anonymous; _ } =
  let rec aux acc = function
    | [] -> List.rev acc
    | [p] -> List.rev (Lsp.Types.Range.create ~start:p ~end_:p :: acc)
    | s :: e :: tl -> aux (Lsp.Types.Range.create ~start:s ~end_:e :: acc) tl
  in
  aux [] pos_anonymous

(** Prints [text] with a [|] at the end of each line. *)
let print_text ppf text =
  String.split_on_char '\n' text |>
  List.iter (fun l -> Pretty.print ppf "%s|@\n" l)

(** Toggles comments on the marked selections [times] times, and returns the
    text after each toggle. *)
let toggle ?(filename = "prog.cob") ?(times = 2) ~toml (text, positions) =
  let { projdir; _ }, server = make_lsp_project ~toml () in
  let server, prog = add_cobol_doc server ~projdir filename text in
  let ranges = ranges_of_markers positions in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let rec loop server i =
    if i < times then begin
      let doc = LSP.Server.find_document prog server in
      let edits =
        Option.get @@
        LSP.Request.INTERNAL.toggle_comments server ~uri:prog.uri ranges in
      let textdoc =
        Lsp.Text_document.apply_text_document_edits doc.textdoc edits in
      Pretty.print ppf "--- after toggle #%d (%d edits):@\n" (succ i)
        (List.length edits);
      print_text ppf (Lsp.Text_document.text textdoc);
      let server =
        LSP.Server.did_change
          Lsp.Types.DidChangeTextDocumentParams.{
            textDocument =
              Lsp.Types.VersionedTextDocumentIdentifier.create
                ~uri:prog.uri ~version:(succ i);
            contentChanges = [
              Lsp.Types.TextDocumentContentChangeEvent.create ()
                ~text:(Lsp.Text_document.text textdoc)
            ];
          } server
      in
      loop server (succ i)
    end
  in
  loop server 0;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let%expect_test "fixed-format" =
  let result = toggle ~toml:(toml "fixed") @@ extract_position_markers {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
000100_|_     DISPLAY "A"
000200     DISPLAY "B"
000300_|_     STOP RUN.
|cobol} in
  ignore [%expect.output];
  print_string result;
  [%expect {|
    --- after toggle #1 (3 edits):
    |
           IDENTIFICATION DIVISION.|
           PROGRAM-ID. prog.|
           PROCEDURE DIVISION.|
    000100*    DISPLAY "A"|
    000200*    DISPLAY "B"|
    000300*    STOP RUN.|
    |
    --- after toggle #2 (3 edits):
    |
           IDENTIFICATION DIVISION.|
           PROGRAM-ID. prog.|
           PROCEDURE DIVISION.|
    000100     DISPLAY "A"|
    000200     DISPLAY "B"|
    000300     STOP RUN.|
    | |}]

let%expect_test "fixed-format-mixed" =
  let result = toggle ~toml:(toml "fixed") @@ extract_position_markers {cobol|
       PROCEDURE DIVISION.
      _|_* Some comment
           DISPLAY "A"

      *    DISPLAY "B"
           *> floating comment
           STOP RUN._|_
|cobol} in
  ignore [%expect.output];
  print_string result;
  [%expect {|
    --- after toggle #1 (2 edits):
    |
           PROCEDURE DIVISION.|
          * Some comment|
          *    DISPLAY "A"|
    |
          *    DISPLAY "B"|
               *> floating comment|
          *    STOP RUN.|
    |
    --- after toggle #2 (5 edits):
    |
           PROCEDURE DIVISION.|
            Some comment|
               DISPLAY "A"|
    |
               DISPLAY "B"|
                  floating comment|
               STOP RUN.|
    | |}]

let%expect_test "fixed-format-special-indicators" =
  let result = toggle ~toml:(toml "fixed") @@ extract_position_markers {cobol|
       PROCEDURE DIVISION.
_|_           DISPLAY "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      -    "AAA"
      D    DISPLAY "DEBUG"
      >>SOURCE FIXED
       >>SOURCE FIXED
           STOP RUN.
_|_
|cobol} in
  ignore [%expect.output];
  print_string result;
  [%expect {|
    --- after toggle #1 (6 edits):
    |
           PROCEDURE DIVISION.|
          *    DISPLAY "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA|
          *-    "AAA"|
          *D    DISPLAY "DEBUG"|
          *>>SOURCE FIXED|
          *>>SOURCE FIXED|
          *    STOP RUN.|
    |
    |
    --- after toggle #2 (6 edits):
    |
           PROCEDURE DIVISION.|
               DISPLAY "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA|
          -    "AAA"|
          D    DISPLAY "DEBUG"|
           >>SOURCE FIXED|
           >>SOURCE FIXED|
               STOP RUN.|
    |
    | |}]

let%expect_test "fixed-format-tabs" =
  let result = toggle ~toml:(toml "fixed") @@ extract_position_markers
    "\n       PROCEDURE DIVISION.\n\
     _|_\tDISPLAY \"A\"\n\
     0001\tDISPLAY \"B\"\n\
     \t\tSTOP RUN._|_\n" in
  ignore [%expect.output];
  print_string result;
  [%expect {|
    --- after toggle #1 (3 edits):
    |
           PROCEDURE DIVISION.|
          * DISPLAY "A"|
    0001  * DISPLAY "B"|
          * 	STOP RUN.|
    |
    --- after toggle #2 (3 edits):
    |
           PROCEDURE DIVISION.|
            DISPLAY "A"|
    0001    DISPLAY "B"|
            	STOP RUN.|
    | |}]

let%expect_test "fixed-format-mf-comments" =
  let result = toggle ~toml:(toml ~mf_comment:true "fixed") ~times:1 @@
  extract_position_markers {cobol|
       PROCEDURE DIVISION.
_|_* MF-STYLE COMMENT LINE.
* ANOTHER ONE._|_
|cobol} in
  ignore [%expect.output];
  print_string result;
  [%expect {|
    --- after toggle #1 (2 edits):
    |
           PROCEDURE DIVISION.|
      MF-STYLE COMMENT LINE.|
      ANOTHER ONE.|
    | |}]

let%expect_test "free-format" =
  let result =
    toggle ~toml:(toml "free") ~times:3 @@ extract_position_markers {cobol|
PROCEDURE DIVISION.
  _|_  DISPLAY "A"
    *> already a comment

      DISPLAY "B"
  STOP RUN._|_
|cobol} in
  ignore [%expect.output];
  print_string result;
  [%expect {|
    --- after toggle #1 (4 edits):
    |
    PROCEDURE DIVISION.|
      *>   DISPLAY "A"|
      *>   *> already a comment|
    |
      *>     DISPLAY "B"|
      *> STOP RUN.|
    |
    --- after toggle #2 (4 edits):
    |
    PROCEDURE DIVISION.|
        DISPLAY "A"|
        *> already a comment|
    |
          DISPLAY "B"|
      STOP RUN.|
    |
    --- after toggle #3 (4 edits):
    |
    PROCEDURE DIVISION.|
      *>   DISPLAY "A"|
      *>   *> already a comment|
    |
      *>     DISPLAY "B"|
      *> STOP RUN.|
    | |}]

let%expect_test "source-format-switch" =
  (* Directive lines are left out of the selections, as toggling them changes
     the format of later lines. *)
  let result = toggle ~toml:(toml "fixed") @@ extract_position_markers {cobol|
       PROCEDURE DIVISION.
_|_           DISPLAY "A"
_|_       >>SOURCE FREE
_|_ DISPLAY "B"
_|_>>SOURCE FIXED
_|_           STOP RUN._|_
|cobol} in
  ignore [%expect.output];
  print_string result;
  [%expect {|
    --- after toggle #1 (3 edits):
    |
           PROCEDURE DIVISION.|
          *    DISPLAY "A"|
           >>SOURCE FREE|
     *> DISPLAY "B"|
    >>SOURCE FIXED|
          *    STOP RUN.|
    |
    --- after toggle #2 (3 edits):
    |
           PROCEDURE DIVISION.|
               DISPLAY "A"|
           >>SOURCE FREE|
     DISPLAY "B"|
    >>SOURCE FIXED|
               STOP RUN.|
    | |}]

let%expect_test "selections" =
  (* A selection ending at the start of a line excludes it; a cursor toggles
     its line. *)
  let result =
    toggle ~toml:(toml "free") ~times:1 @@ extract_position_markers {cobol|
_|_DISPLAY "1"
DISPLAY "2"
_|_DISPLAY "3"
DISPLAY "4"_|_
DISPLAY "5"
DISPLAY_|_ "6"
DISPLAY "7"
DISPLAY "8"_|_
|cobol} in
  ignore [%expect.output];
  print_string result;
  [%expect {|
    --- after toggle #1 (6 edits):
    |
    *> DISPLAY "1"|
    *> DISPLAY "2"|
    DISPLAY "3"|
    *> DISPLAY "4"|
    *> DISPLAY "5"|
    *> DISPLAY "6"|
    DISPLAY "7"|
    *> DISPLAY "8"|
    | |}]

let%expect_test "copybook" =
  let result = toggle ~toml:(toml "fixed") ~filename:"copy.cpy" ~times:1 @@
  extract_position_markers {cobol|
_|_       01 A PIC X._|_
|cobol} in
  ignore [%expect.output];
  print_string result;
  [%expect {|
    --- after toggle #1 (1 edits):
    |
          *01 A PIC X.|
    | |}]

let%expect_test "xopen-format" =
  let result = toggle ~toml:(toml "xopen") @@ extract_position_markers {cobol|
PROCEDURE DIVISION.
_|_* comment line
  DISPLAY "A"_|_
|cobol} in
  ignore [%expect.output];
  print_string result;
  [%expect {|
    --- after toggle #1 (2 edits):
    |
    PROCEDURE DIVISION.|
    *> * comment line|
    *>   DISPLAY "A"|
    |
    --- after toggle #2 (2 edits):
    |
    PROCEDURE DIVISION.|
    * comment line|
      DISPLAY "A"|
    | |}]

let%expect_test "fixed-format-non-ascii-sequence-area" =
  let result = toggle ~toml:(toml "fixed") @@ extract_position_markers {cobol|
       PROCEDURE DIVISION.
_|_AÉ1234     DISPLAY "É"
€€€€€€     STOP RUN._|_
|cobol} in
  ignore [%expect.output];
  print_string result;
  [%expect{|
    --- after toggle #1 (2 edits):
    |
           PROCEDURE DIVISION.|
    AÉ1234*    DISPLAY "É"|
    €€€€€€*    STOP RUN.|
    |
    --- after toggle #2 (2 edits):
    |
           PROCEDURE DIVISION.|
    AÉ1234     DISPLAY "É"|
    €€€€€€     STOP RUN.|
    | |}]
