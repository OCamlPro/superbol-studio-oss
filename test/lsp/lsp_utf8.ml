(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2028 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Lsp_testing

let%expect_test "fixed-cobol-doc-with-missing-dot-after-utf8-chars" =
  (* Tests proper source positioning *)
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  ignore @@ add_cobol_doc server ~projdir "prog.cob" {cobol|
     1 IDENTIFICATION DIVISION.
     2 PROGRAM-ID. prog.
     3 DATA DIVISION.
     4 WORKING-STORAGE SECTION.
     5 01 W.
     6   02 V PICTURE X(20) VALUE "αβ d εφ"
     7*               character number 43 -^
     8 PROCEDURE DIVISION.
     9    DISPLAY V
    10    STOP RUN.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[{"message":"Missing .","range":{"end":{"character":43,"line":6},"start":{"character":43,"line":6}},"severity":4}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
|}];;

(* --- Interaction between tab expansion and UTF-8 positions ---------------

   Two distinct corrections apply between byte offsets and what is reported to
   the client: a tab is a single byte but spans several *columns*, and a
   multi-byte character is several bytes but a single *character*.  Reported
   positions count characters, so a tab must not affect them, and replacing a
   multi-byte character with an ASCII one must leave every reported position
   untouched -- whether the tab comes before it, after it, or not at all.

   Sources are built as ordinary string literals rather than {cobol|...|cobol}
   so that the tabs are explicit rather than invisible in this file. *)

let prog display_line =
  "\n\
  \     1 IDENTIFICATION DIVISION.\n\
  \     2 PROGRAM-ID. prog.\n\
  \     3 PROCEDURE DIVISION.\n\
  \     4 " ^ display_line ^ "\n\
  \     5    STOP RUN.\n"

(* Returns the project's post-processor; [%expect.output] may only be used
   directly inside a [let%expect_test], hence the split. *)
let check display_line =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  ignore @@ add_cobol_doc server ~projdir "prog.cob" (prog display_line);
  end_with_postproc

let%expect_test "utf8-before-tab" =
  let end_with_postproc = check "   DISPLAY \"\xc3\xa9\"\tMOVE TO TO." in
  end_with_postproc [%expect.output];
  [%expect {| {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":32,"line":4},"start":{"character":30,"line":4}},"severity":1},{"message":"Missing <identifiers>","range":{"end":{"character":29,"line":4},"start":{"character":29,"line":4}},"severity":4},{"message":"Invalid syntax","range":{"end":{"character":29,"line":4},"start":{"character":27,"line":4}},"severity":1},{"message":"Missing <literal>","range":{"end":{"character":26,"line":4},"start":{"character":26,"line":4}},"severity":4}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}]

let%expect_test "ascii-before-tab" =
  (* Same line with the multi-byte character replaced by an ASCII one: every
     reported position must be identical to "utf8-before-tab" above. *)
  let end_with_postproc = check "   DISPLAY \"e\"\tMOVE TO TO." in
  end_with_postproc [%expect.output];
  [%expect {| {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":32,"line":4},"start":{"character":30,"line":4}},"severity":1},{"message":"Missing <identifiers>","range":{"end":{"character":29,"line":4},"start":{"character":29,"line":4}},"severity":4},{"message":"Invalid syntax","range":{"end":{"character":29,"line":4},"start":{"character":27,"line":4}},"severity":1},{"message":"Missing <literal>","range":{"end":{"character":26,"line":4},"start":{"character":26,"line":4}},"severity":4}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}]

let%expect_test "utf8-after-tab" =
  let end_with_postproc = check "   DISPLAY\t\"\xc3\xa9\" MOVE TO TO." in
  end_with_postproc [%expect.output];
  [%expect {| {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":32,"line":4},"start":{"character":30,"line":4}},"severity":1},{"message":"Missing <identifiers>","range":{"end":{"character":29,"line":4},"start":{"character":29,"line":4}},"severity":4},{"message":"Invalid syntax","range":{"end":{"character":29,"line":4},"start":{"character":27,"line":4}},"severity":1},{"message":"Missing <literal>","range":{"end":{"character":26,"line":4},"start":{"character":26,"line":4}},"severity":4}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}]

let%expect_test "ascii-after-tab" =
  (* Must be identical to "utf8-after-tab" above. *)
  let end_with_postproc = check "   DISPLAY\t\"e\" MOVE TO TO." in
  end_with_postproc [%expect.output];
  [%expect {| {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":32,"line":4},"start":{"character":30,"line":4}},"severity":1},{"message":"Missing <identifiers>","range":{"end":{"character":29,"line":4},"start":{"character":29,"line":4}},"severity":4},{"message":"Invalid syntax","range":{"end":{"character":29,"line":4},"start":{"character":27,"line":4}},"severity":1},{"message":"Missing <literal>","range":{"end":{"character":26,"line":4},"start":{"character":26,"line":4}},"severity":4}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}]

let%expect_test "utf8-no-tab" =
  (* Control: the no-tab path was already correct; it must stay so. *)
  let end_with_postproc = check "   DISPLAY \"\xc3\xa9\" MOVE TO TO." in
  end_with_postproc [%expect.output];
  [%expect {| {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":32,"line":4},"start":{"character":30,"line":4}},"severity":1},{"message":"Missing <identifiers>","range":{"end":{"character":29,"line":4},"start":{"character":29,"line":4}},"severity":4},{"message":"Invalid syntax","range":{"end":{"character":29,"line":4},"start":{"character":27,"line":4}},"severity":1},{"message":"Missing <literal>","range":{"end":{"character":26,"line":4},"start":{"character":26,"line":4}},"severity":4}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}]

let%expect_test "ascii-no-tab" =
  (* Must be identical to "utf8-no-tab" above. *)
  let end_with_postproc = check "   DISPLAY \"e\" MOVE TO TO." in
  end_with_postproc [%expect.output];
  [%expect {| {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":32,"line":4},"start":{"character":30,"line":4}},"severity":1},{"message":"Missing <identifiers>","range":{"end":{"character":29,"line":4},"start":{"character":29,"line":4}},"severity":4},{"message":"Invalid syntax","range":{"end":{"character":29,"line":4},"start":{"character":27,"line":4}},"severity":1},{"message":"Missing <literal>","range":{"end":{"character":26,"line":4},"start":{"character":26,"line":4}},"severity":4}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}]
