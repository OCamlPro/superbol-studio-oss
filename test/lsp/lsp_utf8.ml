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
