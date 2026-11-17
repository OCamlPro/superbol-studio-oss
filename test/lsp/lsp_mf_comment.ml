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

(** MF-style comment lines, enabled with the [mf-comment] project setting. *)

open Lsp_testing

let prog = {cobol|
* MF-STYLE COMMENT LINE.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
* ANOTHER MF-STYLE COMMENT LINE.
       PROCEDURE DIVISION.
          STOP RUN.
|cobol}

let toml ~mf_comment = Printf.sprintf {|
[cobol]
dialect = "default"
source-format = "fixed"
mf-comment = %b
|} mf_comment

let%expect_test "mf-comment-disabled" =
  let { projdir; end_with_postproc }, server =
    make_lsp_project ~toml:(toml ~mf_comment:false) () in
  ignore @@ add_cobol_doc server ~projdir "prog.cob" prog;
  end_with_postproc [%expect.output];
  [%expect {| {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":9,"line":4},"start":{"character":7,"line":4}},"severity":1},{"message":"Unexpected indicator: `H'","range":{"end":{"character":7,"line":4},"start":{"character":6,"line":4}},"severity":1},{"message":"Invalid syntax","range":{"end":{"character":10,"line":1},"start":{"character":7,"line":1}},"severity":1},{"message":"Unexpected indicator: `T'","range":{"end":{"character":7,"line":1},"start":{"character":6,"line":1}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;

let%expect_test "mf-comment-enabled" =
  let { projdir; end_with_postproc }, server =
    make_lsp_project ~toml:(toml ~mf_comment:true) () in
  ignore @@ add_cobol_doc server ~projdir "prog.cob" prog;
  end_with_postproc [%expect.output];
  [%expect {| {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;
