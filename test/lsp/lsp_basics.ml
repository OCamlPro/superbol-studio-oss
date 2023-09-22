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

open Lsp_testing

let%expect_test "add-empty-cobol-doc" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  ignore @@ add_cobol_doc server ~projdir "prog.cob" "";
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/superbol.toml"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[{"message":"Source format `auto` is not supported yet, using `fixed`","range":{"end":{"character":0,"line":0},"start":{"character":0,"line":0}},"severity":2}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
|}];;

let%expect_test "add-simple-cobol-doc" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  ignore @@ add_cobol_doc server ~projdir "prog.cob" {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
          STOP RUN.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/superbol.toml"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[{"message":"Source format `auto` is not supported yet, using `fixed`","range":{"end":{"character":0,"line":0},"start":{"character":0,"line":0}},"severity":2}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
|}];;
