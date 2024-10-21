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

(** Tests for copybook detection; very basic for now *)

let make_lsp_project_with_global_copybook_dir dir =
  make_lsp_project () ~toml:(Pretty.to_string {toml|
    cobol.copybooks = [{ dir = %S, file-relative = false }]
  |toml} dir)

let make_lsp_project_with_file_relative_copybook_dir dir =
  make_lsp_project () ~toml:(Pretty.to_string {toml|
    cobol.copybooks = [{ dir = %S, file-relative = true }]
  |toml} dir)

(* --- *)

let%expect_test "typical-copybook" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  ignore @@ add_cobol_doc server ~projdir "FIELD" {cobol|
       01 FIELD PIC X.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/FIELD appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/FIELD"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;


let%expect_test "typical-program" =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  ignore @@ add_cobol_doc server ~projdir "PROGRAM" {cobol|
       program-id. prog.
       procedure division.
          stop run.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/PROGRAM"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;

let%expect_test "typical-copybook-in-project-relative-copybook-dir" =
  let { projdir; end_with_postproc }, server =
    make_lsp_project_with_global_copybook_dir "global/copybooks"
  in
  ignore @@ add_cobol_doc server ~projdir "global/copybooks/FIELD" {cobol|
       01 FIELD PIC X.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/global/copybooks/FIELD appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/global/copybooks/FIELD"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;


let%expect_test "weird-copybook-in-project-relative-copybook-dir" =
  let { projdir; end_with_postproc }, server =
    make_lsp_project_with_global_copybook_dir "global/copybooks"
  in
  ignore @@ add_cobol_doc server ~projdir "global/copybooks/INDIRCPY" {cobol|
       COPY X.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/global/copybooks/INDIRCPY appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/global/copybooks/INDIRCPY"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;


let%expect_test "typical-copybook-in-file-relative-copybook-dir" =
  let { projdir; end_with_postproc }, server =
    make_lsp_project_with_file_relative_copybook_dir "lib/copybooks"
  in
  ignore @@ add_cobol_doc server ~projdir "lib/copybooks/FIELD" {cobol|
       01 FIELD PIC X.
  |cobol};
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"message":"file://__rootdir__/lib/copybooks/FIELD appears to be a copybook","type":4},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/lib/copybooks/FIELD"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} |}];;
