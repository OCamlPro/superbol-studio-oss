(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open EzCompat                                                    (* StringMap *)
open Lsp_testing

let print_procedures (doc, positions) =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  let server, prog = add_cobol_doc server ~projdir "prog.cob" doc in
  let checked_doc =
    LSP.Document.checked @@ LSP.Server.find_document prog server in
  let filename = Lsp.Uri.to_path prog.uri in
  StringMap.iter begin fun position_name (position: Lsp.Types.Position.t) ->
    let { proc_name; _ }: LSP.Lookup.TYPES.procedure_at_position =
      LSP.Lookup.proc_at_pos ~filename position checked_doc.group in
    Pretty.out "%s (line %d, character %d): @[<h>%a@]@."
      position_name position.line position.character
      Fmt.(option ~none:(any "None") Cobol_ptree.pp_qualname) proc_name
  end positions.pos_map;
  end_with_postproc

let%expect_test "find-procedure" =
  let end_with_postproc = print_procedures @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        PROCEDURE DIVISION.
            _|anon|_PERFORM A.
        P0. _|p0|_PERFORM A.
        A SE_|a-header|_CTION.
            _|a-head|_PERFORM B.
        B.  _|b|_GOBACK.
        C SECTION.
        D.  _|d|_GOBACK.
        E SE_|e|_CTION.
  |cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    a-head (line 7, character 12): A
    a-header (line 6, character 12): A
    anon (line 4, character 12): None
    b (line 8, character 12): B IN A
    d (line 10, character 12): D IN C
    e (line 11, character 12): E
    p0 (line 5, character 12): P0 |}]
