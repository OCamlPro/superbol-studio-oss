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

(** Semantic tokens of documents restored from the LSP's persistent cache.

    Tokens are not cached; semantic tokens are, instead.  A document restored
    from cache must therefore give the same semantic tokens as one that is
    parsed afresh, for the whole document as for any range. *)

open Ez_file.FileString.OP
open Lsp_caching                 (* layout, server, write, open_doc, report... *)

module LSP = Cobol_lsp

let semtoks_program = {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "BOOK".
       01 Y PIC X(10) VALUE "abc".
       LOCAL-STORAGE SECTION.
       01 L PIC 9.
      * A comment.
       PROCEDURE DIVISION.
       MAIN.
           MOVE "x" TO X Y
           ADD 1 TO L
           PERFORM MAIN
           STOP RUN.
|cobol}

let semtoks_ranges =
  let range (l, c) (l', c') =
    Lsp.Types.Range.create
      ~start:(Lsp.Types.Position.create ~line:l ~character:c)
      ~end_:(Lsp.Types.Position.create ~line:l' ~character:c')
  in
  [ range (0, 0) (100, 0);
    range (5, 0) (9, 0);
    range (12, 19) (12, 20);         (* touches both the literal and [TO] *)
    range (12, 11) (14, 18);
    range (13, 0) (13, 0) ]

(** Semantic tokens of [prog.cob] for the whole document, then for each of
    [semtoks_ranges]. *)
let semtoks server ~projdir =
  let textDocument =
    Lsp.Types.TextDocumentIdentifier.create
      ~uri:(Lsp.Uri.of_path (projdir // "prog.cob"))
  in
  let data = function
    | Some Lsp.Types.SemanticTokens.{ data; _ } -> data
    | None -> failwith "no semantic tokens"
  in
  data (LSP.Request.INTERNAL.semtoks_full server
          (Lsp.Types.SemanticTokensParams.create ~textDocument ())) ::
  List.map begin fun range ->
    data (LSP.Request.INTERNAL.semtoks_range server
            (Lsp.Types.SemanticTokensRangeParams.create ~textDocument
               ~range ()))
  end semtoks_ranges

let doc_of server ~projdir =
  LSP.Server.find_document
    (Lsp.Types.TextDocumentIdentifier.create
       ~uri:(Lsp.Uri.of_path (projdir // "prog.cob"))) server

(** Checks that range requests on the document opened in [server] yield what a
    direct computation for each range would, given the semantic tokens [all]
    obtained for it (for the whole document, and then for each range). *)
let check_ranges server ~projdir all =
  let doc = doc_of server ~projdir in
  let LSP.Types.{ artifacts = { pplog; tokens; rev_comments; rev_ignored };
                  _ } = doc in
  let Cobol_typeck.Outputs.{ ptree; _ } = LSP.Document.checked doc in
  (* Same filename as the server's, which may differ from the raw path on
     Windows (drive letter case, path separators). *)
  let filename = Lsp.Uri.to_path (LSP.Document.uri doc) in
  List.for_all2 begin fun range restricted ->
    LSP.Semtoks.data ~filename ~range:(Some range)
      ~tokens:(Lazy.force tokens) ~pplog ~ptree ~rev_ignored
      ~rev_comments:(EzCompat.StringMap.find "" rev_comments)
    = restricted
  end semtoks_ranges (List.tl all)

(** Opens [prog.cob] in successive sessions, where the client asks for
    semantic tokens or not according to [sessions].  The document is stored as
    soon as it is opened, so every session but the first may restore what the
    previous ones have stored; if [cli] holds, the CLI analyzes the document for
    the LSP before the first one.  Returns a function that tells whether the last
    session restored the document from cache, and whether its semantic tokens
    are available and the same as those of a fresh parse.  Whatever sessions
    print is meant to be dropped. *)
let make_project () =
  let projdir =
    Superbol_testutils.Tempdir.make_n_enter "superbol-caching-semtoks" in
  ignore (write ~projdir layout.project_config_filename "");
  ignore (write ~projdir "BOOK.cpy" "       01 X PIC X.\n");
  ignore (write ~projdir "prog.cob" semtoks_program);
  projdir

let run_sessions ?(cli = false) sessions =
  (* Semantic tokens of a fresh parse, for reference, in a project of its own
     so that what this session stores does not count. *)
  let fresh, ranges_ok =
    let projdir = make_project () in
    let reference = open_doc (server ()) ~projdir "prog.cob" in
    let fresh = semtoks reference ~projdir in
    let ranges_ok = check_ranges reference ~projdir fresh in
    cleanup projdir;
    fresh, ranges_ok
  in
  let projdir = make_project () in
  if cli then cli_analyze ~for_lsp:true ~projdir ();
  let last =
    List.fold_left begin fun _ with_semantic_tokens ->
      let server = server ~with_semantic_tokens () in
      let server = open_doc server ~projdir "prog.cob" in
      LSP.Server.save_project_caches server;
      Some server
    end None sessions |> Option.get
  in
  let doc = doc_of last ~projdir in
  let restored = Option.is_none doc.rewinder  (* a restored doc is not parsed *)
  and semtoks =
    match doc.semtoks with
    | None -> None
    | Some _ -> Some (semtoks last ~projdir)
  in
  cleanup projdir;
  fun () ->
    if not ranges_ok then print_endline "range mismatch";
    print_endline @@ String.concat " " @@
    List.map (fun a -> string_of_int (Array.length a / 5)) fresh;
    print_endline (if restored then "document restored from cache"
                   else "document parsed afresh");
    print_endline begin match semtoks with
      | None -> "no semantic tokens"
      | Some s when s = fresh -> "same semantic tokens"
      | Some _ -> "different semantic tokens"
    end

let%expect_test "semantic tokens are restored with the document" =
  let report = run_sessions [true; true] in
  ignore [%expect.output];
  report ();
  [%expect {|
    49 49 16 2 10 0
    document restored from cache
    same semantic tokens |}];;

let%expect_test "an analysis stored without semantic tokens is not restored \
                 for clients that ask for them..." =
  let report = run_sessions [false; true] in
  ignore [%expect.output];
  report ();
  [%expect {|
    49 49 16 2 10 0
    document parsed afresh
    same semantic tokens |}];;

let%expect_test "... until such a client stores them" =
  let report = run_sessions [false; true; true] in
  ignore [%expect.output];
  report ();
  [%expect {|
    49 49 16 2 10 0
    document restored from cache
    same semantic tokens |}];;

let%expect_test "an analysis stored with semantic tokens is restored for \
                 clients that do not ask for them" =
  let report = run_sessions [true; false] in
  ignore [%expect.output];
  report ();
  [%expect {|
    49 49 16 2 10 0
    document restored from cache
    same semantic tokens |}];;

let%expect_test "an analysis stored without semantic tokens is restored for \
                 clients that do not ask for them, without semantic tokens" =
  let report = run_sessions [false; false] in
  ignore [%expect.output];
  report ();
  [%expect {|
    49 49 16 2 10 0
    document restored from cache
    no semantic tokens |}];;

let%expect_test "semantic tokens stored by the CLI are restored" =
  let report = run_sessions ~cli:true [true] in
  ignore [%expect.output];
  report ();
  [%expect {|
    49 49 16 2 10 0
    document restored from cache
    same semantic tokens |}];;
