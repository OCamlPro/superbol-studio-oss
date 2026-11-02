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

(** Opens [prog.cob] in a first session, where the client asks for semantic
    tokens if [first_with] holds, and then in a later one, where it always
    does.  Tells whether the document was restored from cache along with its
    semantic tokens, and whether these are the same as those computed in the
    first session.  Whatever the first session prints is dropped. *)
let two_sessions ~first_with =
  let projdir =
    Superbol_testutils.Tempdir.make_n_enter "superbol-caching-semtoks" in
  ignore (write ~projdir layout.project_config_filename "");
  ignore (write ~projdir "BOOK.cpy" "       01 X PIC X.\n");
  ignore (write ~projdir "prog.cob" semtoks_program);
  (* The document is stored as soon as it is opened. *)
  let first =
    open_doc (server ~with_semantic_tokens:first_with ()) ~projdir "prog.cob"
  in
  let fresh = semtoks first ~projdir in
  (* What a range request yields is what a direct computation for that range
     would. *)
  let doc = doc_of first ~projdir in
  let LSP.Types.{ artifacts = { pplog; tokens; rev_comments; rev_ignored };
                  _ } = doc in
  let Cobol_typeck.Outputs.{ ptree; _ } = LSP.Document.checked doc in
  (* Same filename as the server's, which may differ from the raw path on
     Windows (drive letter case, path separators). *)
  let filename = Lsp.Uri.to_path (LSP.Document.uri doc) in
  let mismatches =
    List.filter Fun.id @@
    List.map2 begin fun range restricted ->
      LSP.Semtoks.data ~filename ~range:(Some range)
        ~tokens:(Lazy.force tokens) ~pplog ~ptree ~rev_ignored
        ~rev_comments:(EzCompat.StringMap.find "" rev_comments)
      <> restricted
    end semtoks_ranges (List.tl fresh)
  in
  let counts =
    String.concat " " @@
    List.map (fun a -> string_of_int (Array.length a / 5)) fresh
  in
  LSP.Server.save_project_caches first;
  let later =
    open_doc (server ~with_semantic_tokens:true ()) ~projdir "prog.cob"
  in
  projdir, later, fresh, counts, mismatches

let report_later ~projdir later ~fresh ~counts ~mismatches =
  if mismatches <> [] then print_endline "range mismatch";
  print_endline counts;
  (* A restored document was not parsed, and so has no rewinder. *)
  print_endline
    (if Option.is_none (doc_of later ~projdir).rewinder
     then "document restored from cache"
     else "document parsed afresh");
  print_endline
    (if Lazy.is_val (doc_of later ~projdir).semtoks
     then "semantic tokens restored from cache"
     else "semantic tokens not in cache");
  print_endline (if semtoks later ~projdir = fresh then "same semantic tokens"
                 else "different semantic tokens");
  cleanup projdir

let%expect_test "semantic tokens of a restored document are unchanged" =
  let projdir, later, fresh, counts, mismatches =
    two_sessions ~first_with:true in
  ignore [%expect.output];
  report_later ~projdir later ~fresh ~counts ~mismatches;
  [%expect {|
    49 49 16 2 10 0
    document restored from cache
    semantic tokens restored from cache
    same semantic tokens |}];;

let%expect_test "semantic tokens are not cached unless asked for" =
  (* The first client does not ask for semantic tokens, so they are not computed
     when the document is stored; a later one that does ask for them gets the
     same as if they had been. *)
  let projdir, later, fresh, counts, mismatches =
    two_sessions ~first_with:false in
  ignore [%expect.output];
  report_later ~projdir later ~fresh ~counts ~mismatches;
  [%expect {|
    49 49 16 2 10 0
    document restored from cache
    semantic tokens not in cache
    same semantic tokens |}];;
