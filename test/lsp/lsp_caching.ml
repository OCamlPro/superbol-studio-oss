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


(** End-to-end tests for the LSP's persistent cache.

    These matter more than they look: a cache that has silently stopped working
    behaves exactly like one that is working, only slower, so every other test
    in this suite would pass just the same.  What is asserted here is that a hit
    really happens --- and, just as importantly, that it stops happening as soon
    as anything the document was parsed against changes.

    The last two are the cases that the flat per-document checksum this cache
    replaced could not see: a copybook that changed, and a copybook that
    appeared earlier in the lookup path than the one that had been resolved. *)

open Ez_file.V1
open Ez_file.FileString.OP

module LSP = Cobol_lsp

let layout = Superbol_project.Layout.default

let cache_storage_location =
  Superbol_project.TYPES.Store_cache_in_relative_dir {
    relative_dirname = "_superbol" // "cache";
  }

let lsp_cache_for ~rootdir =
  match
    Superbol_project.Cache.for_ ~rootdir
      ~storage_location:cache_storage_location
  with
  | Some (_, cache) ->
      cache
  | None ->
      Superbol_cache.Cache.disabled

let server ?(bounds = Superbol_cache.Types.Never) () =
  Cobol_common.Platform.copy
    ~dst:LSP.Platform.record ~src:Superbol_platform.record;
  LSP.Server.init
    ~params:{ config = { lsp_project_layout = layout;
                         lsp_cache_for;
                         lsp_cache_verbose = true;
                         lsp_cache_pruning_policy = bounds;
                         enable_client_configs = false;
                         force_syntax_diagnostics = true };
              root_uri = None;
              workspace_folders = [];
              with_semantic_tokens = false;
              with_client_file_watcher = `no;
              with_client_config_watcher = false;
              position_encoding = `UTF16 }

let write ~projdir filename text =
  let path = projdir // filename in
  EzFile.(safe_mkdir @@ dirname path);
  EzFile.write_file path text;
  path

let open_doc server ~projdir filename =
  let path = projdir // filename in
  LSP.Server.did_open
    Lsp.Types.DidOpenTextDocumentParams.{
      textDocument = Lsp.Types.TextDocumentItem.{
          languageId = "cobol"; version = 0;
          text = EzFile.read_file path; uri = Lsp.Uri.of_path path;
        };
    } server

(** Whether the server said it restored the document from cache.  The log lines
    carry absolute paths, so they are searched rather than compared. *)
let restored_from_cache output =
  try
    ignore (Str.search_forward
              (Str.regexp_string "Successfully read document cache") output 0);
    true
  with Not_found -> false

let report output =
  print_endline (if restored_from_cache output then "restored from cache"
                 else "parsed afresh")

let program = {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "BOOK".
       PROCEDURE DIVISION.
           STOP RUN.
|cobol}

(** Sets up a project holding [prog.cob] and a copybook, opens the program, and
    writes the cache out.  [%expect.output] only works directly inside a test,
    so what surrounds each step stays in the tests themselves. *)
(** How a session comes to persist what it holds: either because it ended
    normally, or because the editor saved the file. *)
let persist_on_shutdown server ~projdir:_ =
  LSP.Server.save_project_caches server

let persist_on_did_save server ~projdir =
  ignore @@ LSP.Server.did_save
    (Lsp.Types.DidSaveTextDocumentParams.create ()
       ~textDocument:(Lsp.Types.TextDocumentIdentifier.create
                        ~uri:(Lsp.Uri.of_path (projdir // "prog.cob"))))
    server

let setup ?(toml = "") ?(persist = persist_on_shutdown) ?bounds
    ~copybook_dirs () =
  let projdir = Superbol_testutils.Tempdir.make_n_enter "superbol-caching" in
  ignore (write ~projdir layout.project_config_filename toml);
  List.iter (fun (dir, contents) ->
      ignore (write ~projdir (dir // "BOOK.cpy") contents)) copybook_dirs;
  ignore (write ~projdir "prog.cob" program);
  persist (open_doc (server ?bounds ()) ~projdir "prog.cob") ~projdir;
  projdir

(** Opens the program again with a fresh server, as a later session would. *)
let reopen ~projdir =
  ignore (open_doc (server ()) ~projdir "prog.cob")

(** Same, but writing the cache back out on the way, as a session that ends
    normally does. *)
let reopen_and_save ~projdir =
  LSP.Server.save_project_caches (open_doc (server ()) ~projdir "prog.cob")

let cleanup projdir =
  try EzFile.remove_dir ~all:true projdir with _ -> ()

(* --- *)

let%expect_test "an unchanged project is restored from cache" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  reopen ~projdir;
  report [%expect.output];
  cleanup projdir;
  [%expect {| restored from cache |}];;

let%expect_test "editing the program itself is a miss" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  ignore (write ~projdir "prog.cob" (program ^ "      *> edited\n"));
  reopen ~projdir;
  report [%expect.output];
  cleanup projdir;
  [%expect {| parsed afresh |}];;

let%expect_test "editing a copybook the program COPYs is a miss" =
  (* The document's own contents are untouched here.  Only the copybook it
     pulled in has changed, which the flat checksum this cache replaced could
     not see. *)
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  ignore (write ~projdir "BOOK.cpy" "       01 X PIC 9(4).\n");
  reopen ~projdir;
  report [%expect.output];
  cleanup projdir;
  [%expect {| parsed afresh |}];;

let%expect_test "a copybook appearing earlier in the lookup path is a miss" =
  (* Neither the program nor the copybook it read has changed; what changed is
     that a directory searched earlier now holds a candidate, so the COPY
     resolves elsewhere.  This is the negative dependency that directory
     witnesses exist for. *)
  let projdir =
    setup ~copybook_dirs:["second", "       01 X PIC X.\n"]
      ~toml:"[cobol]\ncopybooks = [{dir = \"first\"}, {dir = \"second\"}]\n" ()
  in
  ignore [%expect.output];
  ignore (write ~projdir ("first" // "BOOK.cpy") "       01 X PIC 9(4).\n");
  reopen ~projdir;
  report [%expect.output];
  cleanup projdir;
  [%expect {| parsed afresh |}];;

let%expect_test "a document restored from cache still depends on its copybooks" =
  (* A restored document is not parsed, so nothing observes the outside world on
     its behalf.  Were it then cached again on what that session observed --
     nothing -- the entry would come to depend on nothing at all, and would go
     on matching whatever its copybooks had since become. *)
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  reopen_and_save ~projdir;                       (* a whole session, cache to cache *)
  ignore [%expect.output];
  ignore (write ~projdir "BOOK.cpy" "       01 X PIC 9(4).\n");
  reopen ~projdir;
  report [%expect.output];
  cleanup projdir;
  [%expect {| parsed afresh |}];;

let%expect_test "saving the file persists the document on its own" =
  (* Nothing here ever shuts a session down, so a hit can only come from what
     the notification itself wrote. *)
  let projdir =
    setup ~persist:persist_on_did_save
      ~copybook_dirs:[".", "       01 X PIC X.\n"] ()
  in
  ignore [%expect.output];
  reopen ~projdir;
  report [%expect.output];
  [%expect {| restored from cache |}];
  (* And what it wrote depends on the copybook, like any other entry. *)
  ignore (write ~projdir "BOOK.cpy" "       01 X PIC 9(4).\n");
  reopen ~projdir;
  report [%expect.output];
  cleanup projdir;
  [%expect {| parsed afresh |}];;

let%expect_test "a size bound is honoured when the cache is written out" =
  let copybook_dirs = [".", "       01 X PIC X.\n"] in
  (* Ample room: what was cached is still there to be found. *)
  let projdir =
    setup ~copybook_dirs
      ~bounds:(Superbol_cache.Types.Bounded { max_age = None;
                                              max_size = Some 10_000_000L }) ()
  in
  ignore [%expect.output];
  reopen ~projdir;
  report [%expect.output];
  cleanup projdir;
  [%expect {| restored from cache |}];
  (* No room at all: everything is reclaimed on the way out. *)
  let projdir =
    setup ~copybook_dirs
      ~bounds:(Superbol_cache.Types.Bounded { max_age = None;
                                              max_size = Some 0L }) ()
  in
  ignore [%expect.output];
  reopen ~projdir;
  report [%expect.output];
  cleanup projdir;
  [%expect {| parsed afresh |}];;

(* --- Sharing analyses with the CLI --- *)

(** Analyzes [prog.cob] the way the CLI does (for the LSP if [for_lsp] holds,
    as `superbol cache analyze' does), and tells whether the analysis was found
    in the cache. *)
let cli_analyze ?(for_lsp = false) ~projdir () =
  let filename = projdir // "prog.cob" in
  let rootdir = Superbol_project.rootdir_for ~filename ~layout in
  let project = Cobol_common.Diagnostics.forget_diags @@
    Superbol_project.for_ ~rootdir ~layout in
  let cache =
    lsp_cache_for ~rootdir:(Superbol_project.string_of_rootdir rootdir) in
  let contents = EzFile.read_file filename in
  let platform = Superbol_platform.record in
  let keys =
    Option.get @@ Superbol_project.Analysis.keys ~filename ~contents project
  in
  let hit =
    if for_lsp
    then snd @@ Superbol_project.Analysis.cached_analysis_for_lsp cache
        ~platform ~keys ~filename ~contents project
    else snd @@ Superbol_project.Analysis.cached_analysis cache
        ~platform ~keys ~filename ~contents project
  in
  print_endline (if hit then "found by the CLI" else "analyzed by the CLI")

let%expect_test "an analysis stored by the CLI is restored by the LSP" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  (* The LSP has stored what it opened; editing the program makes sure what it
     restores next can only come from the CLI. *)
  ignore (write ~projdir "prog.cob" (program ^ "      *> edited\n"));
  cli_analyze ~for_lsp:true ~projdir ();
  cli_analyze ~for_lsp:true ~projdir ();
  cli_analyze ~projdir ();
  [%expect {|
    analyzed by the CLI
    found by the CLI
    found by the CLI |}];
  reopen ~projdir;
  report [%expect.output];
  cleanup projdir;
  [%expect {| restored from cache |}];;

let%expect_test "an analysis not meant for the LSP is not restored by it" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  ignore (write ~projdir "prog.cob" (program ^ "      *> edited\n"));
  cli_analyze ~projdir ();
  cli_analyze ~projdir ();
  [%expect {|
    analyzed by the CLI
    found by the CLI |}];
  reopen ~projdir;
  report [%expect.output];
  cleanup projdir;
  [%expect {| parsed afresh |}];;

let%expect_test "an analysis stored by the LSP is found by the CLI" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  cli_analyze ~projdir ();
  cleanup projdir;
  [%expect {| found by the CLI |}];;

let%expect_test "editing a copybook is a miss for both" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  ignore (write ~projdir "BOOK.cpy" "       01 X PIC 9(4).\n");
  reopen ~projdir;
  report [%expect.output];
  [%expect {| parsed afresh |}];
  ignore (write ~projdir "BOOK.cpy" "       01 X PIC 9(5).\n");
  cli_analyze ~projdir ();
  cleanup projdir;
  [%expect {| analyzed by the CLI |}];;
