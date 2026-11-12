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

let server ?(bounds = Superbol_cache.Types.Never)
    ?(with_semantic_tokens = false) ?(enable_client_configs = false)
    ?(workspace_folders = []) () =
  Cobol_common.Platform.copy
    ~dst:LSP.Platform.record ~src:Superbol_platform.record;
  LSP.Server.init
    ~params:{ config = { lsp_project_layout = layout;
                         lsp_cache_for;
                         lsp_cache_verbose = true;
                         lsp_cache_pruning_policy = bounds;
                         enable_client_configs;
                         force_syntax_diagnostics = true };
              root_uri = None;
              workspace_folders;
              with_semantic_tokens;
              with_client_file_watcher = `no;
              with_client_config_watcher = false;
              position_encoding = `UTF16 }

let write ~projdir filename text =
  let path = projdir // filename in
  EzFile.(safe_mkdir @@ dirname path);
  EzFile.write_file path text;
  path

(** Opens [filename], whose contents the client sends after applying [decode]
    (as VS Code does according to its [files.encoding] setting). *)
let open_doc ?(decode = Fun.id) server ~projdir filename =
  let path = projdir // filename in
  LSP.Server.did_open
    Lsp.Types.DidOpenTextDocumentParams.{
      textDocument = Lsp.Types.TextDocumentItem.{
          languageId = "cobol"; version = 0;
          text = decode (EzFile.read_file path); uri = Lsp.Uri.of_path path;
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
let reopen ?decode ~projdir () =
  ignore (open_doc ?decode (server ()) ~projdir "prog.cob")

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
  reopen ~projdir ();
  report [%expect.output];
  cleanup projdir;
  [%expect {| restored from cache |}];;

let%expect_test "editing the program itself is a miss" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  ignore (write ~projdir "prog.cob" (program ^ "      *> edited\n"));
  reopen ~projdir ();
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
  reopen ~projdir ();
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
  reopen ~projdir ();
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
  reopen ~projdir ();
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
  reopen ~projdir ();
  report [%expect.output];
  [%expect {| restored from cache |}];
  (* And what it wrote depends on the copybook, like any other entry. *)
  ignore (write ~projdir "BOOK.cpy" "       01 X PIC 9(4).\n");
  reopen ~projdir ();
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
  reopen ~projdir ();
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
  reopen ~projdir ();
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
  let platform = Superbol_platform.record in
  let project = Cobol_common.Diagnostics.forget_diags @@
    Superbol_project.for_ ~platform ~rootdir ~layout in
  let cache =
    lsp_cache_for ~rootdir:(Superbol_project.string_of_rootdir rootdir) in
  let contents = EzFile.read_file filename in
  let keys =
    Option.get @@ Superbol_project.Analysis.keys ~filename ~contents project
  in
  let hit =
    if for_lsp
    then snd @@ Superbol_project.Semantic_tokens.cached_analysis_for_lsp cache
        ~platform ~keys ~filename ~contents project
        ~semantic_tokens:Cobol_lsp.Semtoks.whole
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
  reopen ~projdir ();
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
  reopen ~projdir ();
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
  reopen ~projdir ();
  report [%expect.output];
  [%expect {| parsed afresh |}];
  ignore (write ~projdir "BOOK.cpy" "       01 X PIC 9(5).\n");
  cli_analyze ~projdir ();
  cleanup projdir;
  [%expect {| analyzed by the CLI |}];;

(** Analyzes [prog.cob] as `superbol cache analyze' does, adding
    [--no-lsp-artifacts] unless [lsp_artifacts] holds. *)
let cli_cache_analyze ?(lsp_artifacts = true) ~projdir () =
  let setup = Superbol_free_lib.Project_commands.lsp_like_setup () in
  let common = fst (Superbol_free_lib.Common_args.get ()) () in
  let filename = projdir // "prog.cob" in
  let hit =
    if lsp_artifacts
    then (Superbol_free_lib.Project_commands.analyze_file_for_lsp ~setup
            common filename).hit
    else (Superbol_free_lib.Project_commands.analyze_file ~setup
            common filename).hit
  in
  print_endline (if hit then "found by the CLI" else "analyzed by the CLI")

let%expect_test "`cache analyze --no-lsp-artifacts' stores an analysis the LSP \
                 does not restore" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  ignore (write ~projdir "prog.cob" (program ^ "      *> edited\n"));
  cli_cache_analyze ~lsp_artifacts:false ~projdir ();
  cli_cache_analyze ~lsp_artifacts:false ~projdir ();
  [%expect {|
    analyzed by the CLI
    found by the CLI |}];
  reopen ~projdir ();
  report [%expect.output];
  cleanup projdir;
  [%expect {| parsed afresh |}];;

let%expect_test "`cache analyze --no-lsp-artifacts' finds analyses stored for \
                 the LSP" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  (* Stored when the LSP opened it, then by the CLI after the edit. *)
  cli_cache_analyze ~lsp_artifacts:false ~projdir ();
  ignore (write ~projdir "prog.cob" (program ^ "      *> edited\n"));
  cli_cache_analyze ~projdir ();
  cli_cache_analyze ~lsp_artifacts:false ~projdir ();
  cleanup projdir;
  [%expect {|
    found by the CLI
    analyzed by the CLI
    found by the CLI |}];;

(* --- Sharing analyses with the CLI, for projects configured by the client --- *)

(** Settings that differ from the defaults, and take part in cache keys. *)
let client_settings : (string * Yojson.Safe.t) list =
  [ "sourceFormat", `String "fixed";
    "copyexts", `List [`String "cpy"] ]

(** Sets up a project that has no configuration file, holding [prog.cob] and a
    copybook. *)
let setup_unconfigured () =
  let projdir = Superbol_testutils.Tempdir.make_n_enter "superbol-client" in
  ignore (write ~projdir "BOOK.cpy" "       01 X PIC X.\n");
  ignore (write ~projdir "prog.cob" program);
  projdir

(** Analyzes [prog.cob] as `superbol cache analyze --client-settings' does. *)
let cli_analyze_with_settings ~projdir settings =
  let setup =
    Superbol_free_lib.Project_commands.lsp_like_setup ()
      ~client_settings:settings
  in
  let common = fst (Superbol_free_lib.Common_args.get ()) () in
  let analyzed =
    Superbol_free_lib.Project_commands.analyze_file_for_lsp ~setup common
      (projdir // "prog.cob")
  in
  print_endline (if analyzed.hit then "found by the CLI"
                 else "analyzed by the CLI")

let request_id_regexp = Str.regexp {|"id":\([0-9]+\)|}

(** Opens [prog.cob] with a fresh server that has its client's settings enabled,
    and answers the server's request for them with [settings].  [output] gives
    what the server has printed so far. *)
let open_with_client_settings ~projdir ~output settings =
  let server = open_doc (server ~enable_client_configs:true ()) ~projdir
      "prog.cob" in
  let requests = output () in
  let id =
    try
      ignore (Str.search_forward request_id_regexp requests 0);
      int_of_string (Str.matched_group 1 requests)
    with Not_found -> failwith "no request for the client's settings"
  in
  ignore (LSP.Server.on_client_response id (`List [`Assoc settings]) server)

let%expect_test "an analysis stored by the CLI with client settings is \
                 restored by the LSP when its client sends the same" =
  let projdir = setup_unconfigured () in
  cli_analyze_with_settings ~projdir client_settings;
  cli_analyze_with_settings ~projdir client_settings;
  [%expect {|
    analyzed by the CLI
    found by the CLI |}];
  open_with_client_settings ~projdir client_settings
    ~output:(fun () -> [%expect.output]);
  report [%expect.output];
  cleanup projdir;
  [%expect {| restored from cache |}];;

let%expect_test "an analysis stored by the CLI with client settings is not \
                 restored by the LSP when its client sends others" =
  let projdir = setup_unconfigured () in
  cli_analyze_with_settings ~projdir client_settings;
  [%expect {| analyzed by the CLI |}];
  open_with_client_settings ~projdir []
    ~output:(fun () -> [%expect.output]);
  report [%expect.output];
  cleanup projdir;
  [%expect {| parsed afresh |}];;

(* --- Sharing analyses of files that are not plain UTF-8 with the CLI --- *)

module TE = Cobol_common.Text_encoding

(** Analyzes [prog.cob] as `superbol cache analyze' does, with [--encoding
    encoding] if given. *)
let cli_analyze_with_encoding ?encoding ~projdir () =
  let setup =
    Superbol_free_lib.Project_commands.lsp_like_setup ()
      ?encoding:(Option.map (fun e -> Result.get_ok (TE.of_name e)) encoding)
  in
  let common = fst (Superbol_free_lib.Common_args.get ()) () in
  let analyzed =
    Superbol_free_lib.Project_commands.analyze_file_for_lsp ~setup common
      (projdir // "prog.cob")
  in
  print_endline (if analyzed.hit then "found by the CLI"
                 else "analyzed by the CLI")

(** Decodes as VS Code does with [files.encoding] set to [encoding]. *)
let vscode_decode encoding = TE.decode (Result.get_ok (TE.of_name encoding))

(** [program], with a comment and a literal in windows-1252. *)
let windows1252_program =
  program ^ "      * caf\xe9 cr\xe8me \x80\n           DISPLAY \"\xe9t\xe9\".\n"

let%expect_test "a windows-1252 program stored by the CLI with that encoding \
                 is restored by the LSP" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  ignore (write ~projdir "prog.cob" windows1252_program);
  cli_analyze_with_encoding ~encoding:"windows1252" ~projdir ();
  cli_analyze_with_encoding ~encoding:"windows1252" ~projdir ();
  [%expect {|
    analyzed by the CLI
    found by the CLI |}];
  reopen ~decode:(vscode_decode "windows1252") ~projdir ();
  report [%expect.output];
  cleanup projdir;
  [%expect {| restored from cache |}];;

let%expect_test "a windows-1252 program stored by the CLI without encoding is \
                 not restored by the LSP" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  ignore (write ~projdir "prog.cob" windows1252_program);
  cli_analyze_with_encoding ~projdir ();
  [%expect {| analyzed by the CLI |}];
  reopen ~decode:(vscode_decode "windows1252") ~projdir ();
  report [%expect.output];
  cleanup projdir;
  [%expect {| parsed afresh |}];;

let%expect_test "a UTF-8 program with a byte order mark stored by the CLI is \
                 restored by the LSP" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  (* Without its byte order mark, this is the program the LSP has stored. *)
  ignore (write ~projdir "prog.cob" ("\xef\xbb\xbf" ^ program));
  cli_analyze_with_encoding ~encoding:"utf8" ~projdir ();
  [%expect {| found by the CLI |}];
  reopen ~decode:(vscode_decode "utf8") ~projdir ();
  report [%expect.output];
  cleanup projdir;
  [%expect {| restored from cache |}];;

let%expect_test "giving an encoding does not change keys of ASCII programs" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  ignore (write ~projdir "prog.cob" (program ^ "      *> edited\n"));
  cli_analyze_with_encoding ~projdir ();
  cli_analyze_with_encoding ~encoding:"windows1252" ~projdir ();
  cli_analyze_with_encoding ~encoding:"utf8" ~projdir ();
  cleanup projdir;
  [%expect {|
    analyzed by the CLI
    found by the CLI
    found by the CLI |}];;

(* --- Superseding earlier versions --- *)

let show_cache_stats ~projdir =
  let rootdir =
    Superbol_project.rootdir_for ~filename:(projdir // "prog.cob") ~layout in
  let cache =
    lsp_cache_for ~rootdir:(Superbol_project.string_of_rootdir rootdir) in
  match Superbol_cache.Cache.stats cache with
  | Ok { stats_entries; stats_objects; _ } ->
      Printf.printf "entries: %d, values: %d\n" stats_entries stats_objects
  | Error _ ->
      print_endline "stats: failed"

let%expect_test "storing a new version reclaims the previous one" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  show_cache_stats ~projdir;
  [%expect {| entries: 5, values: 5 |}];
  (* Edited between sessions: the new version is stored when opened... *)
  ignore (write ~projdir "prog.cob" (program ^ "      *> edited\n"));
  reopen_and_save ~projdir;
  ignore [%expect.output];
  show_cache_stats ~projdir;
  [%expect {| entries: 5, values: 5 |}];
  (* ... and the previous one is gone: reverting is a miss. *)
  ignore (write ~projdir "prog.cob" program);
  reopen ~projdir ();
  report [%expect.output];
  cleanup projdir;
  [%expect {| parsed afresh |}];;

let%expect_test "saving an edited document reclaims the previous version" =
  let projdir =
    setup ~persist:persist_on_did_save
      ~copybook_dirs:[".", "       01 X PIC X.\n"] ()
  in
  ignore [%expect.output];
  let server = open_doc (server ()) ~projdir "prog.cob" in
  ignore [%expect.output];
  let uri = Lsp.Uri.of_path (projdir // "prog.cob") in
  let server =
    LSP.Server.did_change
      Lsp.Types.DidChangeTextDocumentParams.{
        textDocument = Lsp.Types.VersionedTextDocumentIdentifier.create ~uri
            ~version:1;
        contentChanges = [
          Lsp.Types.TextDocumentContentChangeEvent.create
            ~text:(program ^ "      *> edited\n") () ];
      } server
  in
  ignore (write ~projdir "prog.cob" (program ^ "      *> edited\n"));
  persist_on_did_save server ~projdir;
  ignore [%expect.output];
  (* The analysis, LSP data, workspace symbols and diagnostics; no project
     yet. *)
  show_cache_stats ~projdir;
  [%expect {| entries: 4, values: 4 |}];
  reopen ~projdir ();
  report [%expect.output];
  [%expect {| restored from cache |}];
  ignore (write ~projdir "prog.cob" program);
  reopen ~projdir ();
  report [%expect.output];
  cleanup projdir;
  [%expect {| parsed afresh |}];;

let%expect_test "the CLI supersedes the analyses it stored for earlier versions" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  let version n = program ^ Printf.sprintf "      *> version %d\n" n in
  List.iter begin fun for_lsp ->
    ignore (write ~projdir "prog.cob" (version 2));
    cli_analyze ~for_lsp ~projdir ();
    ignore (write ~projdir "prog.cob" (version 3));
    cli_analyze ~for_lsp ~projdir ();
    ignore (write ~projdir "prog.cob" (version 2));
    cli_analyze ~for_lsp ~projdir ();
  end [false; true];
  cleanup projdir;
  [%expect {|
    analyzed by the CLI
    analyzed by the CLI
    analyzed by the CLI
    analyzed by the CLI
    analyzed by the CLI
    analyzed by the CLI |}];;

(* --- Monitoring and pruning from the CLI --- *)

let location ?(is_root = false) dir =
  Superbol_free_lib.Command_cache.{ dir; is_root; storage_directory = None }

(** Shows some of the [output] of `superbol cache status/trim --json'. *)
let show_cache_json ~projdir output =
  let json = Yojson.Safe.from_string output in
  let field f = Yojson.Safe.Util.member f json in
  let int f = match field f with
    | `Int i -> string_of_int i
    | `Intlit i -> i
    | `Null -> "-"
    | _ -> "?"
  in
  Printf.printf "root is project: %b, cache: %b, entries: %s, values: %s, \
                 non-empty: %b"
    (field "project_root" = `String projdir)
    (field "cache" <> `Null) (int "entries") (int "values")
    (int "disk_usage" <> "0");
  begin match field "reclaimed" with
    | `Int n -> Printf.printf ", reclaimed: %d" n
    | _ -> ()
  end;
  begin match field "items" with
    | `List items ->
        let labels =
          List.sort_uniq compare @@ List.map (fun i ->
              Yojson.Safe.Util.(to_string @@ member "label" i)) items
        in
        Printf.printf ", items: %d (%s)" (List.length items)
          (String.concat ", " labels)
    | _ -> ()
  end;
  print_newline ()

let%expect_test "`cache status --json' and `cache trim --json' report on the \
                 cache of the LSP" =
  let projdir = setup ~copybook_dirs:[".", "       01 X PIC X.\n"] () in
  ignore [%expect.output];
  ignore (EzFile.make_dir ~p:true (projdir // "sub"));
  let status ~verbose =
    Superbol_free_lib.Command_cache.status ~json:true ~verbose in
  (* [%expect.output] takes everything output so far: take each output before
     showing any. *)
  status ~verbose:false (location projdir);
  let at_root = [%expect.output] in
  (* [sub] holds no configuration file: it belongs to [projdir]... *)
  status ~verbose:true (location (projdir // "sub"));
  let within = [%expect.output] in
  (* ... unless designated as a root. *)
  status ~verbose:false (location ~is_root:true (projdir // "sub"));
  let as_root = [%expect.output] in
  Superbol_free_lib.Command_cache.trim ~json:true ~older_than:(Some 0.)
    ~max_size:None (location ~is_root:true projdir);
  let trimmed = [%expect.output] in
  List.iter (show_cache_json ~projdir) [at_root; within; as_root; trimmed];
  cleanup projdir;
  [%expect {|
    root is project: true, cache: true, entries: 5, values: 5, non-empty: true
    root is project: true, cache: true, entries: 5, values: 5, non-empty: true, items: 5 (prog.cob, project)
    root is project: false, cache: true, entries: 0, values: 0, non-empty: false
    root is project: true, cache: true, entries: 0, values: 0, non-empty: false, reclaimed: 5 |}];;
