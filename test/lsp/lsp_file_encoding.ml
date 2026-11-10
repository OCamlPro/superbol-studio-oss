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

(** Documents that are not open, read from disk and decoded with the encoding
    the client uses ([files.encoding] in VS Code), so that what `superbol cache
    analyze --encoding' stores for them is found. *)

open Ez_file.FileString.OP
open Lsp_caching                         (* layout, server, write, open_doc... *)

module LSP = Cobol_lsp

let prog = {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
           STOP RUN.
|cobol}

(** A program in windows-1252, with a diagnostic. *)
let other = {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. other.
      * caf|cobol} ^ "\xe9" ^ {cobol| cr|cobol} ^ "\xe8" ^ {cobol|me
       PROCEDURE DIVISION.
       PRINT-CUSTOMERS.
           MOVE TO.
           STOP RUN.
|cobol}

let make_project () =
  let projdir =
    Superbol_testutils.Tempdir.make_n_enter "superbol-file-encoding" in
  ignore (write ~projdir layout.project_config_filename "");
  ignore (write ~projdir "prog.cob" prog);
  ignore (write ~projdir "other.cbl" other);
  projdir

(** Analyzes [other.cbl] as `superbol cache analyze --encoding windows1252'
    does. *)
let cli_analyze ~projdir =
  let setup =
    Superbol_free_lib.Project_commands.lsp_like_setup ()
      ~encoding:(Result.get_ok @@
                 Cobol_common.Text_encoding.of_name "windows1252")
  in
  let common = fst (Superbol_free_lib.Common_args.get ()) () in
  ignore @@
  Superbol_free_lib.Project_commands.analyze_file_for_lsp ~setup common
    (projdir // "other.cbl")

(** A server with client settings enabled, for the project in [projdir] as a
    workspace folder.  It answers the request for the encoding of files that the
    server sends upon loading the project with [settings] (values of
    [files.encoding] and [[cobol]]), unless [None].  [output] gives what the
    server has printed so far. *)
let server_in ~projdir ~output settings =
  let server =
    server () ~enable_client_configs:true
      ~workspace_folders:[Lsp.Uri.of_path projdir]
  in
  let requests = output () in
  match settings with
  | None ->
      server
  | Some settings ->
      let id =
        try
          ignore (Str.search_forward request_id_regexp requests 0);
          int_of_string (Str.matched_group 1 requests)
        with Not_found -> failwith "no request for the client's settings"
      in
      LSP.Server.on_client_response id (`List settings) server

(** Tells whether the workspace symbols and the cached diagnostics of
    [other.cbl] are found.  [output] gives (and drops) what the server has
    printed so far. *)
let query server ~projdir ~output =
  let symbols =
    LSP.Request.INTERNAL.workspace_symbol server
      (Lsp.Types.WorkspaceSymbolParams.create ~query:"print" ())
  in
  let _, missed =
    LSP.Server.publish_cached_diagnostics
      [Lsp.Uri.of_path (projdir // "other.cbl")] server
  in
  ignore (output ());
  Printf.printf "symbols: %s, diagnostics: %s\n"
    (match symbols with None | Some [] -> "missed" | Some _ -> "found")
    (if missed = [] then "found" else "missed")

(** Stores [other.cbl] with the CLI, starts a server whose client answers with
    [settings], and queries it. *)
let check ~output settings =
  let projdir = make_project () in
  cli_analyze ~projdir;
  ignore (output ());
  let server = server_in ~projdir ~output settings in
  query server ~projdir ~output;
  cleanup projdir

let%expect_test "files that are not open are read as they are until the \
                 client tells their encoding" =
  check ~output:(fun () -> [%expect.output]) None;
  [%expect {| symbols: missed, diagnostics: missed |}];;

let%expect_test "files that are not open are decoded with `files.encoding'" =
  check ~output:(fun () -> [%expect.output])
    (Some [`String "windows1252"; `Null]);
  [%expect {| symbols: found, diagnostics: found |}];;

let%expect_test "`files.encoding' for COBOL takes precedence" =
  check ~output:(fun () -> [%expect.output])
    (Some [`String "utf8"; `Assoc ["files.encoding", `String "windows1252"]]);
  [%expect {| symbols: found, diagnostics: found |}];
  check ~output:(fun () -> [%expect.output])
    (Some [`String "windows1252"; `Assoc ["files.encoding", `String "utf8"]]);
  [%expect {| symbols: missed, diagnostics: missed |}];;

let%expect_test "files are read as they are with an unsupported encoding" =
  check ~output:(fun () -> [%expect.output])
    (Some [`String "gbk"; `Null]);
  [%expect {| symbols: missed, diagnostics: missed |}];;

let%expect_test "projects with a configuration file only warn about ignored \
                 settings when COBOL settings change" =
  let projdir = make_project () in
  let server =
    server () ~enable_client_configs:true
      ~workspace_folders:[Lsp.Uri.of_path projdir]
  in
  ignore [%expect.output];
  let notify ~cobol ~encoding (server, results) =
    let server =
      LSP.Server.on_client_config_changes server
        ~changes:(`Assoc [ "superbol", `Assoc ["cobol", cobol];
                           "files", `Assoc ["encoding", `String encoding] ])
    in
    let result =
      try ignore (Str.search_forward (Str.regexp_string "overriden")
                    [%expect.output] 0); "warned"
      with Not_found -> "silent"
    in
    server, result :: results
  in
  let settings = `Assoc ["dialect", `String "ibm"] in
  let _, results =
    (server, []) |>
    notify ~cobol:settings ~encoding:"utf8" |>    (* sent upon registration *)
    notify ~cobol:settings ~encoding:"windows1252" |>
    notify ~cobol:(`Assoc []) ~encoding:"windows1252"
  in
  List.iter print_endline (List.rev results);
  cleanup projdir;
  [%expect {|
    silent
    silent
    warned |}];;
