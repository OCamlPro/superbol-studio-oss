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

(** Publication of diagnostics of documents that are not open, as held in the
    project's cache. *)

open Ez_file.FileString.OP
open Lsp_caching                         (* layout, server, write, open_doc... *)

module LSP = Cobol_lsp

let book = {cobol|
       01 BOOK-ITEM PIC X.
       01 BOOK-GROUP PIC X.
          05 BOOK-SUB PIC X.
|cobol}

let prog = {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "BOOK".
       PROCEDURE DIVISION.
           STOP RUN.
|cobol}

let other = {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. other.
       PROCEDURE DIVISION.
           MOVE TO.
           STOP RUN.
|cobol}

let make_project () =
  let projdir =
    Superbol_testutils.Tempdir.make_n_enter "superbol-cached-diagnostics" in
  ignore (write ~projdir layout.project_config_filename "");
  ignore (write ~projdir "BOOK.cpy" book);
  ignore (write ~projdir "prog.cob" prog);
  ignore (write ~projdir ("sub" // "other.cbl") other);
  projdir

(** Analyzes [filename] as `superbol cache analyze' does. *)
let cli_analyze ~projdir filename =
  let setup = Superbol_free_lib.Project_commands.lsp_like_setup () in
  let common = fst (Superbol_free_lib.Common_args.get ()) () in
  ignore @@
  Superbol_free_lib.Project_commands.analyze_file_for_lsp ~setup common
    (projdir // filename)

let print_diagnostics ~file = function
  | [] ->
      Printf.printf "%s: none\n" file
  | diags ->
      Printf.printf "%s: %s\n" file @@ String.concat "; " @@
      List.map begin fun Lsp.Types.Diagnostic.{ range; message; _ } ->
        Printf.sprintf "%d: %s" (range.start.line + 1) message
      end diags

(** Prints the diagnostics published in [output], one line per publication:
    the file name (relative to [projdir]), followed by the line and message of
    each diagnostic. *)
let print_publications ~projdir output =
  (* Paths are compared as [Lsp.Uri.to_path] spells them, with forward
     slashes. *)
  let spelling path =
    String.map (function '\\' -> '/' | c -> c) @@
    Lsp.Uri.(to_path @@ of_path path)
  in
  let prefix = spelling projdir ^ "/" in
  let relative path =
    let path = spelling path in
    if String.starts_with ~prefix path
    then String.sub path (String.length prefix)
        (String.length path - String.length prefix)
    else path
  in
  String.split_on_char '\n' output |>
  List.iter begin fun line ->
    match Jsonrpc.Packet.t_of_yojson @@ Yojson.Safe.from_string line with
    | Notification n ->
        begin match Lsp.Server_notification.of_jsonrpc n with
          | Ok PublishDiagnostics { uri; diagnostics; _ } ->
              print_diagnostics ~file:(relative @@ Lsp.Uri.to_path uri)
                diagnostics
          | Ok _ | Error _ ->
              ()
        end
    | _ | exception _ ->                          (* headers, requests, etc. *)
        ()
  end

(** A server for the project in [projdir], as a workspace folder. *)
let server_in ~projdir =
  server () ~enable_client_configs:true
    ~workspace_folders:[Lsp.Uri.of_path projdir]

(** [publish server ~projdir files] asks [server] to publish the cached
    diagnostics of [files], and gives back the files it missed. *)
let publish server ~projdir files =
  let uris = List.map (fun f -> Lsp.Uri.of_path (projdir // f)) files in
  LSP.Server.publish_cached_diagnostics uris server

let print_missed missed =
  List.iter begin fun uri ->
    Printf.printf "missed: %s\n" (Filename.basename (Lsp.Uri.to_path uri))
  end missed

let%expect_test "diagnostics of files analyzed by the CLI are published" =
  let projdir = make_project () in
  List.iter (cli_analyze ~projdir) ["prog.cob"; "sub" // "other.cbl";
                                    "BOOK.cpy"];
  ignore [%expect.output];
  let server = server_in ~projdir in
  ignore [%expect.output];
  let server, missed =
    publish server ~projdir ["prog.cob"; "sub" // "other.cbl"; "BOOK.cpy";
                             "missing.cbl"]
  in
  print_publications ~projdir [%expect.output];
  print_missed missed;
  [%expect {|
    prog.cob: none
    sub/other.cbl: 5: Missing <identifiers>; 5: Invalid syntax; 5: Invalid syntax; 5: Missing <literal>
    BOOK.cpy: 3: Unexpected PICTURE clause for group item 'BOOK-GROUP'
    missed: missing.cbl |}];
  (* The copybook shows what the program reports for it, once opened. *)
  ignore (open_doc server ~projdir "BOOK.cpy");
  print_publications ~projdir [%expect.output];
  [%expect {| BOOK.cpy: 3: Unexpected PICTURE clause for group item 'BOOK-GROUP' |}];
  cleanup projdir;;

let%expect_test "open documents are left out, and out-dated diagnostics are \
                 missed" =
  let projdir = make_project () in
  List.iter (cli_analyze ~projdir) ["prog.cob"; "sub" // "other.cbl"];
  ignore [%expect.output];
  let server = open_doc (server_in ~projdir) ~projdir "prog.cob" in
  ignore [%expect.output];
  ignore (write ~projdir ("sub" // "other.cbl") (other ^ "      * changed\n"));
  let _, missed = publish server ~projdir ["prog.cob"; "sub" // "other.cbl"] in
  print_publications ~projdir [%expect.output];
  print_missed missed;
  [%expect {| missed: other.cbl |}];
  cleanup projdir;;

let%expect_test "diagnostics stored by the LSP are published by later \
                 sessions" =
  let projdir = make_project () in
  ignore (open_doc (server ()) ~projdir ("sub" // "other.cbl"));
  ignore [%expect.output];
  let server = server_in ~projdir in
  ignore [%expect.output];
  let _, missed = publish server ~projdir ["sub" // "other.cbl"] in
  print_publications ~projdir [%expect.output];
  print_missed missed;
  [%expect {| sub/other.cbl: 5: Missing <identifiers>; 5: Invalid syntax; 5: Invalid syntax; 5: Missing <literal> |}];
  cleanup projdir;;

let%expect_test "diagnostics no longer reported for a copybook are withdrawn" =
  let projdir = make_project () in
  cli_analyze ~projdir "prog.cob";
  ignore [%expect.output];
  let server = server_in ~projdir in
  ignore [%expect.output];
  let server, _ = publish server ~projdir ["prog.cob"] in
  print_publications ~projdir [%expect.output];
  [%expect {|
    prog.cob: none
    BOOK.cpy: 3: Unexpected PICTURE clause for group item 'BOOK-GROUP' |}];
  (* The copybook is fixed, and the program analyzed again. *)
  ignore (write ~projdir "BOOK.cpy" "       01 BOOK-ITEM PIC X.\n");
  cli_analyze ~projdir "prog.cob";
  ignore [%expect.output];
  let _, missed = publish server ~projdir ["prog.cob"] in
  print_publications ~projdir [%expect.output];
  print_missed missed;
  [%expect {|
    prog.cob: none
    BOOK.cpy: none |}];
  cleanup projdir;;

let%expect_test "URIs that designate the same file are normalized alike" =
  let upper = Lsp.Uri.of_path "C:/Users/me/prog.cob"
  and lower = Lsp.Uri.of_path "c:/Users/me/prog.cob" in
  Printf.printf "as given: %b\n" (Lsp.Uri.equal upper lower);
  Printf.printf "normalized: %b\n"
    LSP.Diagnostics.(Lsp.Uri.equal (normalize_uri upper) (normalize_uri lower));
  let unix = Lsp.Uri.of_path "/home/me/prog.cob" in
  Printf.printf "unchanged: %b\n"
    (Lsp.Uri.equal unix (LSP.Diagnostics.normalize_uri unix));
  [%expect {|
    as given: false
    normalized: true
    unchanged: true |}];;
