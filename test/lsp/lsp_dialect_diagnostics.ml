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

(** Reporting of problems met while loading the dialect that client settings
    give to a project that has no configuration file. *)

open Lsp_testing
open Ez_file.V1
open EzFile.OP

let layout = Superbol_project.Layout.default

(** A project in a directory of its own under [dir], with no configuration
    file. *)
let project_in ~dir name =
  let projdir = dir // name in
  EzFile.make_dir ~p:true projdir;
  LSP.Project.in_existing_dir projdir ~layout

let set_dialect project dialect =
  ignore @@
  LSP.Project.update_project_config ["dialect", `String dialect] project

(** Prints the files [output] publishes diagnostics on (relative to [dir]),
    whether there are any, and the messages it notifies. *)
let print_output ~dir output =
  (* Paths are compared as [Lsp.Uri.to_path] spells them, with forward
     slashes. *)
  let spelling path =
    String.map (function '\\' -> '/' | c -> c) @@
    Lsp.Uri.(to_path @@ of_path path)
  in
  let prefix = spelling dir ^ "/" in
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
              Printf.printf "%s: %s\n" (relative @@ Lsp.Uri.to_path uri)
                (if diagnostics = [] then "none" else "some diagnostics")
          | Ok ShowMessage { message; _ } ->
              Printf.printf "notified: %s\n" message
          | Ok _ | Error _ ->
              ()
        end
    | _ | exception _ ->                          (* headers, requests, etc. *)
        ()
  end

(** Removes [dir] after leaving it: Windows refuses to remove the current
    directory. *)
let remove_temp_dir dir =
  Sys.chdir (Filename.dirname dir);       (* [Unix.chdir] is missing in JS *)
  EzFile.remove_dir ~all:true dir

let%expect_test "problems in a dialect given by client settings are published \
                 on its configuration file" =
  (* As a starting server does: *)
  Cobol_common.Platform.copy ~dst:LSP.Platform.record
    ~src:Superbol_platform.record;
  let dir =
    Superbol_testutils.Tempdir.make_n_enter "superbol-dialect-diagnostics" in
  let conf = dir // "my.conf" in
  (* Lacks every option, and gives one a value of the wrong type. *)
  EzFile.write_text_file conf "name: \"default\"\nref-mod-zero-length: ok\n";
  let a = project_in ~dir "a" and b = project_in ~dir "b" in
  ignore [%expect.output];
  set_dialect a conf;
  set_dialect b conf;
  print_output ~dir [%expect.output];
  [%expect {|
    my.conf: some diagnostics
    my.conf: some diagnostics |}];
  (* Still used by [b]: kept. *)
  set_dialect a "default";
  print_output ~dir [%expect.output];
  [%expect {| |}];
  (* Used by no project anymore: withdrawn. *)
  set_dialect b "default";
  print_output ~dir [%expect.output];
  [%expect {| my.conf: none |}];
  remove_temp_dir dir;;

(* `superbol cache analyze' applies client settings as the LSP server does, but
   runs no server: it must load dialects through its own platform, as that of
   the server is never set up. *)
let%expect_test "the CLI loads dialects given by client settings" =
  (* As in a process that runs no server: *)
  let saved = LSP.Platform.{ record with verbosity = record.verbosity } in
  Cobol_common.Platform.copy ~dst:LSP.Platform.record
    ~src:Cobol_common.Platform.innocuous;
  Fun.protect ~finally:(fun () ->
      Cobol_common.Platform.copy ~dst:LSP.Platform.record ~src:saved)
  @@ fun () ->
  let dir =
    Superbol_testutils.Tempdir.make_n_enter "superbol-cli-dialect" in
  let conf = dir // "my.conf" in
  EzFile.write_text_file conf "name: \"IBM COBOL\"\n";
  let filename = dir // "prog.cob" in
  EzFile.write_text_file filename
    "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. prog.\n";
  let setup =
    { (Superbol_free_lib.Caching_commands.lsp_like_setup ()
         ~client_settings:["dialect", `String conf])
      with project_root = Some dir }
  in
  let common = fst (Superbol_free_lib.Common_args.get ()) () in
  let { project; _ }: _ Superbol_free_lib.Caching_commands.analyzed_file =
    Superbol_free_lib.Caching_commands.analyze_file_for_lsp ~setup common
      filename
  in
  ignore [%expect.output];
  Printf.printf "dialect: %s\n" @@
  Cobol_config.DIALECT.to_string @@ Cobol_config.dialect @@
  (Superbol_project.config project).cobol_config;
  remove_temp_dir dir;
  [%expect {| dialect: ibm-strict |}];;
