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

(** LSP server's main I/O-driven loop *)

open Ez_file.V1
open Ez_file.V1.EzFile.OP

(** [config ~project_config_filename ~relative_work_dirname] creates an LSP
    configuration structure for identifying and managing projects.

    - [project_config_filename] is the names of the TOML file that is to be
    found at the root of each project's directory tree;

    - [relative_work_dirname] is the name of the directory where the LSP should
    put its working files (caches, etc). *)
let config ~project_config_filename ~relative_work_dirname =
  let invalid_dir reason =
    Fmt.invalid_arg
      ("relative_work_dirname: "^^reason^^" (got `%s')") relative_work_dirname
  in
  if relative_work_dirname = ""
  then invalid_dir "invalid direcory name";
  if EzFile.is_absolute relative_work_dirname
  then invalid_dir "relative direcory name expected";
  Lsp_server.{
    cache_config = {
      cache_relative_filename = relative_work_dirname // "lsp-cache";
      cache_verbose = true;
    };
    project_layout = {
      project_config_filename;
    };
  }

(** Start the lsp server, listening and responding on stdin and stdout.  This is
    blocking (driven by standard input), and shutdown is triggered by a client
    request.  Returns [Ok ()] if the server ran and shut down properly, or
    [Error error_message] otherwise. *)
let run ~config =
  Lsp_io.initialize_channels ();
  let rec loop state =
    match Lsp_io.read_message () with
    | Jsonrpc.Packet.Notification n ->
        continue @@ Lsp_notif.handle n state
    | Jsonrpc.Packet.Request r ->
        continue @@ reply @@ Lsp_request.handle r state
    | Jsonrpc.Packet.Batch_call calls ->
        batch calls state
    | Jsonrpc.Packet.Response _ | Batch_response _ ->
        Pretty.error "Response@ recieved@ unexpectedly@.";
        continue state
    | exception Lsp_io.Parse_error msg ->
        Lsp_io.pretty_notification ~type_:Error "%s" msg;
        Error msg                                                (* exit loop *)
  and batch calls state =
    match calls with
    | [] ->
        continue state
    | `Notification n :: calls' ->
        batch calls' @@ Lsp_notif.handle n state
    | `Request n :: calls' ->
        batch calls' @@ reply @@ Lsp_request.handle n state
  and reply (state, response) =
    Lsp_io.send_response response;
    state
  and continue = function
    | Lsp_server.Exit code -> code                                (* exit loop *)
    | state -> loop state
  in
  loop (NotInitialized config)
