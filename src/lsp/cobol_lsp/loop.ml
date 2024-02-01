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

(** [config ~project_layout ~enable_caching ~fallback_storage_directory ()]
    creates an LSP configuration structure for identifying and managing
    projects.

    - [project_layout] describes the layout of projects to be managed by the
      LSP;

    - [enable_caching] (defaulting to [true]) permits the storage of
      pre-computed data, to allow faster LSP restarts and re-opening documents;

    - [fallback_storage_directory], when provided (and if [enable_caching]
      holds), is the name of a directory that is used to store a cache whenever
      [project_layout] does not provide a per-project storage directory ({i i.e,}
      [project_layout.relative_work_dirname = None]). *)
let config
    ~(project_layout: Project.layout)
    ?(enable_caching = true)
    ?(fallback_storage_directory: string option)
    () =
  let cache_storage: Project_cache.storage =
    match project_layout.relative_work_dirname, fallback_storage_directory with
    | _ when not enable_caching ->
        No_storage
    | None, None ->
        No_storage
    | None, Some dirname ->
        Store_in_shared_dir { dirname }
    | Some relative_work_dirname, _ ->
        let invalid_dir reason =
          Fmt.invalid_arg
            ("relative_work_dirname: "^^reason^^" (got `%s')")
            relative_work_dirname
        in
        if relative_work_dirname = ""
        then invalid_dir "invalid directory name";
        if EzFile.is_absolute relative_work_dirname
        then invalid_dir "relative directory name expected";
        Store_in_file {
          relative_filename = relative_work_dirname // "lsp-cache";
        }
  in
  Server.{
    cache_config = {
      cache_storage;
      cache_verbose = true;
    };
    project_layout;
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
        continue @@ reply @@ Request.handle r state
    | Jsonrpc.Packet.Batch_call calls ->
        batch calls state
    | Jsonrpc.Packet.Response _ | Batch_response _ ->
        Pretty.error "Response@ recieved@ unexpectedly@.";
        continue state
    | exception End_of_file ->
        Request.shutdown state;
        Error "Premature end of input stream"                    (* exit loop *)
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
        batch calls' @@ reply @@ Request.handle n state
  and reply (state, response) =
    Lsp_io.send_response response;
    state
  and continue = function
    | Server.Exit code -> code                                (* exit loop *)
    | state -> loop state
  in
  loop (NotInitialized config)
