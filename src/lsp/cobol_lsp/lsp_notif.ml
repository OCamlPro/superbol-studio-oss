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

open Lsp_server.TYPES

let on_notification state notif =
  match state, notif with
  | ShuttingDown, Lsp.Client_notification.Exit ->
      Exit (Ok ())
  | ShuttingDown, _ ->
      ShuttingDown
  | NotInitialized _ | Exit _ as state, _ ->
      state                   (* spec indicate notif should just be discarded *)
  | Initialized params, Initialized ->
      Running (Lsp_server.init ~params)
  | Running registry, ChangeWorkspaceFolders params ->
      Running (Lsp_server.on_change_workspace_folders params registry)
  | Running registry, ChangeConfiguration { settings = changes } ->
      Running (Lsp_server.on_client_config_changes ~changes registry)
  | Running registry, DidChangeWatchedFiles { changes } ->
      Running (Lsp_server.on_watched_file_changes changes registry)
  | Running registry, TextDocumentDidOpen params ->
      Running (Lsp_server.did_open params registry)
  | Running registry, TextDocumentDidChange params ->
      Running (Lsp_server.did_change params registry)
  | Running registry, TextDocumentDidClose params ->
      Running (Lsp_server.did_close params registry)
  | Running _, Exit ->
      Lsp_request.shutdown state;
      Exit (Error "Received premature 'exit' notification")
  | _ ->
      state


let handle json_notif state =
  match Lsp.Client_notification.of_jsonrpc json_notif with
  | Error str ->
      Lsp_io.log_error "Invalid@ notification:@ %s" str;
      state
  | Ok notif ->
      try on_notification state notif with
      | Lsp_server.Document_not_found { uri } ->
          Lsp_io.log_error "Document@ %s@ is@ not@ opened@ yet"
            (Lsp.Types.DocumentUri.to_string uri);
          state
      | e ->
          Lsp_io.log_error "While@ handling@ notification@ %s:@ %a"
            (Yojson.Safe.to_string @@
             Jsonrpc.Notification.yojson_of_t json_notif)
            Fmt.exn e;
          state
