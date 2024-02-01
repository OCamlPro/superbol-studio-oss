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

open Server.TYPES

let on_notification state notif =
  match state, notif with
  | ShuttingDown, Lsp.Client_notification.Exit ->
      Exit (Ok ())
  | ShuttingDown, _ ->
      ShuttingDown
  | NotInitialized _ | Exit _ as state, _ ->
      state                   (* spec indicate notif should just be discarded *)
  | Initialized config, Initialized ->
      Running (Server.init ~config)
  | Running registry, TextDocumentDidOpen params ->
      Running (Server.did_open params registry)
  | Running registry, TextDocumentDidChange params ->
      Running (Server.did_change params registry)
  | Running registry, TextDocumentDidClose params ->
      Running (Server.did_close params registry)
  | Running _, Exit ->
      Request.shutdown state;
      Exit (Error "Received premature 'exit' notification")
  | _ ->
      state

let handle notif state =
  match Lsp.Client_notification.of_jsonrpc notif with
  | Error str ->
      Lsp_io.pretty_notification ~type_:Error "Invalid@ notification:@ %s" str;
      state
  | Ok notif ->
      try on_notification state notif with
      | Server.Document_not_found { uri } ->
          Lsp_io.pretty_notification ~type_:Error
            "Document@ %s@ is@ not@ opened@ yet"
            (Lsp.Types.DocumentUri.to_string uri);
          state
      | e ->
          Lsp_io.pretty_notification ~type_:Error "%a" Fmt.exn e;
          state
