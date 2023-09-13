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
  | Initialized config, Initialized ->
      Running (Lsp_server.init ~config)
  | Running registry, TextDocumentDidOpen params ->
      Running (Lsp_server.add params registry)
  | Running registry, TextDocumentDidChange params ->
      Running (Lsp_server.update params registry)
  | Running registry, TextDocumentDidClose params ->
      Running (Lsp_server.remove params registry)
  | Running _, Exit ->
      Exit (Error "Received premature 'exit' notification")
  | _ ->
      state

let handle notif status =
  match Lsp.Client_notification.of_jsonrpc notif with
  | Error str ->
      Pretty.failwith "LSP@ sever@ could@ not@ decode@ notification:@ %s" str
  | Ok notif ->
      on_notification status notif
