(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2024 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Lsp_server.TYPES

let handle (Jsonrpc.Response.{ id; result } as response) state =
  match id, result, state with
  | `Int id, Ok json, Running registry ->
      Running (Lsp_server.on_response id json registry)
  | `Int _, Error e, Running _ ->
      Jsonrpc.Response.Error.raise e
  | _ ->
      Pretty.error "Unexpected@ response:@ %S@."
        (Yojson.Safe.to_string @@ Jsonrpc.Response.yojson_of_t response);
      state
