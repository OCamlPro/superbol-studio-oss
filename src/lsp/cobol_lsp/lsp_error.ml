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

(** {1 Error handling} *)

(** Raises {!Jsonrpc.Response.Error.E} *)
let error ~code fmt =
  Pretty.string_to begin fun message ->
    Jsonrpc.Response.Error.(raise @@ make ~code ~message ())
  end fmt

let request_failed fmt =
  error ~code:Jsonrpc.Response.Error.Code.RequestFailed fmt

let invalid_params fmt =
  error ~code:Jsonrpc.Response.Error.Code.InvalidParams fmt

let internal fmt =
  error ~code:Jsonrpc.Response.Error.Code.InternalError fmt
