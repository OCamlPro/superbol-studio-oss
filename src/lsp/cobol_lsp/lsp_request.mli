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

val handle: Jsonrpc.Request.t -> (Lsp_server.state as 's) -> 's * Jsonrpc.Response.t
val shutdown: Lsp_server.state -> unit

module INTERNAL: sig
  val lookup_definition
    : Lsp_server.t
    -> Lsp.Types.DefinitionParams.t
    -> [> `Location of Lsp.Types.Location.t list ] option
  val lookup_references
    : Lsp_server.t
    -> Lsp.Types.ReferenceParams.t
    -> Lsp.Types.Location.t list option
  val hover
    : Lsp_server.t
    -> Lsp.Types.HoverParams.t
    -> Lsp.Types.Hover.t option
  val formatting
    : Lsp_server.t
    -> Lsp.Types.DocumentFormattingParams.t
    -> Lsp.Types.TextEdit.t list option
end
