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

module TYPES: sig
  type alternate_handler =
    {
      h: 'reply. 'reply Lsp.Client_request.t -> Lsp_server.registry ->
        ('reply * Lsp_server.registry, 'reply Lsp_server.error) result;
    }
end

val handle
  : ?alternate_handlers: TYPES.alternate_handler list
  -> Jsonrpc.Request.t
  -> (Lsp_server.state as 's)
  -> 's * Jsonrpc.Response.t
val shutdown: Lsp_server.state -> unit

val try_with_doc
  : f:(doc:Lsp_document.t -> 'a option)
  -> Lsp_server.registry
  -> Lsp.Types.TextDocumentIdentifier.t
  -> 'a option
val try_with_checked_doc
  : f:(doc:Lsp_document.t -> Lsp_document.checked_doc -> 'a option)
  -> Lsp_server.registry
  -> Lsp.Types.TextDocumentIdentifier.t
  -> 'a option
val fallback       (* fallback code to report unknown or unhandled requests *)
  : 'reply Lsp.Client_request.t
  -> (_, 'reply Lsp_server.error) result

(** Functions exported for testing purposes *)
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
    : ?always_show_hover_definition_text_in_data_div: bool
    -> Lsp_server.t
    -> Lsp.Types.HoverParams.t
    -> Lsp.Types.Hover.t option
  val completion
    : ?eager:bool
    -> Lsp_server.t
    -> Lsp.Types.CompletionParams.t
    -> [> `CompletionList of Lsp.Types.CompletionList.t] option
  val formatting
    : Lsp_server.t
    -> Lsp.Types.DocumentFormattingParams.t
    -> Lsp.Types.TextEdit.t list option
  val document_symbol
    : Lsp_server.t
    -> Lsp.Types.DocumentSymbolParams.t
    -> [> `DocumentSymbol of Lsp.Types.DocumentSymbol.t list ] option
  val codelens
    : Lsp_server.t
    -> Lsp.Types.CodeLensParams.t
    -> Lsp.Types.CodeLens.t list
  val rename
    : ?abort_when_in_copybook:bool
    -> Lsp_server.t
    -> Lsp.Types.RenameParams.t
    -> Lsp.Types.WorkspaceEdit.t
end
