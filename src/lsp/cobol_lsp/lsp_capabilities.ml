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

open Lsp.Types

let degraded =
  Sys.backend_type = Other "js_of_ocaml"

(* Client capabilities are to be used for special request response, for example
   a definition request can be answered with a LocationLink iff the client
   supports it.

   NOTE: For now we don't use them because we don't have any special
   response. *)
let reply (_: ClientCapabilities.t) =
  let sync =
    TextDocumentSyncOptions.create ()
      ~openClose:true
      ~change:Incremental
  and semtoks =
    let legend =
      SemanticTokensLegend.create
        ~tokenTypes:Lsp_semtoks.token_types
        ~tokenModifiers:Lsp_semtoks.token_modifiers
    and full =
      if degraded
      then `Bool false
      else `Full (SemanticTokensOptions.create_full ~delta:false ())
    in
    SemanticTokensOptions.create () ~full ~range:true ~legend
  and hover =
    HoverOptions.create ()
  and completion_option =
    CompletionOptions.create ()
  and workspace =
    let workspaceFolders =
      WorkspaceFoldersServerCapabilities.create ()
        ~supported:true     (* server supports multiple folders per workspace *)
        ~changeNotifications:(`Bool true)
    in
    ServerCapabilities.create_workspace ()
      ~workspaceFolders
  and codeLensProvider = CodeLensOptions.create () in
  ServerCapabilities.create ()
    ~textDocumentSync:(`TextDocumentSyncOptions sync)
    ~definitionProvider:(`Bool true)
    ~referencesProvider:(`Bool true)
    ~documentRangeFormattingProvider: (`Bool true)
    ~documentFormattingProvider: (`Bool true)
    ~semanticTokensProvider:(`SemanticTokensOptions semtoks)
    ~hoverProvider:(`HoverOptions hover)
    ~foldingRangeProvider:(`Bool true)
    ~completionProvider:completion_option
    ~workspace
    ~documentSymbolProvider:(`Bool true)
    ~codeLensProvider
    ~renameProvider:(`Bool true)
