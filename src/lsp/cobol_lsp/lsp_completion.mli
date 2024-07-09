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

type case =
  | Auto
  | Uppercase
  | Lowercase

type config

val config
  : ?eager:bool
  -> ?case:case
  -> unit
  -> config

val context_completion_items
  : config:config
  -> Lsp_document.t
  -> Lsp_document.checked_doc
  -> Lsp.Types.Position.t
  -> Lsp.Types.CompletionItem.t list
