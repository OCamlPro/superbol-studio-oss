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

open Lsp_imports

module DIAG = Cobol_common.Diagnostics

module TYPES: sig
  type diagnostics = Lsp.Types.Diagnostic.t list URIMap.t
end
include module type of TYPES
  with type diagnostics = TYPES.diagnostics
type t = diagnostics

val translate_one
  : rootdir:string
  -> uri:[< `Force of Lsp.Uri.t | `Main of Lsp.Uri.t ]
  -> Cobol_common.Diagnostics.t -> diagnostics

val translate
  : rootdir:string
  -> uri:[< `Force of Lsp.Uri.t | `Main of Lsp.Uri.t ]
  -> Cobol_common.Diagnostics.Set.t -> diagnostics

val publish
  : diagnostics
  -> unit

val as_notification
  : ?log:bool -> Cobol_common.Diagnostics.t -> Lsp.Server_notification.t
