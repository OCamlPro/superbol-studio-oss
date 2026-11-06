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

(** [normalize_uri uri] is the URI that designates the same file as [uri], and
    that the functions below use as keys of the diagnostics they give.  URIs
    that designate the same file may be spelled differently (e.g. the case of a
    Windows drive letter): those of documents should be normalized before being
    looked up in such maps. *)
val normalize_uri: Lsp.Uri.t -> Lsp.Uri.t

val translate_one
  : ?focus_on_main_doc: bool
  -> rootdir: string
  -> uri: Lsp.Uri.t
  -> Cobol_common.Diagnostics.t -> diagnostics

val translate
  : ?focus_on_main_doc: bool
  -> rootdir: string
  -> uri: Lsp.Uri.t
  -> Cobol_common.Diagnostics.Set.t -> diagnostics

val publish
  : diagnostics
  -> unit

val as_notification
  : ?log:bool -> Cobol_common.Diagnostics.t -> Lsp.Server_notification.t

val as_notifications
  : ?log:bool -> Cobol_common.Diagnostics.Set.t -> Lsp.Server_notification.t list
