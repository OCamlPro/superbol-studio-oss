(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** References to elements of documents. *)

(** [locations ~doc loc_translator ?cu_name element references] gives the
    locations of all references to [element] (excluding its definition), using
    the [references] computed by the typechecker for [doc].  [cu_name] is the
    name of the compilation unit that encloses [element]. *)
val locations
  : doc:Lsp_document.t
  -> Lsp_position.translator
  -> ?cu_name:string
  -> Lsp_lookup.TYPES.element_in_context
  -> Cobol_typeck.Outputs.references_in_group
  -> Lsp.Types.Location.t list
