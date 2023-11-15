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

(** Definitions of module aliases and helper functors *)

module Cobol_data = Cobol_data.OLD

module CUs = Cobol_data.Compilation_unit.SET
module CUMap = Cobol_data.Compilation_unit.MAP
module URIMap = Map.Make (Lsp.Uri)
