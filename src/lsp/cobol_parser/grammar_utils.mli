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

module Overlay_manager: Cobol_preproc.Src_overlay.MANAGER

val relation_condition
  : neg: bool
  -> Cobol_ast.binary_relation
  -> (Cobol_ast.logop * Cobol_ast.flat_combined_relation) option
  -> Cobol_ast.condition
