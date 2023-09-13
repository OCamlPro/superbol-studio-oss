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

open Cobol_common.Srcloc.TYPES

exception SemanticError of string * srcloc

val validate_data_clauses :
  (module Cobol_common.Diagnostics.STATEFUL) -> ?is_elementary:bool ->
  Cobol_data.Pictured_ast.data_item with_loc -> unit
