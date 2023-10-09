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

(* exception SemanticError of string * srcloc *)

val validate_data_clauses
  : ?is_elementary: bool
  -> Cobol_ptree.data_item Cobol_ptree.with_loc
  -> Cobol_common.Diagnostics.Set.t
