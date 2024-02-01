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

(** Utilities to deal with sections/paragraphs of PROCEDURE DIVISIONs *)

val find
  : ?in_section: Unit_types.procedure_section
  -> Cobol_ptree.Types.qualname
  -> Unit_types.procedure
  -> Unit_types.procedure_block

val full_qn
  : ?in_section: Unit_types.procedure_section
  -> Cobol_ptree.Types.qualname
  -> Unit_types.procedure
  -> Cobol_ptree.Types.qualname
