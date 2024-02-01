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

type output =
  {
    definitions: Cobol_unit.Types.data_definitions;
    references: Typeck_outputs.qualrefmap;
  }

val of_compilation_unit
  : Cobol_unit.Types.unit_config
  -> Cobol_ptree.Types.compilation_unit with_loc
  -> output * Typeck_diagnostics.t
