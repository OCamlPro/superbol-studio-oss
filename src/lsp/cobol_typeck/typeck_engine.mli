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

open Cobol_common.Diagnostics.TYPES

val analyze_compilation_group
  : ?config: Cobol_config.t
  -> _ Cobol_parser.Outputs.parsed_compilation_group
  -> (Cobol_unit.Types.group *
      Cobol_ptree.compilation_group option) with_diags
