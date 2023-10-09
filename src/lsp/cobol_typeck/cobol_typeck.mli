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

val analyze_compilation_group
  : ?config: (module Cobol_config.T)
  -> _ Cobol_parser.Outputs.parsed_compilation_group
  -> (Cobol_data.Compilation_unit.SET.t * Cobol_ptree.compilation_group option)
    Cobol_common.Diagnostics.with_diags
