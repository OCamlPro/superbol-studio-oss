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

val compilation_group
  : ?config: Cobol_config.Types.t
  -> _ Cobol_parser.Outputs.parsed_compilation_group
  -> Outputs.t * Diagnostics.t

val translate_diagnostics
  : ?config: Cobol_config.Types.t
  -> Outputs.t * Diagnostics.t
  -> Outputs.t Cobol_common.Diagnostics.with_diags
