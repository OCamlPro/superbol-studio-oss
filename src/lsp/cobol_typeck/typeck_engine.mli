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
  -> Typeck_outputs.t * Typeck_diagnostics.t

val translate_diagnostics
  : ?config: Cobol_config.Types.t
  -> Typeck_outputs.t * Typeck_diagnostics.t
  -> Typeck_outputs.t Cobol_common.Diagnostics.with_diags
