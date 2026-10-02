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

val of_compilation_unit
  : options:Typeck_config.options
  -> ?parent_env:Cobol_unit.Types.unit_env
  -> Cobol_ptree.compilation_unit with_loc
  -> Cobol_unit.Types.unit_env * Typeck_diagnostics.t

(* Additional, temporary... *)

(** Default sign positioning for DISPLAY items *)
val default_display_sign_config: Cobol_data.Types.display_sign_config
