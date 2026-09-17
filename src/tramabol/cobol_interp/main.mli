(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Types

val default_options: options

val run_unit
  : ?options: options
  -> Cobol_unit.Types.t
  -> (int, errors) result
val run_group
  : ?options: options
  -> Cobol_unit.Types.group
  -> (int, errors) result

val print_unit
  : ?options: options
  -> Format.formatter
  -> Cobol_unit.Types.t
  -> (state, Cir_builder.Types.errors) result
val print_group
  : ?options: options
  -> Format.formatter
  -> Cobol_unit.Types.group
  -> (state, Cir_builder.Types.errors) result
