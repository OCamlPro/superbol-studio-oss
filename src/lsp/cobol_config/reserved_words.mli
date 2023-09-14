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

open EzCompat

val words : (string * Types.word_spec) list
val default_intrinsics : StringSet.t
val default_registers : StringSet.t
val intrinsic_functions : StringSet.t
val default_system_names : StringSet.t
val switch_system_names : StringSet.t
val device_system_names : StringSet.t
val feature_system_names : StringSet.t
val system_names : StringSet.t
val registers : StringSet.t
