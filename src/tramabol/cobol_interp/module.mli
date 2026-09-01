(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

val create: name:string -> source_file:string -> Types.cob_module_memory
val enter: Types.cob_module_memory -> params:Types.cob_field array -> unit
val leave: Types.cob_module_memory -> unit

val ws_needs_initialization
  : Types.cob_module_memory -> bool
val ws_initialization_done
  : Types.cob_module_memory -> Types.state -> Types.evaluation_result
