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

open Types

val create
  : name:string
  -> source_file:string
  -> cob_module_memory
val enter
  : cob_module_memory
  -> params:cob_field array
  -> state
  -> evaluation_result
val leave
  : cob_module_memory
  -> state
  -> evaluation_result

val ws_initialized
  : cob_module_memory
  -> state
  -> (state * bool) Cir_logic.Types.evaluation_result
val ws_initialization_done
  : cob_module_memory
  -> state
  -> evaluation_result
