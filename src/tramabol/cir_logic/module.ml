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

open Cir_types
open Types

open Syntax

(* --- *)

let init_field ~vm f state =
  vm.init_field ~vm f state

let init_fields ~vm fields state =
  List.fold_left begin fun state field ->
    let* state in
    init_field ~vm field state
  end (Ok state) fields

let init ~vm (m: _ module_handle) state =
  let ws_init = vm.module_ws_needs_initialization m.module_memory in
  let* state =
    if ws_init
    then Ok state
    else init_fields ~vm m.module_fields.working_storage state
  in
  let* state =
    init_fields ~vm m.module_fields.local_storage state
  in
  vm.module_ws_initialization_done m.module_memory state
