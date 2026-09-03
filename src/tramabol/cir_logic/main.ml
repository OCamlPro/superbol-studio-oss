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

type ('f, 'r, 'm, 's, 'branch) internal_data =
  {
    vm: ('f, 'r, 'm, 's, 'branch) manager;
    module_handle: ('f, 'r, 'm) module_handle;
  }

(* --- *)

exception STOP of Types.localized_runtime_errors

let eval_data_ref ~vm state f =
  Error.localize_errors ~loc:f.field_ref_loc @@
  vm.field_value ~vm f.field_ref state

let eval_data_refs ~vm state data_refs =
  try
    Result.ok @@
    Array.fold_left_map begin fun state data_ref ->
      match eval_data_ref ~vm state data_ref with
      | Ok res -> res
      | Error errors -> raise (STOP errors)
    end state data_refs
  with STOP errors ->
    Error errors

let rec run_block data state statements =
  match state, statements with
  | Stop (state, status), _ ->
      Ok (state, status)
  | Continue state, [] ->
      Ok (state, 0)
  | Continue state, stmt :: next_statments ->
      let* state = run_statement data state stmt in
      run_block data state next_statments

and run_statement { vm; _ } state stmt =
  Error.localize_errors ~loc:~@stmt @@
  match ~&stmt with
  | IR_display { data_refs; advancing } ->
      let* state, field_values = eval_data_refs ~vm state data_refs in
      let* branch = vm.display_fields ~vm ~advancing field_values state in
      vm.proceed branch
  | IR_stop { optional_status = None } ->
      let* branch = vm.stop ~vm state in
      vm.proceed branch
  | IR_stop { optional_status = Some f } ->
      let* state, status = eval_data_ref ~vm state f in
      let* branch = vm.stop ~vm ~status state in
      vm.proceed branch

let run_proc data block state =
  run_block data (Continue state) block

let run_module ~vm module_handle state =
  let* state = Module.init ~vm module_handle state in
  vm.enter_module module_handle.module_memory ~params:[| |] ;
  let data = { vm; module_handle } in
  let* state, status = run_proc data module_handle.module_proc state in
  vm.leave_module module_handle.module_memory;
  Ok (state, status)
