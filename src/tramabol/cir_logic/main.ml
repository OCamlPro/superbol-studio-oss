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

(* Only used locally, shouldn't escape. *)
exception STOP of Types.localized_runtime_errors

let eval_data_ref ~vm state f =
  Error.localize_errors ~loc:f.data_ref_loc @@
  vm.data_value ~vm f state

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

let rec run_block ~vm branch statements stack =
  match branch, statements, stack with
  | Stop (state, status), _, _ ->
      Ok (state, status)
  | Continue state, [], [] ->
      Ok (state, 0)
  | Perform (state, code_block), next_statements, stack ->
      run_block ~vm (Continue state) code_block (next_statements :: stack)
  | branch, [], code_block :: stack ->
      run_block ~vm branch code_block stack
  | Continue state, stmt :: next_statments, stack ->
      let* state = run_statement ~vm state stmt in
      run_block ~vm state next_statments stack

and run_statement ~vm state stmt =
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
  | IR_conditional { condition; then_branch; else_branch } ->
      let* state, c = vm.eval_condition ~vm condition state in
      let* branch = vm.conditional ~vm c then_branch else_branch state in
      vm.proceed branch

let run_proc ~vm block state =
  run_block ~vm (Continue state) block []

let run_module ~vm module_handle state =
  let* state = Module.init ~vm module_handle state in
  vm.enter_module module_handle.module_memory ~params:[| |] ;
  let* state, status = run_proc ~vm module_handle.module_proc state in
  vm.leave_module module_handle.module_memory;
  Ok (state, status)
