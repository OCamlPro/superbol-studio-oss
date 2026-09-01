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

type ('f, 'r, 'm, 's) internal_data =
  {
    vm: ('f, 'r, 'm, 's) manager;
    module_handle: ('f, 'm) module_handle;
  }

(* TODO: should appear in functions below, in a parametric way. *)
type 's computation_state =
  | Running of 's
  | Stopping of 's * int                             (* int status... for now? *)

(* --- *)

let rec run_block data state statements =
  match state, statements with
  | Stopping (state, status), _ ->
      Ok (state, status)
  | Running state, [] ->
      Ok (state, 0)
  | Running state, stmt :: next_statments ->
      let* state = run_statement data state stmt in
      run_block data state next_statments

and run_statement { vm; _ } state stmt =
  match ~&stmt with
  | IR_display { fields; advancing } ->
      let* state = vm.display_fields ~vm ~advancing fields state in
      Ok (Running state)
  | IR_stop { optional_status = None } ->
      Ok (Stopping (state, 0))
  | IR_stop { optional_status = Some f } ->
      let* state, status = vm.field_as_int ~vm f state in
      Ok (Stopping (state, status))

(* and update ~vm { state; flow } = *)
(*   match flow with *)
(*   | Result res -> *)
(*       Ok res *)
(*   | Exception _  *)

let run_proc data block state =
  run_block data (Running state) block

let run_module ~vm module_handle state =
  let* state = Module.init ~vm module_handle state in
  vm.enter_module module_handle.module_memory ~params:[| |] ;
  let data = { vm; module_handle } in
  let* state, status = run_proc data module_handle.module_proc state in
  vm.leave_module module_handle.module_memory;
  Ok (state, status)
