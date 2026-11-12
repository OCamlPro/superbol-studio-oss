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

let eval_field_ref ~vm state f =
  Error.localize_errors ~loc:f.field_ref_loc @@
  vm.field_value ~vm f state

let eval_data_ref ~vm state f =
  Error.localize_errors ~loc:f.data_ref_loc @@
  vm.data_value ~vm f state

let eval_data_refs ~vm state data_refs =
  try
    ok @@ Array.fold_left_map begin fun state data_ref ->
      match eval_data_ref ~vm state data_ref with
      | Ok res -> res
      | Error errors -> raise (STOP errors)
    end state data_refs
  with STOP errors ->
    error errors

let run_arithmetic ~vm ~checked { operator; operand; targets } state =
  let* state, operand = eval_field_ref ~vm state operand in
  NEL.fold_left ~f:begin fun state { target; rounded } ->
    let* state in
    let* state, target = eval_field_ref ~vm state target in
    vm.arithmetic ~vm operator operand ~target ~rounded ~checked state
  end (Ok state) targets

let run_straight_statement ~vm state stmt =
  Error.localize_errors ~loc:~@stmt @@
  match ~&stmt with
  | IR_display { data_refs; advancing } ->
      let* state, field_values = eval_data_refs ~vm state data_refs in
      vm.display_fields ~vm ~advancing field_values state
  | IR_move { source; targets } ->
      let* state, value = eval_data_ref ~vm state source in
      NEL.fold_left ~f:begin fun state target ->
        let* state in
        let* state, into = eval_data_ref ~vm state target in
        vm.move ~vm value ~into state
      end (Ok state) targets
  | IR_arithmetic arithmetic ->
      run_arithmetic ~vm ~checked:false arithmetic state

let runtime_error error =
  Syntax.error (NEL.one { loc = None; error })

let block_start pc_block =
  { pc_block; pc_offset = 0 }

let set_pc stack act_pc =
  NEL.map_hd stack ~f:(fun act -> { act with act_pc })

let jump stack pc_block =
  set_pc stack @@ block_start pc_block

let perform ~options stack ~entry ~exit_point ~return_to =
  let { act_performs; _ } = NEL.hd stack in
  if List.compare_length_with act_performs options.perform_stack_size >= 0
  then
    runtime_error @@
    Perform_stack_overflow { perform_stack_size = options.perform_stack_size }
  else
    let frame = { frame_exit = exit_point; frame_return = return_to } in
    ok @@ NEL.map_hd stack ~f:begin fun _ ->
      { act_pc = block_start entry; act_performs = frame :: act_performs }
    end

let perform_exit stack ~exit_points ~fallthrough =
  NEL.map_hd stack ~f:begin fun act ->
    match act.act_performs with
    | { frame_exit; frame_return } :: act_performs
      when NEL.exists exit_points ~f:(( = ) frame_exit) ->
        { act_pc = block_start frame_return; act_performs }
    | _ ->
        { act with act_pc = block_start fallthrough }
  end

(** Leaves the current activation. *)
let leave_activation ~vm { stack; state } =
  match stack with
  | NEL.One _ ->
      let+ state, status = vm.stop ~vm state in
      Branches.single vm.mode @@ Halted { state; status }
  | NEL.(_ :: stack) ->
      ok @@
      Branches.single vm.mode @@ Running { stack; state }

let run_terminator ~options ~vm ({ stack; state } as config) term =
  Error.localize_errors ~loc:~@term @@
  match ~&term with
  | IR_goto label ->
      ok @@ Branches.single vm.mode @@
      Running { stack = jump stack label; state }
  | IR_branch { condition; if_true; if_false } ->
      let* state, c = vm.eval_condition ~vm condition state in
      let+ decisions = vm.decide ~vm c state in
      Branches.map decisions ~f:begin fun (state, c) ->
        let label = if c then if_true else if_false in
        Running { stack = jump stack label; state }
      end
  | IR_goto_depending { selector; targets; default } ->
      let* state, f = eval_field_ref ~vm state selector in
      let+ selections =
        vm.decide_int_value ~vm f ~bounds:(1, NEL.length targets) state
      in
      Branches.map selections ~f:begin fun (state, i) ->
        let label = match i with
          | Some i -> NEL.nth targets (pred i)
          | None -> default
        in
        Running { stack = jump stack label; state }
      end
  | IR_perform { entry; exit_point; return_to } ->
      let+ stack = perform ~options stack ~entry ~exit_point ~return_to in
      Branches.single vm.mode @@ Running { stack; state }
  | IR_perform_exit { exit_points; fallthrough } ->
      let stack = perform_exit stack ~exit_points ~fallthrough in
      ok @@ Branches.single vm.mode @@ Running { stack; state }
  | IR_set_counter { counter; count; next } ->
      let* state, f = eval_field_ref ~vm state count in
      let+ state = vm.set_counter ~vm counter f state in
      Branches.single vm.mode @@ Running { stack = jump stack next; state }
  | IR_count_down { counter; if_positive; if_zero } ->
      let+ decisions = vm.count_down ~vm counter state in
      Branches.map decisions ~f:begin fun (state, positive) ->
        let label = if positive then if_positive else if_zero in
        Running { stack = jump stack label; state }
      end
  | IR_checked_arithmetic { arithmetic; on_size_error; not_on_size_error } ->
      let* state = vm.clear_size_error ~vm state in
      let* state = run_arithmetic ~vm ~checked:true arithmetic state in
      let* state, c = vm.size_error ~vm state in
      let+ decisions = vm.decide ~vm c state in
      Branches.map decisions ~f:begin fun (state, c) ->
        let label = if c then on_size_error else not_on_size_error in
        Running { stack = jump stack label; state }
      end
  | IR_goback ->
      leave_activation ~vm config
  | IR_exit_program { fallthrough } ->
      begin match stack with
        | NEL.One _ ->
            ok @@ Branches.single vm.mode @@
            Running { stack = jump stack fallthrough; state }
        | NEL.(_ :: _) ->
            leave_activation ~vm config
      end
  | IR_stop { optional_status = None } ->
      let+ state, status = vm.stop ~vm state in
      Branches.single vm.mode @@ Halted { state; status }
  | IR_stop { optional_status = Some f } ->
      let* state, status = eval_data_ref ~vm state f in
      let+ state, status = vm.stop ~vm ~status state in
      Branches.single vm.mode @@ Halted { state; status }

let step ~options ~vm (proc: _ procedure) ({ stack; state } as config) =
  let { act_pc = { pc_block; pc_offset }; _ } = NEL.hd stack in
  let block = proc.proc_blocks.(pc_block) in
  if pc_offset < Array.length block.block_stmts
  then
    let+ state = run_straight_statement ~vm state block.block_stmts.(pc_offset) in
    let stack = set_pc stack { pc_block; pc_offset = succ pc_offset } in
    Branches.single vm.mode @@ Running { stack; state }
  else
    run_terminator ~options ~vm config block.block_term

(* --- *)

let default_run_options =
  {
    max_steps = None;
    perform_stack_size = 255;
  }

(** Deterministic interpretation *)
let run ~(vm: (_, _, _, _, _, det) manager) ~options proc config =
  let rec aux ~budget config =
    match budget with
    | Some 0 ->
        let max_steps = Option.get options.max_steps in
        runtime_error @@ Step_budget_exhausted { max_steps }
    | _ ->
        let* branches = step ~options ~vm proc config in
        match branches with
        | Det Running config ->
            aux ~budget:(Option.map pred budget) config
        | Det Halted { state; status } ->
            ok (state, status)
  in
  aux ~budget:options.max_steps config

(** Nondeterministic interpretation: explores every execution path, within
    [options.max_steps] steps overall; paths that remain unexplored once the
    budget is exhausted end with an error.

    At each step, [merge] is given the step outcomes, the (reverse) list of
    execution path termination results, and the (reverse) list of
    pending/non-terminated runtime configurations. *)
let explore ~merge ~options ~vm proc config =
  let rec aux ~budget rev_results = function
    | [] ->
        NEL.of_rev_list rev_results
    | pending when budget = Some 0 ->
        let max_steps = Option.get options.max_steps in
        let exhausted = runtime_error @@ Step_budget_exhausted { max_steps } in
        NEL.of_rev_list @@
        List.rev_append (List.map (fun _ -> exhausted) pending) rev_results
    | config :: pending_configs ->
        let budget = Option.map pred budget in
        match step ~options ~vm proc config with
        | Error _ as error ->
            aux ~budget (error :: rev_results) pending_configs
        | Ok branches ->
            let step_outcomes = Branches.to_step_outcomes branches in
            let rev_results, pending_configs =
              merge step_outcomes rev_results pending_configs
            in
            aux ~budget rev_results pending_configs
  in
  aux ~budget:options.max_steps [] [config]

let run_proc (type k) ~options ~(vm: (_, _, _, _, _, k) manager)
    (proc: _ procedure) state : (_, k) branches =
  let act_pc = block_start proc.proc_entry in
  let stack = NEL.one { act_pc; act_performs = [] } in
  let config = { stack; state } in
  match vm.mode with
  | Det_mode ->
      Det (run ~vm ~options proc config)
  | Nondet_mode { merge_outcomes = merge } ->
      Nondet (explore ~merge ~vm ~options proc config)

let run_module ?(options = default_run_options) ~vm module_handle state =
  let memory = module_handle.module_memory in
  match
    let* state = Module.init ~vm module_handle state in
    vm.enter_module memory ~params:[| |] state
  with
  | Error _ as error ->
      Branches.single vm.mode error
  | Ok state ->
      run_proc ~options ~vm module_handle.module_proc state |>
      Branches.map ~f:begin fun result ->
        let* state, status = result in
        let+ state = vm.leave_module memory state in
        state, status
      end
