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

(* In addition to the type variables mentioned in {!Cir_types}, this file uses:

   - ['bool] to denote the type of evaluated conditions; a concrete interpreter
     will typically use ['bool = bool];

   - ['s] to denote the type of runtime states; a concrete imperative
     interpreter will typically use ['s = unit];

   - ['k] to denote the mode of an interpreter, that is either {!det} or
     {!nondet}. *)

open Cir_types

module NEL = Cobol_common.Basics.NEL

(* --- *)

type runtime_operation = ..

(*  *)

(* Note: call `Printer.register_runtime_error_printer` and
   `Error.register_runtime_error_loc_retriever` when extending this type. *)
type runtime_error = ..

(* Note: edit the corresponding functions in `printer.ml` and `error.ml` when
   adjusting this type. *)
type runtime_error +=
  | Unsupported_runtime_operation of runtime_operation
  | Step_budget_exhausted of
      {
        max_steps: int;
      }
  | Perform_stack_overflow of
      {
        perform_stack_size: int;
      }

type localized_runtime_errors = localized_runtime_error NEL.t
and localized_runtime_error =
  {
    loc: Cobol_common.Srcloc.TYPES.srcloc option;
    error: runtime_error;
  }

(* --- *)

(** The outcome of executing a step of the semantics. *)
type ('f, 's) step_outcome =
  | Running of 's runtime_configuration
  | Halted of
      {
        state: 's;
        status: 'f;
      }

(** Runtime configuration: the control stack part is kept apart from the state
    so that it remains concrete, even when the state is symbolic. *)
and 's runtime_configuration =
  {
    stack: activation NEL.t;                                 (* control state *)
    state: 's;                                                (* data state *)
  }

(** State of an active COBOL unit (i.e, not terminated call to a unit). *)
and activation =
  {
    act_pc: pc;                                          (* procedure-wise *)
    act_performs: perform_frame list;                    (* stack of PERFORMs *)
  }

(** Position of the next instruction to execute in a procedure. *)
and pc =
  {
    pc_block: label;                                       (* block reference *)
    pc_offset: int;                                        (* offset in block *)
  }

and perform_frame =
  {
    frame_exit: exit_point;
    frame_return: label;
  }

(* --- *)

type run_options =
  {
    max_steps: int option;
    perform_stack_size: int;
  }

(* --- *)

type det = Deterministic_mode
type nondet = Nondeterministic_mode

(** COBOL exceptions... not represented for now.  May need to be part of
    Cir_type if this kind of data needs to be manipulated explicitly by COBOL
    operations... *)
type cobol_exception = |

type ('f, 'bool, 'r, 'm, 's, 'k) manager =
  {
    mode:
      ('f, 's, 'k) mode;

    enter_module:
      'm -> params: 'f array -> 's -> 's evaluation_result;
    leave_module:
      'm -> 's -> 's evaluation_result;
    module_ws_initialized:
      'm -> 's -> ('s * bool) evaluation_result;
    module_ws_initialization_done:
      'm -> 's -> 's evaluation_result;

    init_field:
      vm: 'vm -> 'f field_access -> 's -> 's evaluation_result;
    field_value:
      vm: 'vm -> 'f field_reference -> 's -> ('s * 'f) evaluation_result;
    data_value:
      vm: 'vm -> 'f data_reference -> 's -> ('s * 'f) evaluation_result;

    eval_condition:
      vm: 'vm -> 'f condition -> 's -> ('s * 'bool) evaluation_result;

    decide:
      vm: 'vm -> 'bool -> 's -> ('s * bool, 'k) branches evaluation_result;
    decide_int_value:         (* [None] if [f] is not within the given bounds *)
      vm: 'vm -> 'f -> bounds: int * int -> 's ->
      ('s * int option, 'k) branches evaluation_result;
    set_counter:                    (* the counter then holds the given value *)
      vm: 'vm -> counter -> 'f -> 's -> 's evaluation_result;
    count_down:            (* decrements the counter if positive, and says so *)
      vm: 'vm -> counter -> 's -> ('s * bool, 'k) branches evaluation_result;
    stop:   (* gives the exit status, [status] if provided (0 field otherise) *)
      vm: 'vm -> ?status: 'f -> 's -> ('s * 'f) evaluation_result;

    display_fields:
      vm: 'vm -> advancing: bool -> 'f array -> 's -> 's evaluation_result;
    move:
      vm: 'vm -> 'f -> into: 'f -> 's -> 's evaluation_result;

    arithmetic:
      vm: 'vm -> arithmetic_operator -> 'f -> target: 'f -> rounded: bool ->
      checked: bool -> 's -> 's evaluation_result;
    clear_size_error:
      vm: 'vm -> 's -> 's evaluation_result;
    size_error:
      vm: 'vm -> 's -> ('s * 'bool) evaluation_result;
  }
  constraint 'vm = ('f, 'bool, 'r, 'm, 's, 'k) manager

and ('f, 's, 'k) mode =
  | Det_mode:
      ('f, 's, det) mode
  | Nondet_mode:
      {
        merge_outcomes:
          (* how a set of step outcomes are merged into results and pending
             configs; the latter are explored in the order they are returned. *)
          ('f, 's) step_outcome NEL.t ->
          (('s * 'f) evaluation_result list as 'results) ->
          ('s runtime_configuration list as 'runtime_configs) ->
          'results * 'runtime_configs;
      } -> ('f, 's, nondet) mode

and 's evaluation_result = ('s, localized_runtime_errors) result

and ('a, 'k) branches =
  | Det: 'a -> ('a, det) branches
  | Nondet: 'a NEL.t -> ('a, nondet) branches
