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

(** Test-only builder and non-deterministic manager.  Abstracts away from data:
    fields are only described by their name or value, conditions are unknown,
    and execution traces are recorded functionally. *)

open Cir_types
open Cir_logic.Types

open Cobol_common.Srcloc.INFIX

type field = string

module COUNTERS = Map.Make (Int)

(** Counter values are abstracted with intervals. *)
type state =
  {
    rev_trace: string list;
    ws_initializations: int;
    counters: (int * int) COUNTERS.t;
  }

let initial_state =
  { rev_trace = []; ws_initializations = 0; counters = COUNTERS.empty }

let () =
  Cir_builder.Error.register_loc_retrievers ();
  Cir_builder.Printer.register_printers ();
  Cir_logic.Error.register_loc_retrievers ();
  Cir_logic.Printer.register_printers ()

let builder: (field, unit, unit) Cir_builder.Types.builder =
  {
    create_record_data = begin fun record_definition ->
      Ok { record_memory = (); record_definition }
    end;
    create_field_from_definition = begin fun def _record ->
      Ok (match ~&def.field_qualname with
          | Some qn -> Pretty.to_string "%a" Cobol_ptree.pp_qualname ~&qn
          | None -> "<anonymous>")
    end;
    create_field_from_literal_value = begin fun lit ->
      Ok (Pretty.to_string "%a" Cobol_data.Printer.pp_value ~&lit)
    end;
    create_module_memory = (fun ~name:_ ~source_file:_ -> ());
    const_fields = Cir_builder.Types.CONST_TABLE.create 7;
  }

let rec describe_field: field Cir_types.field -> field = function
  | Constant_field f
  | Field_in_memory { field = Fixed_field f; _ } -> f
  | Field_in_memory { field = Table_field c; field_info } ->
      describe_field (Field_in_memory { field = c.cell_first_field;
                                        field_info })

(** Interval of the possible values of a count. *)
let count_interval f =
  match int_of_string_opt f with
  | Some i -> i, i
  | None -> 0, 2

(** Possible outcomes of decrementing a counter if it is positive. *)
let count_down counter state =
  let lo, hi = COUNTERS.find counter state.counters in
  let with_interval i =
    { state with counters = COUNTERS.add counter i state.counters }
  in
  List.filter_map Fun.id [
    if lo <= 0 then Some (with_interval (0, 0), false)
    else None;
    if hi > 0 then Some (with_interval (max 0 (pred lo), pred hi), true)
    else None;
  ]

let merge_outcomes step_outcomes rev_results rev_configs =
  let rev_running, rev_results =
    NEL.fold_left ~f:begin fun (rev_running, rev_results) -> function
      | Running config ->
          config :: rev_running, rev_results
      | Halted { state; status } ->
          rev_running, Ok (state, status) :: rev_results
    end ([], rev_results) step_outcomes
  in
  (* merge for depth-first *)
  rev_results, List.rev_append rev_running rev_configs

let manager: (field, unit, unit, unit, state, nondet) manager =
  {
    mode = Nondet_mode { merge_outcomes } ;
    enter_module = (fun () ~params:_ state -> Ok state);
    leave_module = (fun () state -> Ok state);
    module_ws_initialized = begin fun () state ->
      Ok (state, state.ws_initializations > 0)
    end;
    module_ws_initialization_done = begin fun () state ->
      Ok { state with ws_initializations = succ state.ws_initializations }
    end;
    init_field = (fun ~vm:_ _ state -> Ok state);
    field_value = begin fun ~vm:_ f state ->
      Ok (state, describe_field f.field_ref)
    end;
    data_value = begin fun ~vm:_ d state ->
      Ok (state, describe_field d.data_field)
    end;
    eval_condition = (fun ~vm:_ _ state -> Ok (state, ()));
    decide = begin fun ~vm:_ () state ->
      Ok (Nondet NEL.((state, true) :: One (state, false)))
    end;
    decide_int_value = begin fun ~vm:_ f ~bounds:(lb, ub) state ->
      let selection i = state, if i >= lb && i <= ub then Some i else None in
      match int_of_string_opt f with
      | Some i ->
          Ok (Nondet (NEL.one @@ selection i))
      | None ->
          let ascending =
            NEL.of_rev_list @@
            List.init (ub - lb + 1) (fun i -> selection @@ ub - i)
          in
          Ok (Nondet ((state, None) :: ascending))           (* [None] upfront *)
    end;
    set_counter = begin fun ~vm:_ counter f state ->
      let i = count_interval f in
      Ok { state with counters = COUNTERS.add counter i state.counters }
    end;
    count_down = begin fun ~vm:_ counter state ->
      Ok (Nondet (NEL.of_list (count_down counter state)))
    end;
    display_fields = begin fun ~vm:_ ~advancing:_ fields state ->
      let line = String.concat "" (Array.to_list fields) in
      Ok { state with rev_trace = line :: state.rev_trace }
    end;
    move = (fun ~vm:_ _ ~into:_ state -> Ok state);
    arithmetic = begin fun ~vm:_ _ _ ~target:_ ~rounded:_ ~checked:_ state ->
      Ok state
    end;
    clear_size_error = (fun ~vm:_ state -> Ok state);
    size_error = (fun ~vm:_ state -> Ok (state, ()));
    stop = begin fun ~vm:_ ?status state ->
      Ok (state, Option.value status ~default:"0")
    end;
  }

let pp_path ppf = function
  | Ok ({ rev_trace; _ }, status) ->
      Pretty.print ppf "@[<h>%a@] (status %s)"
        Fmt.(list ~sep:(any ",@ ") string) (List.rev rev_trace) status
  | Error errors ->
      Pretty.print ppf "@[%a@]"
        (Cir_logic.Printer.pp_localized_runtime_errors
           ~platform:Prog_common.platform) errors

let with_modules contents ~f =
  Prog_typeck.typeck contents |>
  Cobol_common.Diagnostics.show_n_forget ~set_status:false ~ppf:Fmt.stdout
    ~platform:Prog_common.platform |> fun typeck_outputs ->
  Cobol_unit.Group.fold begin fun unit () ->
    match Cir_builder.Module.of_cobol_unit ~builder unit with
    | Error errors ->
        Cir_builder.Printer.pp_errors ~platform:Prog_common.platform
          Fmt.stdout errors
    | Ok m ->
        f m
  end typeck_outputs.group ()

(** Prints the trace and outcome of every execution path. *)
let explore ?max_steps contents =
  let options = { Cir_logic.Main.default_run_options with max_steps } in
  with_modules contents ~f:begin fun m ->
    let Nondet paths =
      Cir_logic.Main.run_module ~options ~vm:manager m initial_state
    in
    NEL.iter paths ~f:(Pretty.out "%a@." pp_path)
  end

(** Runs a module twice, from the state reached on the first path of the first
    run, and prints the number of initializations of its WORKING-STORAGE. *)
let reenter contents =
  let run m state =
    let Nondet paths = Cir_logic.Main.run_module ~vm:manager m state in
    Result.map fst (NEL.hd paths)
  in
  with_modules contents ~f:begin fun m ->
    match Result.bind (run m initial_state) (run m) with
    | Ok { ws_initializations; _ } ->
        Pretty.out "WORKING-STORAGE initializations: %d@." ws_initializations
    | Error _ as path ->
        Pretty.out "%a@." pp_path path
  end
