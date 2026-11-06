(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** Integration tests that drive debugging sessions of the bundled extension
    through VS Code commands ([workbench.action.debug.*]), as a user would.

    This module is loaded by VS Code's extension test host (see [run.js]),
    which calls the exported [run] function.  The tests need [cobc] and [gdb]
    in the [PATH], and are skipped otherwise.  They build programs in the
    directory given by the [SUPERBOL_TEST_WORKDIR] environment variable, and
    use the workspace folder given by [SUPERBOL_TEST_PROJECT] to test build
    tasks and debug configurations.  When [SUPERBOL_TEST_SHARD] is set, only
    a shard of the suites runs (see {!shard}).

    Tests are either [Expected] behaviors, whose failures fail the run, or
    [Probe]s for suspected bugs, whose failures are only reported (see
    {!Harness.kind}). *)

(** All suites, in the order they run, with their weights: roughly how many
    seconds they take (as reported at the end of each suite), used to
    balance shards (see {!shard}). *)
let weighted_suites = [
  T_stepping.suite, 9;
  T_breakpoints.suite, 9;
  T_variables.suite, 6;
  T_evaluate.suite, 4;
  T_calls.suite, 8;
  T_lifecycle.suite, 5;
  T_tasks.suite, 17;
  T_launch.suite, 47;
]

(** Suites whose names contain one of the comma-separated words of the
    [SUPERBOL_TEST_SUITES] environment variable, if set. *)
let selected_suites () =
  match Node.Process.Env.get "SUPERBOL_TEST_SUITES" with
  | None | Some "" -> weighted_suites
  | Some words ->
      let words = List.map String.lowercase_ascii (String.split_on_char ',' words) in
      List.filter (fun ({ Harness.suite; _ }, _) ->
          List.exists (fun sub -> Harness.contains ~sub (String.lowercase_ascii suite)) words)
        weighted_suites

(** Splits the given weighted suites into [n] shards of similar total
    weights, by giving each suite in turn, heaviest first, to the lightest
    shard so far (the first one, among equally light ones).  Returns the
    suites of the [k]-th shard, in their original order. *)
let balanced_shard ~k ~n weighted =
  let loads = Array.make n 0 in
  let lightest () =
    let best = ref 0 in
    Array.iteri (fun i load -> if load < loads.(!best) then best := i) loads;
    !best in
  let indexed = List.mapi (fun i (suite, weight) -> i, suite, weight) weighted in
  let heaviest_first =
    List.stable_sort (fun (_, _, w1) (_, _, w2) -> compare w2 w1) indexed in
  let assigned =
    List.map (fun (i, suite, weight) ->
        let shard = lightest () in
        loads.(shard) <- loads.(shard) + weight;
        i, suite, shard) heaviest_first in
  List.sort (fun (i1, _, _) (i2, _, _) -> compare i1 i2) assigned
  |> List.filter_map (fun (_, suite, shard) -> if shard = k then Some suite else None)

(** The [k]-th of [n] shards of the given weighted suites, balanced by
    weight, when the [SUPERBOL_TEST_SHARD] environment variable is [k/n]
    (with [0 <= k < n]); all of them otherwise.  [run.js] sets this variable
    to run shards of the suites in separate VS Code instances (see
    [SUPERBOL_TEST_JOBS]). *)
let shard weighted =
  match Node.Process.Env.get "SUPERBOL_TEST_SHARD" with
  | None | Some "" -> Ok (List.map fst weighted)
  | Some s ->
      match List.map int_of_string_opt (String.split_on_char '/' s) with
      | [ Some k; Some n ] when 0 <= k && k < n ->
          Ok (balanced_shard ~k ~n weighted)
      | _ ->
          Error (Printf.sprintf "invalid SUPERBOL_TEST_SHARD=%S (expected k/n, with \
                                 0 <= k < n)" s)

(** Entry point called by VS Code's extension test host. *)
let run () =
  match shard (selected_suites ()) with
  | Error msg ->
      Harness.log "FAILED: %s" msg;
      Promise.reject (Js_of_ocaml.Js.string msg)
  | Ok suites ->
      Promise.then_ (Harness.run_suites suites) ~fulfilled:(fun failures ->
          if failures > 0
          then Promise.reject (Js_of_ocaml.Js.string "some expected behaviors failed")
          else Promise.return ())

let () =
  (* The test host calls [run (testsRoot, callback)], and waits on the
     returned promise. *)
  Js_of_ocaml.Js.export "run"
    (Ojs.fun_to_js 1 (fun _ -> Promise.t_to_js Ojs.unit_to_js (run ())))
