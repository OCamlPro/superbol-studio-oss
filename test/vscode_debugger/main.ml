(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
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
    in the [PATH], and are skipped otherwise.  They work in the directory
    given by the [SUPERBOL_TEST_WORKDIR] environment variable.

    Tests are either [Expected] behaviors, whose failures fail the run, or
    [Probe]s for suspected bugs, whose failures are only reported (see
    {!Harness.kind}). *)

let suites = [
  T_stepping.suite;
  T_breakpoints.suite;
  T_variables.suite;
  T_evaluate.suite;
  T_calls.suite;
  T_lifecycle.suite;
]

(** Entry point called by VS Code's extension test host. *)
let run () =
  Promise.then_ (Harness.run_suites suites) ~fulfilled:(fun failures ->
      if failures > 0
      then Promise.reject (Js_of_ocaml.Js.string "some expected behaviors failed")
      else Promise.return ())

let () =
  (* The test host calls [run (testsRoot, callback)], and waits on the
     returned promise. *)
  Js_of_ocaml.Js.export "run"
    (Ojs.fun_to_js 1 (fun _ -> Promise.t_to_js Ojs.unit_to_js (run ())))
