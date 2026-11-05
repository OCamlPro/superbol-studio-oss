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

(** Debug configurations, with and without [launch.json], started with F5
    ([workbench.action.debug.start]) or Ctrl+F5 ([workbench.action.debug.run])
    from the active editor. *)

open Harness
open Harness.Syntax
open Project

let hello = [ "hello.cbl", marker_program () ]

let launch_json configs = Printf.sprintf {|{ "version": "0.2.0", "configurations": [ %s ] }|}
    (String.concat ", " configs)

(** The "SuperBOL: debug (launch)" snippet of [package.json]. *)
let snippet = {|{ "name": "SuperBOL: debug (launch)", "type": "superbol-gdb",
                 "request": "launch", "preLaunchTask": "SuperBOL: build module (debug)",
                 "useCobcrun": true, "target": "${file}", "arguments": "",
                 "cwd": "${workspaceFolder}", "gdbtty": true }|}

let minimal = {|{ "name": "minimal", "type": "superbol-gdb", "request": "launch" }|}

(** Opens [file] with a breakpoint on [line], and presses F5. *)
let f5_at ?(file = "hello.cbl") ?(line = display_line) ctx : stop res =
  let* () = open_file ctx file in
  add_breakpoints ctx [ bp file line ];
  let* s = f5_stop () in
  let+ () = expect_stop_at file line s in
  s

let check_output ~sub =
  check (contains ~sub (program_output ())) "program output: %S" (program_output ())

(** Checks that the program wrote [text] to [marker], waiting at most
    [timeout] for that file to be created: give a short one when the program
    has already terminated. *)
let check_marker ?(marker = "hello.out") ?(text = "HELLO") ?timeout ctx =
  with_context begin
    let* contents = file_contents ?timeout ctx marker in
    check (contains ~sub:text contents) "%s contains %S" marker contents
  end

let args_program = [ "args.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. args.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ARGS  PIC X(40).
       01 GREET PIC X(20).
       PROCEDURE DIVISION.
           ACCEPT ARGS FROM COMMAND-LINE.
           ACCEPT GREET FROM ENVIRONMENT "GREETING".
           DISPLAY "ARGS=" FUNCTION TRIM (ARGS) ";"
                   "GREETING=" FUNCTION TRIM (GREET) ";".
           STOP RUN.
|} ]

let calls = [
  "caller.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. caller.
       PROCEDURE DIVISION.
           CALL "callee".
           DISPLAY "BACK IN CALLER".
           STOP RUN.
|};
  "callee.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. callee.
       PROCEDURE DIVISION.
           DISPLAY "IN CALLEE".
           GOBACK.
|} ]

(** Builds callee.cbl with the default task, then opens caller.cbl. *)
let build_callee ctx =
  let* () = open_file ctx "callee.cbl" in
  let* r = run_task "SuperBOL: build module (debug)" in
  let* () = check_exit r in
  open_file ctx "caller.cbl"

let suite = { suite = "Debug configurations"; tests = [

  (* Without launch.json *)

  expected_with "F5 builds the active file as a module and stops at its breakpoints"
    (in_project hello) (fun ctx ->
        let* _ = f5_at ctx in
        let* r = prelaunch_task () in
        let* () = check (task_label r = "SuperBOL: build module (debug)") "%s" (describe_run r) in
        let* () = to_end "continue" in
        check_output ~sub:"HELLO");

  expected_with "F5 fills in the default debug configuration" (in_project hello) (fun ctx ->
      let* _ = f5_at ctx in
      let* config = session_config () in
      let v = config_value config in
      check (v "preLaunchTask" = {|"SuperBOL: build module (debug)"|}
             && v "useCobcrun" = "true"
             && v "target" = Printf.sprintf "%S" (path ctx "hello.cbl"))
        "preLaunchTask: %s, useCobcrun: %s, target: %s"
        (v "preLaunchTask") (v "useCobcrun") (v "target"));

  expected_with "Ctrl+F5 builds without debug information and runs the program"
    (in_project hello) (fun ctx ->
        let* () = open_file ctx "hello.cbl" in
        debug_run ();
        let* () = check_marker ctx in
        let* r = prelaunch_task () in
        check (task_label r = "SuperBOL: build module" && not (has_arg "-g" r))
          "%s" (describe_run r));

  expected_with "F5 rebuilds the program after it changed" (in_project hello) (fun ctx ->
      let* () = open_file ctx "hello.cbl" in
      let* () = f5_end () in
      let* () = check_output ~sub:"HELLO" in
      let* () = stop_debugging () |> Promise.map Result.ok in
      write_file ctx.dir ("hello.cbl", marker_program ~text:"CHANGED" ());
      let* () = f5_end () in
      check_output ~sub:"CHANGED");

  expected_with "F5 does not start debugging when the build fails"
    (in_project [ "broken.cbl", broken_program ]) (fun ctx ->
        let* () = open_file ctx "broken.cbl" in
        debug_start ();
        let* r = task_after ~before:0 "the build" in
        let* () = check_failed r in
        let* () = Promise.map (fun () -> Ok ()) (sleep 2_000) in
        check (!started_sessions = []) "a debug session started");

  expected_with "F5 debugs a program whose PROGRAM-ID differs from the file name"
    (in_project [ "main-prog.cbl", marker_program ~id:"MAINPROG" () ]) (fun ctx ->
        let* _ = f5_at ctx ~file:"main-prog.cbl" in
        ok ());

  expected_with "a program can CALL a module built beforehand" (in_project calls) (fun ctx ->
      let* () = build_callee ctx in
      let* () = f5_end () in
      check_output ~sub:"IN CALLEE");

  probe_with "breakpoints in a CALLed module are hit" (in_project calls) (fun ctx ->
      let* () = build_callee ctx in
      add_breakpoints ctx [ bp "callee.cbl" 4 ];
      let* s = f5_stop () in
      expect_stop_at "callee.cbl" 4 s);

  probe_with "F5 on a file in a sub-directory" (in_project [ "src/hello.cbl", marker_program () ])
    (fun ctx -> let* _ = f5_at ctx ~file:"src/hello.cbl" in ok ());

  probe_with "F5 on a file whose name contains a space"
    (in_project [ "my prog.cbl", marker_program ~id:"myprog" () ])
    (fun ctx -> let* _ = f5_at ctx ~file:"my prog.cbl" in ok ());

  (* With launch.json *)

  expected_with "the launch configuration snippet builds and debugs the active file"
    (in_project hello ~launch:(launch_json [ snippet ])) (fun ctx ->
        let* _ = f5_at ctx in
        ok ());

  expected_with "a launch configuration with only a type and a request gets the defaults"
    (in_project hello ~launch:(launch_json [ minimal ])) (fun ctx ->
        let* _ = f5_at ctx in
        let* r = prelaunch_task () in
        check (task_label r = "SuperBOL: build module (debug)") "%s" (describe_run r));

  expected_with "a launch configuration can build and debug an executable"
    (in_project hello ~launch:(launch_json [
         {|{ "name": "exe", "type": "superbol-gdb", "request": "launch",
             "preLaunchTask": "SuperBOL: build executable (debug)",
             "useCobcrun": false, "target": "${file}" }|} ])) (fun ctx ->
        let* _ = f5_at ctx in
        check_exists ctx (executable_file "hello"));

  expected_with "an executable is debugged without cobcrun when useCobcrun is not given"
    (in_project hello ~launch:(launch_json [
         {|{ "name": "exe", "type": "superbol-gdb", "request": "launch",
             "preLaunchTask": "SuperBOL: build executable (debug)" }|} ])) (fun ctx ->
        let* _ = f5_at ctx in
        ok ());

  expected_with "arguments are passed to the program"
    (in_project args_program ~launch:(launch_json [
         {|{ "name": "args", "type": "superbol-gdb", "request": "launch",
             "arguments": "one two" }|} ])) (fun ctx ->
        let* () = open_file ctx "args.cbl" in
        let* () = f5_end () in
        check_output ~sub:"ARGS=one two;");

  expected_with "environment variables are passed to the program"
    (in_project args_program ~launch:(launch_json [
         {|{ "name": "env", "type": "superbol-gdb", "request": "launch",
             "env": { "GREETING": "bonjour" } }|} ])) (fun ctx ->
        let* () = open_file ctx "args.cbl" in
        let* () = f5_end () in
        check_output ~sub:"GREETING=bonjour;");

  probe_with "the program runs in a configured working directory"
    (in_project (("run/README", "the program runs here\n") :: hello) ~launch:(launch_json [
         {|{ "name": "cwd", "type": "superbol-gdb", "request": "launch",
             "cwd": "${workspaceFolder}/run" }|} ])) (fun ctx ->
        let* () = open_file ctx "hello.cbl" in
        let* () = f5_end () in
        (* The session has ended: the file is there, or will never be. *)
        check_marker ctx ~marker:"run/hello.out" ~timeout:3_000.);

  expected_with "Ctrl+F5 with a launch configuration runs the program"
    (in_project hello ~launch:(launch_json [ minimal ])) (fun ctx ->
        let* () = open_file ctx "hello.cbl" in
        debug_run ();
        check_marker ctx);

  probe_with "a launch configuration with a fixed target builds that target"
    (in_project (("other.cbl", marker_program ~id:"other" ~marker:"other.out" ()) :: hello)
       ~launch:(launch_json [
           {|{ "name": "hello", "type": "superbol-gdb", "request": "launch",
               "target": "${workspaceFolder}/hello.cbl" }|} ])) (fun ctx ->
        let* () = open_file ctx "other.cbl" in
        add_breakpoints ctx [ bp "hello.cbl" display_line ];
        let* s = f5_stop () in
        expect_stop_at "hello.cbl" display_line s);

  probe_with "F5 without a build task nor a built program ends the session"
    (in_project hello ~launch:(launch_json [
         {|{ "name": "nobuild", "type": "superbol-gdb", "request": "launch",
             "preLaunchTask": "" }|} ])) (fun ctx ->
        let* () = open_file ctx "hello.cbl" in
        debug_start ();
        let* () = with_context (terminated ~timeout:15_000. ()) in
        check (contains ~sub:"hello" (console_output ()))
          "nothing reported in the debug console: %S" (console_output ()));
] }
