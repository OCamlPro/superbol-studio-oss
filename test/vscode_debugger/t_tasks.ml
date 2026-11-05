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

(** SuperBOL build tasks, as provided by default and as defined in
    [tasks.json], run on the active editor through "Tasks: Run Task". *)

open Harness
open Harness.Syntax
open Project

let hello = [ "hello.cbl", marker_program () ]

let module_debug = "SuperBOL: build module (debug)"
let module_release = "SuperBOL: build module"
let executable_debug = "SuperBOL: build executable (debug)"

(** Runs [label] on [file], once opened. *)
let build ?(file = "hello.cbl") ctx label =
  let* () = open_file ctx file in
  run_task label

(** Like [build], but builds again until [ok r] holds for the task run [r],
    for a few seconds: the LSP server only takes a new [superbol.toml] into
    account once VS Code has notified it of the change, which may happen after
    the build has started. *)
let build_until ?file ?(timeout = 15_000.) ctx label ok : task_run res =
  let deadline = within timeout in
  let rec attempt () =
    let* r = build ?file ctx label in
    if ok r || Node.Date.now () > deadline then Promise.return (Ok r)
    else Promise.bind attempt (sleep 1_000)
  in
  attempt ()

let check_args r ~present ~absent =
  let missing = List.filter (fun a -> not (has_arg a r)) present
  and unwanted = List.filter (fun a -> has_arg a r) absent in
  check (missing = [] && unwanted = [])
    "missing arguments [%s], unwanted arguments [%s] in %s"
    (String.concat " " missing) (String.concat " " unwanted) (describe_run r)

let tasks_json tasks = Printf.sprintf {|{ "version": "2.0.0", "tasks": [ %s ] }|}
    (String.concat ", " tasks)

let usecopy = [
  "usecopy.cbl", {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. usecopy.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "defs".
       PROCEDURE DIVISION.
           DISPLAY GREETING.
           STOP RUN.
|};
  "copy/defs.cpy", "       01 GREETING PIC X(5) VALUE \"HELLO\".\n";
]

let suite = { suite = "Build tasks"; tests = [

  expected_with "the four default build tasks are offered" (in_project hello) (fun ctx ->
      let* () = open_file ctx "hello.cbl" in
      let* tasks = catch (Vscode.Tasks.fetchTasks ()) in
      let superbol = List.filter (fun t -> Vscode.Task.source t = "SuperBOL") tasks in
      let names = List.sort compare (List.map Vscode.Task.name superbol) in
      let* () = check (names = [ "build executable"; "build executable (debug)";
                                 "build module"; "build module (debug)" ])
          "SuperBOL tasks: [%s]" (String.concat "; " names) in
      check (List.for_all (fun t -> Vscode.Task.group t <> None) superbol)
        "some SuperBOL tasks are not in the build group");

  expected_with "build module (debug) builds a module of the active file with debug information"
    (in_project hello) (fun ctx ->
        let* r = build ctx module_debug in
        let* () = check_exit r in
        let* () = check_args r ~present:[ "-m"; "-g" ] ~absent:[ "-x" ] in
        check_exists ctx (module_file "hello"));

  expected_with "build module builds a module without debug information" (in_project hello)
    (fun ctx ->
       let* r = build ctx module_release in
       let* () = check_exit r in
       let* () = check_args r ~present:[ "-m" ] ~absent:[ "-g"; "-x" ] in
       check_exists ctx (module_file "hello"));

  expected_with "build executable (debug) builds an executable" (in_project hello) (fun ctx ->
      let* r = build ctx executable_debug in
      let* () = check_exit r in
      let* () = check_args r ~present:[ "-x"; "-g" ] ~absent:[ "-m" ] in
      check_exists ctx (executable_file "hello"));

  expected_with "compilation errors are reported as problems on the source file"
    (in_project [ "broken.cbl", broken_program ]) (fun ctx ->
        let* r = build ctx module_debug ~file:"broken.cbl" in
        let* () = check_failed r in
        let* () = wait_until ~deadline:(within 5_000.) ~what:"problems on broken.cbl"
            (fun () -> cobc_problems ctx "broken.cbl" <> []) in
        let problems = cobc_problems ctx "broken.cbl" in
        check (List.exists (fun (line, _) -> line = 5) problems) "problems: %s"
          (String.concat "; " (List.map (fun (l, m) -> Printf.sprintf "%d: %s" l m) problems)));

  expected_with "a build task without an active editor does not build anything"
    (in_project hello) (fun ctx ->
        fire "workbench.action.tasks.runTask" ~args:[ [%js.of: string] module_debug ];
        let* () = Promise.map (fun () -> Ok ()) (sleep 3_000) in
        check (not (exists ctx (module_file "hello"))
               && List.for_all (fun r -> r.exit_code <> Some 0) !task_runs)
          "something was built: %s" (describe_runs ()));

  expected_with "copybook directories from superbol.toml are passed to cobc"
    (in_project (("superbol.toml", "[cobol]\ncopybooks = [ { dir = \"copy\" } ]\n")
                 :: usecopy)) (fun ctx ->
        let* r = build_until ctx module_debug ~file:"usecopy.cbl"
            (fun r -> r.exit_code = Some 0) in
        let* () = check_exit r in
        check_exists ctx (module_file "usecopy"));

  expected_with "the dialect from superbol.toml is passed to cobc"
    (in_project (("superbol.toml", "[cobol]\ndialect = \"ibm\"\n") :: hello)) (fun ctx ->
        let* r = build_until ctx module_debug (has_arg "-std=ibm") in
        let* () = check_exit r in
        check_args r ~present:[ "-std=ibm" ] ~absent:[]);

  expected_with "the superbol.cobcPath setting selects the compiler"
    (fun ~workdir ->
       let* cobc = which "cobc" in
       in_project hello ~workdir
         ~settings:(Printf.sprintf {|{ "superbol.cobcPath": %S }|} cobc)) (fun ctx ->
        let* cobc = which "cobc" in
        let* r = build ctx module_debug in
        let* () = check_exit r in
        check (r.command = cobc) "expected %s, got %s" cobc (describe_run r));

  expected_with "a tasks.json task of type superbol builds a module by default"
    (in_project hello
       ~tasks:(tasks_json [ {|{ "type": "superbol", "label": "build hello as a module", "forDebug": true }|} ]))
    (fun ctx ->
       let* r = build ctx "build hello as a module" in
       let* () = check_exit r in
       let* () = check_args r ~present:[ "-m"; "-g" ] ~absent:[ "-x" ] in
       check_exists ctx (module_file "hello"));

  expected_with "a tasks.json task can build an executable"
    (in_project hello
       ~tasks:(tasks_json [ {|{ "type": "superbol", "label": "build hello as an executable",
                                "forDebug": true, "executable": true }|} ]))
    (fun ctx ->
       let* r = build ctx "build hello as an executable" in
       let* () = check_exit r in
       check_exists ctx (executable_file "hello"));

  expected_with "extraArgs of a tasks.json task are passed to cobc"
    (in_project hello
       ~tasks:(tasks_json [ {|{ "type": "superbol", "label": "build hello with extra arguments",
                                "extraArgs": [ "-debug" ] }|} ]))
    (fun ctx ->
       let* r = build ctx "build hello with extra arguments" in
       let* () = check_exit r in
       check_args r ~present:[ "-debug" ] ~absent:[]);

  expected_with "a tasks.json task with a missing compiler fails"
    (in_project hello
       ~tasks:(tasks_json [ {|{ "type": "superbol", "label": "build hello with a missing compiler",
                                "cobcPath": "/nonexistent/cobc" }|} ]))
    (fun ctx ->
       let* r = build ctx "build hello with a missing compiler" in
       check_failed r);

  probe_with "listingsTarget of a tasks.json task generates listings in that directory"
    (in_project (("listings/README", "listings go here\n") :: hello)
       ~tasks:(tasks_json [ {|{ "type": "superbol", "label": "build hello with listings",
                                "listingsTarget": "listings" }|} ]))
    (fun ctx ->
       let* r = build ctx "build hello with listings" in
       let* () = check_exit r in
       let* entries = Promise.map (Result.map_error Fun.id)
           (Node.Fs.readDir (path ctx "listings")) in
       check (List.exists (fun e -> e <> "README") entries)
         "no listing in listings/ (%s)" (describe_run r));

  probe_with "customizing a default build task in tasks.json applies to that task"
    (in_project hello
       ~tasks:(tasks_json [ {|{ "type": "superbol", "label": "SuperBOL: build module (debug)",
                                "forDebug": true, "executable": false,
                                "extraArgs": [ "-debug" ],
                                "problemMatcher": [ "$gnucobol" ], "group": "build" }|} ]))
    (fun ctx ->
       let* r = build ctx module_debug in
       let* () = check_exit r in
       let* () = check_args r ~present:[ "-debug" ] ~absent:[] in
       let* tasks = catch (Vscode.Tasks.fetchTasks ()) in
       let same = List.filter (fun t ->
           Vscode.Task.name t = "build module (debug)"
           || Vscode.Task.name t = module_debug) tasks in
       check (List.length same = 1) "tasks named %s: %s" module_debug
         (String.concat ", " (List.map (fun t ->
              Printf.sprintf "%s/%s" (Vscode.Task.source t) (Vscode.Task.name t)) same)));

  probe_with "building a file whose name contains a space"
    (in_project [ "my prog.cbl", marker_program ~id:"myprog" () ]) (fun ctx ->
        let* r = build ctx module_debug ~file:"my prog.cbl" in
        let* () = check_exit r in
        check_exists ctx (module_file "my prog"));
] }
