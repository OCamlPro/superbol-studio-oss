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

(** Tests that work in the workspace folder VS Code is opened on (given by
    the [SUPERBOL_TEST_PROJECT] environment variable), using the build tasks
    and debug configurations of the extension, with or without [tasks.json]
    and [launch.json] files, through VS Code commands. *)

open Harness
open Harness.Syntax
open Vscode_debug

(* --- Waiting ------------------------------------------------------------ *)

let within ms = Node.Date.now () +. ms

(** Default delay for builds, and debug sessions that start with a build. *)
let build_timeout = 60_000.

let rec wait_until ?(interval = 100) ~deadline ~what cond : unit res =
  if cond () then ok ()
  else if Node.Date.now () > deadline then error "timed out waiting for %s" what
  else Promise.bind (fun () -> wait_until ~interval ~deadline ~what cond) (sleep interval)

(** Fires a VS Code command without waiting for it to complete: some commands
    only complete once the user has answered a prompt. *)
let fire ?(args = []) command =
  ignore (catch (Vscode.Commands.executeCommand ~command ~args))

(* --- Recording task runs -------------------------------------------------- *)

type task_run =
  {
    task_name: string;                  (** without the source *)
    source: string;
    command: string;                    (** as given to the shell *)
    args: string list;                  (** before variable substitution *)
    mutable exit_code: int option;
    mutable ended: bool;
  }

(* Task runs, latest first. *)
let task_runs : task_run list ref = ref []

let shell_string = function
  | `String s -> s
  | `ShellQuotedString q -> Vscode.ShellQuotedString.value q

let task_started e =
  let task = TaskExecution.task (TaskStartEvent.execution e) in
  let command, args =
    match Vscode.Task.execution task with
    | Some (`ShellExecution x) ->
        (match Vscode.ShellExecution.commandLine x with
         | Some line -> line, []
         | None ->
             shell_string (Vscode.ShellExecution.command x),
             List.map shell_string (Vscode.ShellExecution.args x))
    | _ -> "", []
  in
  task_runs := { task_name = Vscode.Task.name task; source = Vscode.Task.source task;
                 command; args; exit_code = None; ended = false } :: !task_runs

let task_ended e =
  let name = Vscode.Task.name (TaskExecution.task (TaskProcessEndEvent.execution e)) in
  match List.find_opt (fun r -> r.task_name = name && not r.ended) (List.rev !task_runs) with
  | Some r -> r.exit_code <- TaskProcessEndEvent.exitCode e; r.ended <- true
  | None -> ()

let record_tasks =
  let recording = ref false in
  fun () ->
    if not !recording then begin
      recording := true;
      ignore @@ Tasks.onDidStartTask () ~listener:task_started;
      ignore @@ Tasks.onDidEndTaskProcess () ~listener:task_ended
    end

let task_label r = if r.source = "Workspace" then r.task_name else r.source ^ ": " ^ r.task_name

let describe_run r =
  Printf.sprintf "%s: %s %s (%s)" (task_label r) r.command (String.concat " " r.args)
    (match r.ended, r.exit_code with
     | false, _ -> "running"
     | true, Some c -> Printf.sprintf "exit code %d" c
     | true, None -> "no exit code")

let describe_runs () =
  match !task_runs with
  | [] -> "no task ran"
  | runs -> String.concat "; " (List.rev_map describe_run runs)

(** Waits for the end of the latest task run that started after [before]
    runs. *)
let task_after ?(timeout = build_timeout) ~before what : task_run res =
  let* () = wait_until ~deadline:(within 10_000.) ~what:(what ^ " to start")
      (fun () -> List.length !task_runs > before) in
  let run = List.hd !task_runs in
  let+ () = wait_until ~deadline:(within timeout) ~what:(what ^ " to end")
      (fun () -> run.ended) in
  run

(** Runs the task with the given label, as with "Tasks: Run Task", and waits
    for its end.  The task service may take a while to load a new
    [tasks.json] (and then asks for the task to run instead), so this retries
    a few times. *)
let run_task label : task_run res =
  let before = List.length !task_runs in
  let rec attempt n =
    fire "workbench.action.tasks.runTask" ~args:[ [%js.of: string] label ];
    let open Promise.Syntax in
    let* started = wait_until ~deadline:(within 5_000.) ~what:("task " ^ label ^ " to start")
        (fun () -> List.length !task_runs > before) in
    match started with
    | Error _ when n > 1 ->
        fire "workbench.action.closeQuickOpen";
        attempt (n - 1)
    | Error _ as e -> Promise.return e
    | Ok () -> task_after ~before ("task " ^ label)
  in
  attempt 4

(** The task run that ended before a debug session started. *)
let prelaunch_task () : task_run res =
  match !task_runs with
  | [] -> error "no build task ran"
  | r :: _ -> ok r

let has_arg a r = List.mem a r.args

let check_exit ?(code = 0) r =
  check (r.exit_code = Some code) "%s" (describe_run r)

let check_failed r =
  check (r.ended && r.exit_code <> Some 0) "expected a failure: %s" (describe_run r)

(* --- The workspace folder ---------------------------------------------- *)

type fixture =
  {
    files: (string * string) list;      (** relative paths and contents *)
    tasks: string option;               (** [.vscode/tasks.json] *)
    launch: string option;              (** [.vscode/launch.json] *)
    settings: string option;            (** [.vscode/settings.json] *)
  }

let json_parse (s : string) : Ojs.t =
  Ojs.call (Ojs.variable "JSON") "parse" [| Ojs.string_to_js s |]

let json_stringify (v : Ojs.t) : string =
  match Ojs.type_of v with
  | "undefined" -> "undefined"
  | _ -> Ojs.string_of_js (Ojs.call (Ojs.variable "JSON") "stringify" [| v |])

let string_field field v =
  let f = Ojs.get_prop_ascii v field in
  if Ojs.type_of f = "string" then Ojs.string_of_js f else ""

(** Names of the entries of [key] in configuration [section], as VS Code
    currently sees them. *)
let configured section key field =
  let config = Vscode.Workspace.getConfiguration ~section () in
  match Vscode.WorkspaceConfiguration.get config ~section:key with
  | Some v when Ojs.type_of v = "object" && not (Ojs.is_null v) ->
      List.map (string_field field) (Ojs.list_of_js (fun x -> x) v)
  | _ -> []

let written_names json key field =
  match json with
  | None -> []
  | Some s -> List.map (string_field field)
                (Ojs.list_of_js (fun x -> x) (Ojs.get_prop_ascii (json_parse s) key))

(** Setting keys and values of a [settings.json]. *)
let settings_of json =
  match json with
  | None -> []
  | Some s ->
      let o = json_parse s in
      List.map (fun k -> k, json_stringify (Ojs.get_prop_ascii o k))
        (Ojs.list_of_js Ojs.string_of_js
           (Ojs.call (Ojs.variable "Object") "keys" [| o |]))

(** Current value of setting [key], as JSON. *)
let setting key =
  match Vscode.WorkspaceConfiguration.get (Vscode.Workspace.getConfiguration ()) ~section:key with
  | Some v -> json_stringify v
  | None -> "undefined"

(** Waits until VS Code sees the given tasks, launch configurations and
    settings, and not the settings of the previous test anymore.  VS Code
    occasionally misses changes to these files, so [rewrite ()] writes them
    again whenever VS Code has not seen them for a while. *)
let configuration_applied ~previous_settings ~rewrite fixture =
  let tasks = written_names fixture.tasks "tasks" "label"
  and launch = written_names fixture.launch "configurations" "name"
  and settings = settings_of fixture.settings in
  let mismatches () =
    let names what expected actual =
      if actual = expected then []
      else [ Printf.sprintf "%s: expected [%s], got [%s]" what
               (String.concat "; " expected) (String.concat "; " actual) ] in
    names "tasks" tasks (configured "tasks" "tasks" "label")
    @ names "launch configurations" launch (configured "launch" "configurations" "name")
    @ List.filter_map (fun (k, v) ->
        let actual = setting k in
        if actual = v then None
        else Some (Printf.sprintf "%s: expected %s, got %s" k v actual)) settings
    @ List.filter_map (fun (k, v) ->
        if List.mem_assoc k settings || setting k <> v then None
        else Some (Printf.sprintf "%s: still %s, from the previous test" k v))
      previous_settings in
  let deadline = within 10_000. and rewrite_period = 3_000. in
  let rec wait ~rewrite_at =
    match mismatches () with
    | [] -> ok ()
    | m when Node.Date.now () > deadline ->
        error "timed out waiting for VS Code to load the configuration files (%s)"
          (String.concat "; " m)
    | m ->
        let rewrite_at =
          if Node.Date.now () < rewrite_at then rewrite_at
          else begin
            log "  (rewriting the configuration files: %s)" (String.concat "; " m);
            rewrite ();
            within rewrite_period
          end in
        Promise.bind (fun () -> wait ~rewrite_at) (sleep 100) in
  wait ~rewrite_at:(within rewrite_period)

let previous_settings = ref []

let write_file dir (file, contents) =
  let file = Node.Path.join [ dir; file ] in
  Node.Fs.mkdirSync (Node.Path.dirname file) ~recursive:true;
  Node.Fs.writeFileSync file contents

let rm_rf paths : unit res =
  if paths = [] then ok ()
  else
    Promise.map (fun (r : Node.ChildProcess.return) ->
        if r.exitCode = 0 then Ok () else Error ("rm failed: " ^ r.stderr))
      (Node.ChildProcess.spawn "rm" (Array.of_list ("-rf" :: paths)))

(** Brings VS Code back to a state with no open editor, no terminal, and an
    empty workspace folder. *)
let reset dir : unit res =
  List.iter TaskExecution.terminate (Tasks.taskExecutions ());
  fire "workbench.action.closeQuickOpen";
  fire "notifications.clearAll";
  let* () = catch (Vscode.Commands.executeCommand
                     ~command:"workbench.action.closeAllEditors" ~args:[])
            |> Promise.map (Result.map ignore) in
  fire "workbench.action.terminal.killAll";
  let* entries = Promise.map (Result.map_error (fun e -> "cannot list " ^ dir ^ ": " ^ e))
      (Node.Fs.readDir dir) in
  let+ () = rm_rf (List.map (fun e -> Node.Path.join [ dir; e ]) entries) in
  task_runs := []

(** Sets up the workspace folder with the given files. *)
let in_project ?tasks ?launch ?settings files ~workdir:_ : ctx res =
  match Node.Process.Env.get "SUPERBOL_TEST_PROJECT" with
  | None -> error "SUPERBOL_TEST_PROJECT is not set"
  | Some dir ->
      record_tasks ();
      let fixture = { files; tasks; launch; settings } in
      let* () = reset dir in
      List.iter (write_file dir) files;
      let config file = Option.map (fun s -> ".vscode/" ^ file, s) in
      let write_config () =
        List.iter (write_file dir) @@ List.filter_map Fun.id
          [ config "tasks.json" tasks; config "launch.json" launch;
            config "settings.json" settings ] in
      write_config ();
      let* () = configuration_applied fixture ~rewrite:write_config
          ~previous_settings:!previous_settings in
      previous_settings := settings_of settings;
      ok { dir }

let exists ctx file = Node.Fs.existsSync (path ctx file)

let module_file name =
  match Node.Process.platform with
  | "win32" -> name ^ ".dll"
  | "darwin" -> name ^ ".dylib"
  | _ -> name ^ ".so"

let executable_file name =
  if Node.Process.platform = "win32" then name ^ ".exe" else name

let check_exists ctx file =
  check (exists ctx file) "%s was not created (%s)" file (describe_runs ())

let read_file ctx file = Promise.map (fun s -> Ok s) (Node.Fs.readFile (path ctx file))

(** Waits for [file] to exist, and returns its contents. *)
let file_contents ?(timeout = build_timeout) ctx file : string res =
  let* () = wait_until ~deadline:(within timeout) ~what:(file ^ " to be created")
      (fun () -> exists ctx file) in
  (* Let the program finish writing. *)
  let* () = Promise.map (fun () -> Ok ()) (sleep 300) in
  read_file ctx file

(** Opens [file] in an editor, which becomes the active one. *)
let open_file ctx file : unit res =
  let* doc = catch (Vscode.Workspace.openTextDocument (`Filename (path ctx file))) in
  let+ _ = catch (Vscode.Window.showTextDocument ~document:(`TextDocument doc) ()) in
  ()

(** Problems reported by the GnuCOBOL problem matchers on [file], as line
    numbers and messages. *)
let cobc_problems ctx file =
  List.filter_map (fun d ->
      if Vscode.Diagnostic.source d = Some "GnuCOBOL" then
        Some (Vscode.Position.line (Vscode.Range.start (Vscode.Diagnostic.range d)) + 1,
              Vscode.Diagnostic.message d)
      else None)
    (Vscode.Languages.getDiagnostics (Vscode.Uri.file (path ctx file)))

(** Path to [command] in the [PATH]. *)
let which command : string res =
  Promise.map (fun (r : Node.ChildProcess.return) ->
      if r.exitCode = 0 then Ok (String.trim r.stdout)
      else Error (command ^ " not found"))
    (Node.ChildProcess.spawn "sh" [| "-c"; "command -v " ^ command |])

(* --- Debugging with F5 and Ctrl+F5 ------------------------------------------ *)

(** Starts debugging, as with F5. *)
let debug_start () = fire "workbench.action.debug.start"

(** Starts without debugging, as with Ctrl+F5. *)
let debug_run () = fire "workbench.action.debug.run"

let describe_sessions () =
  match !started_sessions with
  | [] -> "no debug session started"
  | sessions ->
      String.concat "; " (List.rev_map (fun s ->
          "started " ^ json_stringify
            (Vscode.DebugConfiguration.t_to_js (Vscode.DebugSession.configuration s)))
          sessions)

(** Terminals, with their exit codes if they have exited. *)
let describe_terminals () =
  match Vscode.Window.terminals () with
  | [] -> "no terminal"
  | terminals ->
      String.concat "; " (List.map (fun t ->
          Printf.sprintf "terminal %S%s" (Vscode.Terminal.name t)
            (match Vscode.Terminal.exitStatus t with
             | None -> ""
             | Some s -> Printf.sprintf " (exit code %d)" (Vscode.TerminalExitStatus.code s)))
          terminals)

(** Adds what happened to errors of [r]. *)
let with_context r : _ res =
  Promise.map (Result.map_error (fun msg ->
      let console = console_output () in
      let n = String.length console in
      Printf.sprintf "%s\n               [%s]\n               [%s]\n               [%s]\n               [console: %S]"
        msg (describe_runs ()) (describe_sessions ()) (describe_terminals ())
        (if n > 400 then String.sub console (n - 400) 400 else console))) r

(** F5, and waits for the first stop. *)
let f5_stop () : stop res =
  debug_start ();
  with_context (next_stop ~timeout:build_timeout ())

(** F5, and waits for the end of the session. *)
let f5_end () : unit res =
  debug_start ();
  with_context (terminated ~timeout:build_timeout ())

(** Configuration of the latest debug session, after resolution. *)
let session_config () : Vscode.DebugConfiguration.t res =
  match !started_sessions with
  | s :: _ -> ok (Vscode.DebugSession.configuration s)
  | [] -> error "no debug session started"

let config_value config key = json_stringify (Vscode.DebugConfiguration.get_property config key)

let expect_stop_at file line stop =
  check (file_of stop = Some (Node.Path.basename file) && line_of stop = line)
    "expected a stop at %s:%d, got %s:%d (%s)" file line
    (Option.value (file_of stop) ~default:"?") (line_of stop) (describe_runs ())

(* --- Programs -------------------------------------------------------------- *)

(** A program that writes [text] into [marker] (in its working directory),
    then displays [text] (line 17). *)
let marker_program ?(id = "hello") ?(marker = "hello.out") ?(text = "HELLO") () =
  Printf.sprintf {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. %s.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MARK ASSIGN TO "%s"
               ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD MARK.
       01 MARK-REC PIC X(20).
       PROCEDURE DIVISION.
           OPEN OUTPUT MARK.
           MOVE "%s" TO MARK-REC.
           WRITE MARK-REC.
           CLOSE MARK.
           DISPLAY "%s".
           STOP RUN.
|} id marker text text

(** Line of the [DISPLAY] statement in [marker_program]. *)
let display_line = 17

(** A program with a syntax error on line 5. *)
let broken_program = {|       IDENTIFICATION DIVISION.
       PROGRAM-ID. broken.
       PROCEDURE DIVISION.
           DISPLAY "A".
           MOVE TO.
           STOP RUN.
|}
