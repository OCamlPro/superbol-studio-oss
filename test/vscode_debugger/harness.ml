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

(** Test harness driving debugging sessions of the bundled extension through
    VS Code commands, and observing them through a debug adapter tracker. *)

open Vscode_debug

let debug_type = "superbol-gdb"
let extension_id = "OCamlPro.SuperBOL"
let event_timeout_ms = 10_000.

let log fmt = Printf.ksprintf Node.Console.log fmt

let sleep ms = Promise.make (fun ~resolve ~reject:_ -> ignore (Node.setTimeout resolve ms))

(* --- Results ------------------------------------------------------------ *)

(** Outcome of a test step: failures are values, not exceptions. *)
type 'a res = ('a, string) result Promise.t

module Syntax = Promise.Result.Syntax

let ok x : _ res = Promise.return (Ok x)
let error fmt = Printf.ksprintf (fun s -> Promise.return (Error s)) fmt

(** [check cond "..."] fails with the given message unless [cond]. *)
let check cond fmt =
  Printf.ksprintf (fun s -> Promise.return (if cond then Ok () else Error s)) fmt

let expect_int ~what expected actual =
  check (actual = expected) "%s: expected %d, got %d" what expected actual

let expect_string ~what expected actual =
  check (actual = expected) "%s: expected %S, got %S" what expected actual

(** Turns rejections of [p] (from VS Code) into [Error]s. *)
let catch p =
  Promise.map (Result.map_error Node.JsError.message) (Promise.Result.from_catch p)

(** Digits of [s] as a number, ignoring any other character (quotes,
    padding, etc.). *)
let digits s =
  let b = Buffer.create 8 in
  String.iter (function '0'..'9' as c -> Buffer.add_char b c | _ -> ()) s;
  int_of_string_opt (Buffer.contents b)

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = sub || at (i + 1)) in
  at 0

(* --- Programs under test ------------------------------------------------ *)

type program =
  {
    name: string;                       (** also the build directory *)
    sources: (string * string) list;    (** file names and contents *)
    cobc_args: string list;             (** before the source file names *)
  }

(** A program made of the given sources, built as an executable with debug
    information. *)
(** Arguments of [cobc] for programs to debug: like those of SuperBOL's debug
    build tasks, they disable C optimizations that [cobc] may enable by
    default, and that break debugging. *)
let debug_args = [ "-g"; "-A"; "-O0"; "-A"; "-U_FORTIFY_SOURCE" ]

let program ?(cobc_args = "-x" :: debug_args) name sources = { name; sources; cobc_args }

(** Where a test finds the program it debugs. *)
type ctx = { dir: string }

let path ctx file = Node.Path.join [ ctx.dir; file ]

let built : (string, (ctx, string) result) Hashtbl.t = Hashtbl.create 7

(** Builds [p] once, in a sub-directory of [workdir]. *)
let build p ~workdir : ctx res =
  match Hashtbl.find_opt built p.name with
  | Some r -> Promise.return r
  | None ->
      let dir = Node.Path.join [ workdir; p.name ] in
      Node.Fs.mkdirSync dir ~recursive:true;
      List.iter (fun (file, text) -> Node.Fs.writeFileSync (Node.Path.join [ dir; file ]) text)
        p.sources;
      let args = Array.of_list (p.cobc_args @ List.map fst p.sources) in
      Node.ChildProcess.spawn "cobc" args
        ~options:(Node.ChildProcess.Options.create ~cwd:dir ())
      |> Promise.map (fun (r : Node.ChildProcess.return) ->
          let res =
            if r.exitCode = 0 then Ok { dir }
            else Error (Printf.sprintf "cobc failed on %s: %s" p.name r.stderr) in
          Hashtbl.replace built p.name res;
          res)

(* --- Recording debug sessions ------------------------------------------ *)

type frame =
  {
    frame_id: int;
    frame_name: string;
    line: int;
    file: string option;                (** base name *)
  }

type stop =
  {
    reason: string;
    thread_id: int;
    frames: frame list;                 (** at least the top one *)
  }

(** Line of the top frame. *)
let line_of stop = match stop.frames with f :: _ -> f.line | [] -> 0
let file_of stop = match stop.frames with f :: _ -> f.file | [] -> None

type event =
  | Stopped of stop
  | Terminated

let events : event Queue.t = Queue.create ()
let wake_up : (unit -> unit) option ref = ref None

let push e =
  Queue.push e events;
  Option.iter (fun f -> f ()) !wake_up

(** What the program printed on its standard output, and what the debug
    console showed otherwise. *)
let stdout_buf = Buffer.create 256
let console_buf = Buffer.create 256
let program_output () = Buffer.contents stdout_buf
let console_output () = Buffer.contents console_buf

(* A stop is only recorded once VS Code has fetched the corresponding stack
   trace, as debug commands act on the thread VS Code focuses at that
   point. *)
let pending_stop : (string * int) option ref = ref None

(* Number of breakpoint updates the adapter has answered. *)
let breakpoint_updates = ref 0

let reason_name : DebugProtocol.StoppedEvent.reason -> string = function
  | Step -> "step"
  | Breakpoint -> "breakpoint"
  | Exception -> "exception"
  | Pause -> "pause"
  | Entry -> "entry"
  | Goto -> "goto"
  | FunctionBreakpoint -> "function breakpoint"
  | DataBreakpoint -> "data breakpoint"
  | InstructionBreakpoint -> "instruction breakpoint"
  | Other s -> s

let frame_of f =
  let open DebugProtocol in
  { frame_id = StackFrame.id f; frame_name = StackFrame.name f; line = StackFrame.line f;
    file = Option.bind (StackFrame.source f) Source.path |> Option.map Node.Path.basename }

let on_adapter_message message =
  let open DebugProtocol in
  let js = Vscode.DebugProtocolMessage.t_to_js message in
  let message = ProtocolMessage.t_of_js js in
  match ProtocolMessage.type_ message with
  | ProtocolMessage.Event ->
      (match Event.event (Event.t_of_js js) with
       | "stopped" ->
           let body = StoppedEvent.body (StoppedEvent.t_of_js js) in
           pending_stop := Some (reason_name body.reason,
                                 Option.value body.threadId ~default:1)
       | "output" ->
           let body = OutputEvent.body (OutputEvent.t_of_js js) in
           (match body.category with
            | Some Stdout -> Buffer.add_string stdout_buf body.output
            | _ -> Buffer.add_string console_buf body.output)
       | _ -> ())
  | ProtocolMessage.Response ->
      let response = Response.t_of_js js in
      (match Response.command response with
       | "setBreakpoints" | "setFunctionBreakpoints" -> incr breakpoint_updates
       | _ -> ());
      (match !pending_stop with
       | Some (reason, thread_id)
         when Response.command response = "stackTrace" && Response.success response ->
           pending_stop := None;
           let body = StackTraceResponse.body (StackTraceResponse.t_of_js js) in
           push (Stopped { reason; thread_id;
                           frames = List.map frame_of body.stackFrames })
       | _ -> ())
  | _ -> ()

(* Debug sessions started since the last [stop_debugging], latest first. *)
let started_sessions : Vscode.DebugSession.t list ref = ref []

let record_sessions () =
  let tracker _session = DebugAdapterTracker.create ~onDidSendMessage:on_adapter_message () in
  ignore @@ Debug.registerDebugAdapterTrackerFactory ~debugType:debug_type
    ~factory:(DebugAdapterTrackerFactory.create ~createDebugAdapterTracker:tracker ());
  ignore @@ Debug.onDidTerminateDebugSession () ~listener:(fun session ->
      if Vscode.DebugSession.type_ session = debug_type then push Terminated);
  ignore @@ Debug.onDidStartDebugSession () ~listener:(fun session ->
      if Vscode.DebugSession.type_ session = debug_type then
        started_sessions := session :: !started_sessions)

let rec next_event ~deadline : event res =
  match Queue.take_opt events with
  | Some e -> ok e
  | None ->
      let remaining = deadline -. Node.Date.now () in
      if remaining <= 0. then error "timed out waiting for the debug session"
      else
        Promise.bind (fun () -> next_event ~deadline) @@
        Promise.make (fun ~resolve ~reject:_ ->
            let timer =
              Node.setTimeout (fun () -> wake_up := None; resolve ())
                (int_of_float remaining) in
            wake_up := Some (fun () -> wake_up := None; Node.clearTimeout timer; resolve ()))

let describe = function
  | Terminated -> "the program terminated"
  | Stopped s -> Printf.sprintf "the program stopped (%s) at line %d" s.reason (line_of s)

(** Waits for the next stop. *)
let next_stop ?(timeout = event_timeout_ms) () : stop res =
  let open Syntax in
  let* e = next_event ~deadline:(Node.Date.now () +. timeout) in
  match e with
  | Terminated -> error "the program terminated instead of stopping"
  | Stopped stop ->
      (* Let VS Code focus the stopped thread before any further command. *)
      Promise.map (fun () -> Ok stop) (sleep 100)

(** Waits for the end of the session. *)
let terminated ?(timeout = event_timeout_ms) () : unit res =
  let open Syntax in
  let* e = next_event ~deadline:(Node.Date.now () +. timeout) in
  match e with
  | Terminated -> ok ()
  | Stopped _ as e -> error "expected the program to terminate, but %s" (describe e)

(* --- Driving debug sessions -------------------------------------------- *)

type breakpoint =
  | Line of { file: string; line: int; enabled: bool;
              condition: string option; hit: string option }
  | Function of string

let bp ?(enabled = true) ?condition ?hit file line =
  Line { file; line; enabled; condition; hit }

let add_breakpoints ctx bps =
  let make = function
    | Line { file; line; enabled; condition; hit } ->
        let position = Vscode.Position.make ~line:(line - 1) ~character:0 in
        let location = Vscode.Location.make ~uri:(Vscode.Uri.file (path ctx file))
            ~rangeOrPosition:(`Position position) in
        (SourceBreakpoint.make () ~location ~enabled ?condition ?hitCondition:hit
         :> Vscode.Breakpoint.t)
    | Function name ->
        (FunctionBreakpoint.make () ~functionName:name :> Vscode.Breakpoint.t)
  in
  if bps <> [] then Debug.addBreakpoints (List.map make bps)

let remove_all_breakpoints () = Debug.removeBreakpoints (Debug.breakpoints ())

(** Waits until the adapter has answered at least one breakpoint update sent
    after [before] updates, and VS Code has not sent any other update for a
    while (it may send several for a single change). *)
let rec breakpoints_updated ~before ~deadline : unit res =
  if Node.Date.now () > deadline then error "breakpoints were not updated"
  else if !breakpoint_updates <= before then
    Promise.bind (fun () -> breakpoints_updated ~before ~deadline) (sleep 50)
  else
    let seen = !breakpoint_updates in
    Promise.bind (fun () ->
        if !breakpoint_updates = seen then ok ()
        else breakpoints_updated ~before:seen ~deadline) (sleep 300)

(** Changes breakpoints during a session, and waits until the adapter has
    taken the change into account. *)
let update_breakpoints f : unit res =
  let before = !breakpoint_updates in
  f ();
  breakpoints_updated ~before ~deadline:(Node.Date.now () +. 3000.)

(** Starts debugging [target] (a source file in [ctx]), with the given
    breakpoints. *)
let launch ?(group = []) ?(bps = []) ctx target : unit res =
  let open Syntax in
  add_breakpoints ctx bps;
  let config =
    Vscode.DebugConfiguration.create () ~type_:debug_type ~request:"launch"
      ~name:"debugger test"
      ~properties:[ "target", [%js.of: string] (path ctx target);
                    "cwd", [%js.of: string] ctx.dir;
                    "group", [%js.of: string list] group;
                    "preLaunchTask", [%js.of: string] "" ] in
  let* started = catch (Debug.startDebugging None config) in
  check started "could not start debugging"

(** Like [launch], and waits for the first stop. *)
let start ?group ?bps ctx target : stop res =
  let open Syntax in
  let* () = launch ?group ?bps ctx target in
  next_stop ()

let command name =
  catch (Vscode.Commands.executeCommand ~args:[] ~command:("workbench.action.debug." ^ name))
  |> Promise.map (Result.map ignore)

let then_stop p = let open Syntax in let* () = p in next_stop ()

let step_over () = then_stop (command "stepOver")
let step_into () = then_stop (command "stepInto")
let step_out () = then_stop (command "stepOut")
let continue_ () = then_stop (command "continue")
let pause () = then_stop (command "pause")

(** Runs a command that should end the session. *)
let to_end name = let open Syntax in let* () = command name in terminated ()

(** Waits for the session to end after [stop_debugging] or termination. *)
let rec session_gone ~deadline =
  match Debug.activeDebugSession () with
  | None -> Promise.return ()
  | Some _ when Node.Date.now () > deadline -> Promise.return ()
  | Some _ -> Promise.bind (fun () -> session_gone ~deadline) (sleep 100)

let stop_debugging () =
  let open Promise.Syntax in
  let* () = match Debug.activeDebugSession () with
    | None -> Promise.return ()
    | Some _ -> Promise.map ignore (Promise.Result.from_catch (Debug.stopDebugging ())) in
  let* () = session_gone ~deadline:(Node.Date.now () +. 5000.) in
  remove_all_breakpoints ();
  Queue.clear events;
  pending_stop := None;
  started_sessions := [];
  Buffer.clear stdout_buf;
  Buffer.clear console_buf;
  Promise.return ()

(* --- DAP requests --------------------------------------------------------- *)

let request command args decode : _ res =
  match Debug.activeDebugSession () with
  | None -> error "no active debug session"
  | Some session ->
      catch (DebugSession.customRequest session command args)
      |> Promise.map (Result.map decode)

(** Whole call stack at [stop]. *)
let stack_trace stop : frame list res =
  request "stackTrace" (DebugSession.Args.stackTrace ~threadId:stop.thread_id ())
    (fun body ->
       List.map frame_of
         (DebugProtocol.StackTraceResponse.M.body_of_js body).stackFrames)

type variable =
  {
    var_name: string;
    value: string;
    var_type: string option;
    reference: int;                     (** to expand it, if non-zero *)
  }

let variable_of v =
  let open DebugProtocol in
  { var_name = Variable.name v; value = Variable.value v; var_type = Variable.type_ v;
    reference = Variable.variablesReference v }

let variables reference : variable list res =
  request "variables" (DebugSession.Args.variables ~variablesReference:reference ())
    (fun body ->
       List.map variable_of (DebugProtocol.VariablesResponse.M.body_of_js body).variables)

(** Scopes of the top frame at [stop], as names and references. *)
let scopes stop : (string * int) list res =
  match stop.frames with
  | [] -> error "no frame"
  | f :: _ ->
      request "scopes" (DebugSession.Args.scopes ~frameId:f.frame_id ())
        (fun body ->
           List.map (fun s -> DebugProtocol.Scope.(name s, variablesReference s))
             (DebugProtocol.ScopesResponse.M.body_of_js body).scopes)

(** Variables of the [Local] scope of the top frame at [stop], with the
    reference of that scope. *)
let locals stop : (int * variable list) res =
  let open Syntax in
  let* scopes = scopes stop in
  match List.assoc_opt "Local" scopes with
  | None -> error "no Local scope"
  | Some reference -> let+ vars = variables reference in reference, vars

let find_variable name vars : variable res =
  match List.find_opt (fun v -> v.var_name = name) vars with
  | Some v -> ok v
  | None ->
      error "no variable %s (have: %s)" name
        (String.concat ", " (List.map (fun v -> v.var_name) vars))

(** Value of the local variable [name] at [stop]. *)
let local stop name : variable res =
  let open Syntax in
  let* _, vars = locals stop in
  find_variable name vars

let evaluate ?(context = "watch") stop expression : string res =
  match stop.frames with
  | [] -> error "no frame"
  | f :: _ ->
      request "evaluate"
        (DebugSession.Args.evaluate ~expression ~frameId:f.frame_id ~context ())
        (fun body -> (DebugProtocol.EvaluateResponse.M.body_of_js body).result)

let set_variable ~reference name value : string res =
  request "setVariable"
    (DebugSession.Args.setVariable ~variablesReference:reference ~name ~value ())
    (fun body -> (DebugProtocol.SetVariableResponse.M.body_of_js body).value)

(* --- Tests and their classification ------------------------------------- *)

(** [Expected] tests check behaviors a COBOL debugger should have, and that
    the extension is believed to have: failures are regressions.  [Probe]
    tests check behaviors that the extension is suspected to get wrong:
    failures point at likely bugs, and do not fail the run. *)
type kind = Expected | Probe

type test =
  {
    title: string;
    kind: kind;
    prepare: workdir:string -> ctx res;   (** sets up what [body] works on *)
    body: ctx -> unit res;
  }

(** Tests that debug the given program, built beforehand. *)
let expected title prog body = { title; kind = Expected; prepare = build prog; body }
let probe title prog body = { title; kind = Probe; prepare = build prog; body }

(** Tests that work on what [prepare] sets up. *)
let expected_with title prepare body = { title; kind = Expected; prepare; body }
let probe_with title prepare body = { title; kind = Probe; prepare; body }

type suite = { suite: string; tests: test list }

type outcome = { test: test; suite_name: string; result: (unit, string) result }

let run_test ~workdir ~suite_name test : outcome Promise.t =
  let open Promise.Syntax in
  let* result =
    let open Syntax in
    let* ctx = test.prepare ~workdir in
    test.body ctx in
  let* () = stop_debugging () in
  let label = match test.kind, result with
    | Expected, Ok () -> "ok      "
    | Expected, Error _ -> "FAIL    "
    | Probe, Ok () -> "no bug  "
    | Probe, Error _ -> "BUG?    " in
  log "  %s%s%s" label test.title
    (match result with Ok () -> "" | Error msg -> "\n            -> " ^ msg);
  Promise.return { test; suite_name; result }

let summary outcomes =
  let count kind ok =
    List.length (List.filter (fun o ->
        o.test.kind = kind && Result.is_ok o.result = ok) outcomes) in
  log "";
  log "Expected behaviors: %d ok, %d FAILED" (count Expected true) (count Expected false);
  log "Probes for suspected bugs: %d BUG?, %d no bug" (count Probe false) (count Probe true);
  let bugs = List.filter (fun o -> o.test.kind = Probe && Result.is_error o.result) outcomes in
  if bugs <> [] then begin
    log "";
    log "Suspected bugs:";
    List.iter (fun o -> log "  - [%s] %s" o.suite_name o.test.title) bugs
  end;
  count Expected false

let available command =
  Promise.map (fun r -> r.Node.ChildProcess.exitCode = 0)
    (Node.ChildProcess.spawn command [| "--version" |])

let activate_extension () =
  match Vscode.Extensions.getExtension extension_id with
  | None -> error "extension not found: %s" extension_id
  | Some extension -> catch (Extension.activate extension)

(** Runs all suites, and returns the number of failed [Expected] tests. *)
let run_suites suites : int Promise.t =
  let open Promise.Syntax in
  let* have_cobc = available "cobc" in
  let* have_gdb = available "gdb" in
  match Node.Process.Env.get "SUPERBOL_TEST_WORKDIR" with
  | _ when not (have_cobc && have_gdb) ->
      log "SKIPPED: `cobc` or `gdb` is not available";
      Promise.return 0
  | None ->
      log "FAILED: SUPERBOL_TEST_WORKDIR is not set";
      Promise.return 1
  | Some workdir ->
      let* activated = activate_extension () in
      match activated with
      | Error msg ->
          log "FAILED: could not activate %s: %s" extension_id msg;
          Promise.return 1
      | Ok () ->
          record_sessions ();
          let rec loop acc = function
            | [] -> Promise.return (List.rev acc)
            | (suite_name, test) :: rest ->
                let* o = run_test ~workdir ~suite_name test in
                loop (o :: acc) rest in
          let rec suites_loop acc = function
            | [] -> Promise.return acc
            | { suite; tests } :: rest ->
                log "";
                log "%s" suite;
                let started = Node.Date.now () in
                let* outcomes = loop [] (List.map (fun t -> suite, t) tests) in
                log "  (%s: %.0fs)" suite ((Node.Date.now () -. started) /. 1000.);
                suites_loop (acc @ outcomes) rest in
          let* outcomes = suites_loop [] suites in
          Promise.return (summary outcomes)
