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

open Types

type t = {
  mutable gdbPath : string;
  mutable procEnv : string Interop.Dict.t;
  mutable noDebug : bool;
  mutable gdbTty : Types.GdbTty.t;
  mutable cobcrunPath: string;
  mutable gdbTargetWrapperPath : string;
  mutable useCobcrun: bool;
  mutable sourceDirs: string list;
  mutable breakpoints : int Breakpoint.Map.t; (* or set *)
  mutable ignoredBreakpoints : Breakpoint.Set.t;
  mutable lastStepCommand : t -> bool Promise.t;
  mutable hasCobPutFieldStr : bool;
  mutable hasCobGetFieldStr : bool;
  mutable subroutine : int; (* for step into / step over *)
  mutable map : SourceMap.t;
  mutable tok : int;
  mutable handlers : (MiNode.result -> unit) IntMap.t;
  mutable process : Node.ChildProcess.t option;
  mutable ttyName : string option;
  mutable ttyPid : int option;
  mutable newConsole : bool;
  buffer : SmartBuffer.t;
  errbuf : SmartBuffer.t;
  eventEmitter : Node.Events.EventEmitter.t;
  sessionId : int;
}

type failure_handling =
  | Resolve
  | Reject

module Event = struct

  external of_js : Ojs.t -> 'a = "%identity"
  external to_js : 'a -> Ojs.t = "%identity"

  type _ t =
    | Message         : (string * string) t
    | DebugReady      : unit t
    | Quit            : unit t
    | ExecAsyncOutput : MiNode.async t
    | ExitedNormally  : MiNode.async t
    | SignalStop      : MiNode.async t
    | Stopped         : MiNode.async t
    | Running         : MiNode.async t
    | StepEnd         : MiNode.async t
    | StepOutEnd      : MiNode.async t
    | ThreadCreated   : MiNode.async t
    | ThreadExited    : MiNode.async t

  let toString (type a) (event : a t) =
    match event with
    | Message         -> "msg"
    | DebugReady      -> "debug-ready"
    | Quit            -> "quit"
    | ExecAsyncOutput -> "exec-async-output"
    | ExitedNormally  -> "exited-normally"
    | SignalStop      -> "signal-stop"
    | Stopped         -> "stopped"
    | Running         -> "running"
    | StepEnd         -> "step-end"
    | StepOutEnd      -> "step-out-end"
    | ThreadCreated   -> "thread-created"
    | ThreadExited    -> "thread-exited"

  let emit (type a) mi2 (event : a t) (arg : a) =
    let args =
      match event with
      | Message         -> [[%js.of: string] (fst arg);
                            [%js.of: string] (snd arg)]
      | DebugReady      -> []
      | Quit            -> []
      | ExecAsyncOutput -> [to_js arg]
      | ExitedNormally  -> [to_js arg]
      | SignalStop      -> [to_js arg]
      | Stopped         -> [to_js arg]
      | Running         -> [to_js arg]
      | StepEnd         -> [to_js arg]
      | StepOutEnd      -> [to_js arg]
      | ThreadCreated   -> [to_js arg]
      | ThreadExited    -> [to_js arg]
    in
    ignore @@ Node.Events.EventEmitter.emit
      mi2.eventEmitter ~eventName:(toString event) ~args

  let wrapListener (type a) (event : a t) (listener : a -> unit) =
    (fun ~args ->
       match event, args with
       | Message,    a1 :: a2 :: _ -> listener ([%js.to: string] a1,
                                                [%js.to: string] a2)
       | DebugReady,             _ -> listener ()
       | Quit,                   _ -> listener ()
       | ExecAsyncOutput, arg :: _ -> listener (of_js arg)
       | ExitedNormally,  arg :: _ -> listener (of_js arg)
       | SignalStop,      arg :: _ -> listener (of_js arg)
       | Stopped,         arg :: _ -> listener (of_js arg)
       | Running,         arg :: _ -> listener (of_js arg)
       | StepEnd,         arg :: _ -> listener (of_js arg)
       | StepOutEnd,      arg :: _ -> listener (of_js arg)
       | ThreadCreated,   arg :: _ -> listener (of_js arg)
       | ThreadExited,    arg :: _ -> listener (of_js arg)

       | (Message | ExecAsyncOutput | ExitedNormally |
          SignalStop | Stopped | Running | StepEnd | StepOutEnd |
          ThreadCreated | ThreadExited ), _ -> ()
    )

  let on (type a) mi2 (event : a t) (listener : a -> unit) =
    ignore @@ Node.Events.EventEmitter.on mi2.eventEmitter
      ~eventName:(toString event)
      ~listener:(wrapListener event listener)

  let once (type a) mi2 (event : a t) (listener : a -> unit) =
    ignore @@ Node.Events.EventEmitter.once mi2.eventEmitter
      ~eventName:(toString event)
      ~listener:(wrapListener event listener)

  let oncep (type a) mi2 (event : a t) =
    Promise.make @@ fun ~resolve ~reject:_ -> once mi2 event resolve

end

(* Unique session ID to distinguish external terminals.
   In practice we will rarely have more than
   one terminal par VSCode instance. *)
let genSessionId =
  let counter = ref 0 in
  fun () ->
    counter := !counter + 1;
    if !counter >= 512 then
      counter := 1;
    !counter * 4194304 + Node.Process.pid

let rec create () =
  {
    gdbPath = "";
    procEnv = Interop.Dict.empty;
    noDebug = false;
    gdbTty = Bool (false);
    cobcrunPath = "";
    gdbTargetWrapperPath = "";
    useCobcrun = false;
    sourceDirs = [];
    breakpoints = Breakpoint.Map.empty;
    ignoredBreakpoints = Breakpoint.Set.empty;
    lastStepCommand = continue;
    hasCobPutFieldStr = true;
    hasCobGetFieldStr = true;
    subroutine = -1;
    map = SourceMap.empty;
    tok = 0;
    handlers = IntMap.empty;
    process = None;
    ttyName = None;
    ttyPid = None;
    newConsole = false;
    buffer = SmartBuffer.create ~size:4096 ();
    errbuf = SmartBuffer.create ~size:4096 ();
    eventEmitter = Node.Events.EventEmitter.create ();
    sessionId = genSessionId ();
  }

and init mi2 gdbPath procEnv noDebug
    gdbTty cobcrunPath gdbTargetWrapperPath useCobcrun sourceDirs =
  let procEnv =
    Interop.Dict.union (fun _n _v1 v2 ->
        Some v2
      ) Node.Process.Env.env procEnv
  in
  mi2.gdbPath <- gdbPath;
  mi2.procEnv <- procEnv;
  mi2.noDebug <- noDebug;
  mi2.gdbTty <- gdbTty;
  mi2.cobcrunPath <- cobcrunPath;
  mi2.gdbTargetWrapperPath <- gdbTargetWrapperPath;
  mi2.useCobcrun <- useCobcrun;
  mi2.sourceDirs <- sourceDirs

(* target = /path/to/prog.cob *)
and load mi2 cwd target targetArgs group : unit Promise.t =
  let target = (* TODO: differs from targetCobExecutable ? *)
    if mi2.useCobcrun then
      target
    else
      let target = Node.Path.resolve [cwd; Filename.basename target] in
      let target = Filename.remove_extension target in
      if Node.Process.platform = "win32" then target ^ ".exe"
      else target
  in
  loadOrAttach mi2 cwd target ~targetArgs group

and attach mi2 cwd target group : unit Promise.t =
  let target = (* TODO: why file name different between load and attach ?*)
    if Filename.is_relative target then
      Filename.concat cwd target
    else
      target
  in (* TODO: how about .exe under Windows ? *)
  loadOrAttach mi2 cwd target group

and loadOrAttach mi2 cwd target ?targetArgs group =
  if not (Node.Fs.existsSync cwd) then
    Promise.reject (mkError "cwd does not exist")
  else
    begin
      begin
        try mi2.map <- SourceMap.create cwd (target :: group) mi2.sourceDirs
        with e -> Log.error [Printexc.to_string e]
      end;
      Log.debug ["Source map created"];
      Log.info [SourceMap.toString mi2.map];
      begin
        match targetArgs with
        | Some (_targetArgs) ->
            setupTerminal mi2 target
        | None ->
            Promise.return ()
      end |>
      Promise.then_ ~fulfilled:(fun () ->
          let process =
            Node.ChildProcess.(spawn_process mi2.gdbPath
                [|"gdb"; "--interpreter=mi2"; "--quiet"|]
                ~options:(Options.create ~cwd ~env:mi2.procEnv ()) ())
          in
          Node.Stream.Readable.on
            (Node.ChildProcess.get_stdout process) (`Data (stdout mi2));
          Node.Stream.Readable.on
            (Node.ChildProcess.get_stderr process) (`Data (stderr mi2));
          Node.ChildProcess.on process
            (`Exit (fun ~code:_ ?signal:_ () ->
                 Option.iter (fun ttyPid ->
                     Node.Process.kill ttyPid ()) mi2.ttyPid;
                 Event.(emit mi2 Quit ())));
          (Promise.make @@ fun ~resolve ~reject ->
           Node.ChildProcess.on process (`Error (fun ~err -> reject err));
           Node.ChildProcess.on process (`Spawn resolve)) |>
          Promise.then_ ~fulfilled:(fun () ->
              mi2.process <- Some (process);
              initCommands mi2 target ?targetArgs cwd |> List.rev |>
              Promise.all_list |> Promise.then_ ~fulfilled:(fun _ ->
                  Promise.resolve ()
                )
            )
        )
    end

and initCommands mi2 target ?targetArgs cwd =
  let cwd' = "\"" ^ (Util.escape cwd) ^ "\"" in
  let dirs = "\"" ^ (String.concat "\" \""
                       (List.map Util.escape mi2.map.sourceDirs)) ^ "\"" in
  let commands =
    (* NOTE: mi-async is GDB 7.8 (2014), was target-async before *)
    [ sendCommand mi2 "gdb-set mi-async on" ();
      sendCommand mi2 "gdb-set print null-stop off" ();
      sendCommand mi2 "gdb-set print raw-values on" ();
      sendCommand mi2 "gdb-set print elements 1000" ();
      sendCommand mi2 "gdb-set print repeats 1000" ();
      sendCommand mi2 "gdb-set charset UTF-8" ();
      sendCommand mi2 ("environment-directory " ^ cwd') ();
      sendCommand mi2 "gdb-set stop-on-solib-events 1" ();
      sendCommand mi2 ("gdb-set directories " ^ dirs) () ] |> List.rev
  in
  let commands =
    match targetArgs with
    | Some (targetArgs) ->
        let targetArgs =
          if mi2.useCobcrun then
            Util.removePathExtension target ^ " " ^ targetArgs
          else
            targetArgs
        in
        sendCommand mi2 ("gdb-set args " ^ targetArgs) () :: commands
    | None ->
        commands
  in
  let targetExec =
    if mi2.useCobcrun then mi2.cobcrunPath
    else targetCobExecutable target cwd
  in
  let commands =
    sendCommand mi2 ("file-exec-and-symbols \"" ^ targetExec ^ "\"") () ::
    commands
  in
  let commands =
    match mi2.ttyName, mi2.newConsole with
    | Some _, _ ->
        sendCommand mi2 ("gdb-set exec-wrapper " ^ mi2.gdbTargetWrapperPath) () :: commands
    | None, true ->
        sendCommand mi2 "gdb-set new-console on" () :: commands
    | None, false ->
        (* TODO: Win32 *)
        sendCommand mi2 "gdb-set inferior-tty /dev/null" () :: commands
  in
  commands |> List.rev

and targetCobExecutable target cwd =
  let target = Util.removePathExtension target in
  let target =
    if Filename.is_relative target then
      Filename.concat cwd target
    else
      target
  in
  let target =
    if Node.Process.platform = "win32" then
      target ^ ".exe"
    else
      target
  in
  Util.escape target

and stdout mi2 ~chunk =
  let data =
    match chunk with
    | `String s -> s
    | `Buffer b -> Node.Buffer.toString b
  in
  Log.debug ["stdout: "; data];
  SmartBuffer.append mi2.buffer data;
  SmartBuffer.foreach_line mi2.buffer (fun b off len ->
      let line = Bytes.sub_string b off len in
      let lexbuf = Lexing.from_string ~with_positions:false line in
      try onMiNode mi2 (Mi2Parser.node Mi2Lexer.token lexbuf)
      with _ -> (
        () (* NOTE: (gdb) marker arrives here *))
    )

and stderr mi2 ~chunk =
  let data =
    match chunk with
    | `String s -> s
    | `Buffer b -> Node.Buffer.toString b
  in
  Log.debug ["stderr: "; data];
  SmartBuffer.append mi2.errbuf data;
  SmartBuffer.foreach_line mi2.errbuf (fun b off len ->
      log mi2 "stderr" (Bytes.sub_string b off len)
    );

and stdin mi2 ?cb fmt =
  Format.kasprintf (fun s ->
    match mi2.process with
    | Some (process) ->
        Log.debug ["stdin: "; s];
        (* We must insert a newline to execute the command. *)
        let data = Format.sprintf "%s\n" s in
        Node.Stream.Writable.write
          (Node.ChildProcess.get_stdin process) data ?callback:cb ()
    | None ->
       ()
  ) fmt

and onMiNode mi2 (node : MiNode.t) =
  match node with
  | Result ({ token; class_; _ } as rnode) -> (* result = ^ *)
      let tok = match token with Some t -> t | _ -> 0 in
      begin
        match IntMap.find_opt tok mi2.handlers with
        | Some (handler) ->
            mi2.handlers <- IntMap.remove tok mi2.handlers;
            handler rnode
        | None ->
            if class_ = Error then
              log mi2 "stderr" MiNode.(rfield rnode "msg" |> Value.string)
            else
              Log.error ["Unhandled: "; MiNode.toString node]
      end

  | Stream { kind; output } -> (* stream = ~ @ & *)
      log mi2 (MiNode.stringOfStreamKind kind) output

  | Async ({ kind = Exec; class_; _ } as anode) -> (* async/exec = * *)
      Event.(emit mi2 ExecAsyncOutput anode);
      let frame = MiNode.afield anode "frame" in
      let frameFullname, frameLine =
        MiNode.Value.(field "fullname" frame |> string,
                      field "line" frame |> int ~def:(-1))
      in
      mi2.subroutine <-
        SourceMap.hasLineSubroutine mi2.map frameFullname frameLine;
      begin
        match class_ with
        | Running ->
            Event.(emit mi2 Running anode)
        | Stopped ->
            let reason = MiNode.(afield anode "reason" |> Value.string) in
            Log.debug ["stop:"; reason];
            let hasLineCobol =
              SourceMap.hasLineCobol mi2.map frameFullname frameLine in
            begin
              match reason with
              | "breakpoint-hit" -> (* extra fields: disp, bkpto, frame *)
                  if hasLineCobol then
                    Event.(emit mi2 StepEnd anode)
                  else
                    let disposition = (* del / keep *)
                      MiNode.(afield anode "disp" |> Value.string) in
                    if mi2.lastStepCommand == continue &&
                       disposition = "del" then
                      resume mi2 |> ignore
                    else
                      stepOver mi2 |> ignore
              | "loaction-reached" ->
                  if hasLineCobol then
                    Event.(emit mi2 StepEnd anode)
                  else
                    stepOver mi2 |> ignore
              | "end-stepping-range" ->
                  if hasLineCobol then
                    Event.(emit mi2 StepEnd anode)
                  else
                    resume mi2 |> ignore
              | "function-finished" ->
                  if hasLineCobol then
                    Event.(emit mi2 StepOutEnd anode)
                  else
                    resume mi2 |> ignore
              | "signal-received" ->
                  Event.(emit mi2 SignalStop anode)
              | "exited-normally" ->
                  Event.(emit mi2 ExitedNormally anode)
              | "exited" ->
                  let exitCode =
                    MiNode.(afield anode "exit-code" |> Value.string) in
                  Log.info ["Program exited with code "; exitCode];
                  Event.(emit mi2 Quit ())
              | "solib-event" ->
                  onSolibEvent mi2 anode;
                  resume mi2 |> ignore
              | _ ->
                  if hasLineCobol then
                    begin
                      Log.info ["Unimplemented stop reason: "; reason];
                      Event.(emit mi2 Stopped anode)
                    end
                  else
                    continue mi2 |> ignore
            end
        | Other _ | _ ->
            Log.debug [MiNode.toString node];
      end

  | Async ({ kind = Notify; class_; _ } as anode) -> (* async/notify = = *)
      let processLib updateFun libMsg mapMsg =
        let lib = MiNode.(afield anode "target-name" |> Value.string) in
        let map, changed = updateFun mi2.map lib in
        if changed then
          begin
            mi2.map <- map;
            Log.debug [libMsg; lib];
            Log.debug [mapMsg; SourceMap.toString mi2.map];
            reloadBreakpoints mi2 |> ignore
          end
      in
      begin
        match class_ with
        | ThreadCreated ->
            Event.(emit mi2 ThreadCreated anode)
        | ThreadExited ->
            Event.(emit mi2 ThreadExited anode)
        | LibraryLoaded ->
            processLib SourceMap.addLib
              "Added library to map: " "Source map created\n "
        | LibraryUnloaded ->
            processLib SourceMap.remLib
              "Removed library from map: " "Source map updated\n "
        | Running | Stopped | Other _ | _ ->
            Log.debug [MiNode.toString node];
      end

  | Async { kind = Status; _ } -> (* async/status = + *)
      ()

and onSolibEvent mi2 anode =
  let processLibs event msg update_fun =
    let libs = MiNode.(afield anode event) in
    let map, changed =
      MiNode.Value.kvFold (fun (map, changed) key lib ->
          if key <> "library" then
            map, changed
          else
            begin
              let lib = MiNode.Value.string lib in
              Log.debug [msg; "library:"; lib];
              let map, res = update_fun map lib in
              map, changed || res
            end
        ) (mi2.map, false) libs
    in
    mi2.map <- map;
    changed
  in
  let added = processLibs "added" "loaded" SourceMap.addLib in
  let removed = processLibs "removed" "unloaded" SourceMap.remLib in
  if added || removed then
    begin
      Log.debug ["Source map updated\n "; SourceMap.toString mi2.map];
      reloadBreakpoints mi2 |> ignore
    end

and start mi2 ?attachTarget () : bool Promise.t =
  Log.debug ["start"];
  let send cmd expected =
    sendCommand mi2 cmd () |>
    Promise.then_ ~fulfilled:(fun (rnode : MiNode.result) ->
        (* NOTE: this clears the terminal upon start, to hide a GDB warning *)
        Option.iter (fun ttyName ->
            Node.Fs.writeFileSync
              ("/dev/" ^ ttyName) "\x1B[2J\x1B[H") mi2.ttyName;
        if rnode.class_ = expected then
            Promise.resolve false
        else
            Promise.reject (mkError "Unexpected outcome on start" ())
      )
  in
  if mi2.noDebug then
    send "exec-run" Running
  else
    Event.oncep mi2 Event.DebugReady |>
    Promise.then_ ~fulfilled:(fun _args ->
        match attachTarget with
        | None | Some "" ->
            send "exec-run" Running
        | Some t when Util.isNumeric t ->
            send ("target-attach " ^ t) Done
        | Some t ->
            send ("target-select remote " ^ t) Connected
      )

and stop mi2 =
  Log.debug ["stop"];
  stopOrDetach mi2 "gdb-exit" MiNode.Exit

and detach mi2 =
  Log.debug ["detach"];
  stopOrDetach mi2 "target-detach" MiNode.Done

and stopOrDetach mi2 cmd expected =
  (* NOTE: pid might be null when calling this function
     (if called when launching gdb failed) *)
  match mi2.process with
  | Some (process) ->
      let to_ = Node.setTimeout (fun () ->
          Node.Process.kill (- (Node.ChildProcess.get_pid process)) ()
        ) 1000
      in
      Node.ChildProcess.on process
        (`Exit (fun ~code:_ ?signal:_ () -> Node.clearTimeout to_));
      sendCommand mi2 cmd ~on_failure:Resolve () |> resultIs expected
  | None ->
      Promise.return false

and interrupt mi2 : bool Promise.t =
  Log.debug ["interrupt"];
  sendCommand mi2 "exec-interrupt" () |> resultIs MiNode.Done

(* Repeats the last step command (continue, stepOver, stepInto, stepOut) *)
and resume mi2 =
  Log.debug ["resume"];
  mi2.lastStepCommand mi2

and continue mi2 : bool Promise.t =
  mi2.lastStepCommand <- continue;
  Log.debug ["continue"];
  sendCommand mi2 "exec-continue" () |> resultIs Running

and stepOver mi2 : bool Promise.t =
  mi2.lastStepCommand <- stepOver;
  Log.debug ["stepOver"];
  let cmd =
    if mi2.subroutine < 0 then "exec-next"
    else "exec-until " ^ (string_of_int mi2.subroutine)
  in
  sendCommand mi2 cmd () |> resultIs Running

and stepInto mi2 : bool Promise.t =
  Log.debug ["stepInto"];
  mi2.lastStepCommand <- stepInto;
  if mi2.subroutine < 0 then
    sendCommand mi2 "exec-step" () |> resultIs Running
  else
    sendCommand mi2 ("break-insert -t " ^
                      (string_of_int mi2.subroutine)) () |>
    Promise.then_ ~fulfilled:(fun _rnode ->
        sendCommand mi2 "exec-step" () |> resultIs Running)

and stepOut mi2 : bool Promise.t =
  Log.debug ["stepOut"];
  mi2.lastStepCommand <- stepOut;
  sendCommand mi2 "exec-finish" () |> resultIs Running

and goto mi2 filename line : bool Promise.t =
  Log.debug ["goto"];
  let filename = if filename = "" then "" else (Util.escape filename) ^ ":" in
  let target = "\"" ^ filename ^ (string_of_int line) ^ "\"" in
  sendCommand mi2 ("break-insert -t " ^ target) () |>
  Promise.then_ ~fulfilled:(fun _rnode ->
      sendCommand mi2 "exec-step" () |> resultIs Running)

and reloadBreakpoints mi2 =
  Log.debug ["reloadBreakpoints"];
  let breakpoints = mi2.ignoredBreakpoints in
  mi2.ignoredBreakpoints <- Breakpoint.Set.empty;
  let promises =
    Breakpoint.Set.fold (fun bkpt promises ->
        addBreakpoint mi2 bkpt :: promises
      ) breakpoints []
  in
  Promise.all_list promises

(* TODO: add in map according to input data file/line/fun *)
and addBreakpoint mi2 (breakpoint : Breakpoint.t)
  : Breakpoint.t option Promise.t =
  Log.debug ["addBreakpoint"];
  if Breakpoint.Map.mem breakpoint mi2.breakpoints then
    Promise.resolve None
  else
    let loc_opt =
      match breakpoint.location with
      | Function (funName) ->
          Some ("\"" ^ (Util.escape funName) ^ "\"")
      | FileLine { file; line } ->
          match SourceMap.getLineC mi2.map file line with
          | Some (sline) ->
              Some ("\"" ^ (Util.escape sline.cFile) ^ ":" ^
                    (string_of_int sline.cLine) ^ "\"")
          | None ->
              Log.debug [
                "addBreakPoint: ignoring breakpoint ";
                "for unknown source file: ";
                file; ":"; string_of_int line ];
              mi2.ignoredBreakpoints <-
                Breakpoint.Set.add breakpoint mi2.ignoredBreakpoints;
              None
    in
    match loc_opt with
    | None ->
        Promise.resolve None
    | Some (loc) ->
        let count =
          match breakpoint.countCondition with
          | None | Some ("") -> ""
          | Some (count) when Util.isNumeric count ->
              "-t -i " ^ count
          | Some (count) when count.[0] = '>' ->
              let count = String.sub count 1 (String.length count - 2) in
              if Util.isNumeric count then "-i " ^ count
              else "-t"  (* Invalid condition: temporary bp *)
          | Some (_count) ->
              "-t" (* Invalid condition: temporary bp *)
        in
        let cond =
          match breakpoint.condition with
          | None | Some ("") -> ""
          | Some (cond) -> "-c " ^ cond
        in
        sendCommand mi2
          ("break-insert -f " ^ loc ^ " " ^ count ^ " " ^ cond) () |>
        Promise.then_ ~fulfilled:(fun (rnode : MiNode.result) ->
            let bkpt = MiNode.rfield rnode "bkpt" in
            let number = MiNode.Value.(field "number" bkpt |> int) in
            (* NOTE: the following does not seem to be necessary *)
            (*
            let location =
              MiNode.Value.(field "original-location" bkpt |> string) in
            let file, line =
              match String.split_on_char '\'' location with
              | file :: line :: [] ->
                  file, (try int_of_string line with _ -> -1)
              | _ ->
                  "", -1
            in
            let breakpoint = Breakpoint.{
              location = FileLine { file; line };
              condition = None;
              countCondition = None;
            } in
            *)
            mi2.breakpoints <-
              Breakpoint.Map.add breakpoint number mi2.breakpoints;
            Promise.resolve (Some (breakpoint))
          )

(* NOTE: never rejected *)
and clearBreakpoints mi2 file : bool Promise.t =
  Log.debug ["clearBreakpoints"];
  let isBreakpointForFile file (bp : Breakpoint.t) =
    match bp.location with
    | FileLine { file = f; line = _ } when file = f -> true
    | FileLine (_) | Function _ -> false
  in
  let toDelete, remaining =
    Breakpoint.Map.fold (fun (bp : Breakpoint.t) n (del, rem) ->
        if isBreakpointForFile file bp then string_of_int n :: del, rem
        else del, Breakpoint.Map.add bp n rem
      ) mi2.breakpoints ([], Breakpoint.Map.empty)
  in
  if toDelete = [] then
    Promise.resolve true
  else
    sendCommand mi2 ("break-delete " ^ (String.concat " " toDelete)) () |>
    Promise.then_ ~fulfilled:(fun (_rnode : MiNode.result) ->
        mi2.breakpoints <- remaining;
        mi2.ignoredBreakpoints <-
          Breakpoint.Set.filter (fun bp ->
              not (isBreakpointForFile file bp)) mi2.ignoredBreakpoints;
        Promise.resolve true
      ) ~rejected:(fun _err ->
        Promise.resolve false
      )

and getThreads mi2 : Thread.t list Promise.t =
  Log.debug ["getThreads"];
  if mi2.noDebug then
    Promise.resolve []
  else
    sendCommand mi2 "thread-info" () |>
    Promise.then_ ~fulfilled:(fun (rnode : MiNode.result) ->
        let threads = MiNode.rfield rnode "threads" in
        let threads =
          MiNode.Value.map (fun thread ->
              let id, targetId, name = MiNode.Value.(
                  field "id" thread |> int,
                  field "target-id" thread |> int,
                  field "name" thread |> opt string)
              in
              Thread.{ id; targetId; name }
            ) threads
        in
        Promise.resolve threads
      )

(* NOTE: never rejected *)
and getStack mi2 threadId maxFrameLevels : Stack.t list Promise.t =
  Log.debug ["getStack"];
  let command = "stack-list-frames" in
  let command =
    if threadId <> 0 then
      command ^ " --thread " ^ (string_of_int threadId)
    else
      command
  in
  let command =
    if maxFrameLevels <> 0 then
      command ^ " 0 " ^ (string_of_int maxFrameLevels)
    else
      command
  in
    sendCommand mi2 command () |>
    Promise.then_ ~fulfilled:(fun (rnode : MiNode.result) ->
        let stack = MiNode.rfield rnode "stack" in
        let stack =
          MiNode.Value.kvFilterMap (fun key frame ->
              if key <> "frame" then
                None
              else
                let level, func, from, file, line = MiNode.Value.(
                    field "level" frame |> int,
                    field "func" frame |> opt string,
                    field "from" frame |> opt string,
                    field "fullname" frame |> opt string,
                    field "line" frame |> opt int)
                in
                let function_ =
                  match func, from with
                  | Some (func), _ -> func
                  | None, Some (from) -> from
                  | _ -> ""
                in
                let file, line, lineContents =
                  match file, line with
                  | Some (file), Some (line) ->
                      begin
                        match SourceMap.getLineCobol mi2.map file line with
                        | Some (sline) ->
                            sline.cobFile, sline.cobLine, sline.cobolLine
                        | None ->
                            file, line, ""
                      end
                  | _ ->
                      "(unknown)", 0, ""
                in
                Some (Stack.{ level; function_; file; line; lineContents })
            ) stack
        in
        Promise.resolve stack
      ) ~rejected:(fun _err ->
        Promise.resolve []
      )

(* Retrive the list of global "storage" symbols (ie those stating with b_);
   results are grouped by file *)
(* NOTE: never rejected *)
and getGlobalStorageSymbols mi2 : FileSymbols.t list Promise.t =
  Log.debug ["getGlobalStorageSymbols"];
  sendCommand mi2 ("symbol-info-variables --name \"^b_[0-9]*$\"") () |>
  Promise.then_ ~fulfilled:(fun (rnode : MiNode.result) ->
      let debug = MiNode.(rfield rnode "symbols" |> Value.field "debug") in
      let symbols = MiNode.Value.map (fun file_symbols ->
          let filename, fullname, symbols = MiNode.Value.(
              field "filename" file_symbols |> string,
              field "fullname" file_symbols |> string,
              field "symbols" file_symbols |> map (fun symbol ->
                  let line, name, type_, description =
                    field "line" symbol |> int,
                    field "name" symbol |> string,
                    field "type" symbol |> string,
                    field "description" symbol |> string
                  in
                  Symbol.{ line; name; type_; description }
                )
            )
          in
          FileSymbols.{ filename; fullname; symbols }
        ) debug
      in
      Promise.resolve symbols
    ) ~rejected:(fun _err ->
      Promise.resolve []
    )

(* Retrieve the list of local "storage" variables (ie those strating with b_) *)
(* NOTE: never rejected *)
and getLocalStorageVariables mi2 threadId frameLevel
  : DebuggerVariable.t list Promise.t =
  Log.debug ["getLocalStorageVariables"];
  getCurrentFunctionName mi2 |>
  Promise.then_ ~fulfilled:(fun funName_opt ->
      match funName_opt with
      | None ->
          Promise.resolve []
      | Some (funName) ->
          sendCommand mi2 ("stack-list-variables --thread " ^
                            (string_of_int threadId) ^ " --frame " ^
                            (string_of_int frameLevel) ^ " --all-values") () |>
          Promise.then_ ~fulfilled:(fun (rnode : MiNode.result) ->
              MiNode.(rfield rnode "variables") |>
              MiNode.Value.fold (fun vars v ->
                  let cName, value =
                    MiNode.Value.(field "name" v |> string,
                                  field "value" v |> string)
                  in
                  if String.starts_with ~prefix:"b_" cName then
                    SourceMap.findLocalByC mi2.map funName cName |>
                    Option.fold ~none:vars ~some:(fun dv ->
                          DebuggerVariable.setValue dv value;
                          dv :: vars)
                  else
                    vars
                ) [] |> List.rev |>
              Promise.resolve
            ) ~rejected:(fun _err ->
              Promise.resolve []
            )
    )

(* Find a (local) variable by its C name and get its value *)
(* NOTE: threadId and frameLevel are actually never passed to this function *)
and evalLocalCVariable mi2 cName ?(threadId=0) ?(frameLevel=0) ()
  : DebuggerVariable.t option Promise.t =
  Log.debug ["evalCLocalVariable "; cName];
  getCurrentFunctionName mi2 |>
  Promise.then_ ~fulfilled:(fun funName_opt ->
      match funName_opt with
      | None ->
          Promise.resolve None
      | Some (funName) ->
          begin
            let dv_opt = SourceMap.findLocalByC mi2.map funName cName in
            match dv_opt with
            | Some (dv) ->
                evalVariable mi2 dv ~threadId ~frameLevel () |>
                Promise.then_ ~fulfilled:(fun () ->
                    Promise.resolve (Some dv))
            | None ->
                Promise.resolve None
          end
    )

(* Find a global variable by its C name and get its value *)
and evalGlobalCVariable mi2 cName : DebuggerVariable.t option Promise.t =
  Log.debug ["evalCGlobalVariable "; cName];
  let dv_opt = SourceMap.findGlobalByC mi2.map cName in
  match dv_opt with
  | Some (dv) ->
      evalVariable mi2 dv () |>
      Promise.then_ ~fulfilled:(fun () -> Promise.resolve (Some dv))
  | None ->
      Promise.resolve None

(* Find a global symbol by its C name and get its value *)
and evalGlobalSymbol mi2 (s : LocalizedSymbol.t)
  : DebuggerVariable.t Promise.t =
  Log.debug ["evalGlobalSymbol"];
  let dv_opt =
    SourceMap.findGlobalByC mi2.map ~cFile:s.filename s.symbol.name in
  match dv_opt with
  | Some v ->
      evalVariable mi2 v () |>
      Promise.then_ ~fulfilled:(fun _ -> Promise.resolve v)
  | None ->
      Promise.reject (mkError ("Unknown symbol: " ^ s.symbol.name))

(* This updates the variable description *)
and evalVariable mi2 (dv : DebuggerVariable.t) ?(threadId=0) ?(frameLevel=0) () =
  let command = "data-evaluate-expression " in
  let arguments =
    if threadId <> 0 then
      "--thread " ^ (string_of_int threadId) ^
      " --frame " ^ (string_of_int frameLevel) ^ " "
    else
      ""
  in
  let cName = Util.escape dv.cName in
  let expression =
    if mi2.hasCobGetFieldStr && dv.isField then
      "\"(char *)cob_get_field_str_buffered(&" ^ cName ^ ")\""
    else if dv.isField then
      cName ^ ".data"
    else
      cName
  in
  sendCommand mi2 (command ^ arguments ^ expression) () |>
  Promise.then_ ~rejected:(fun err ->
      let msg = Node.JsError.message err in
      (* NOTE: cob_get_field_str added (06/2020) in GC 3.1 (12/2020) *)
      let re = Str.regexp_string "No symbol \"cob_get_field_str_buffered\"" in
      if Util.matches re msg then
        begin
          mi2.hasCobGetFieldStr <- false;
          evalVariable mi2 dv ~threadId ~frameLevel ()
        end
      else
        begin
          log mi2 "stderr" (msg);
          Promise.reject err
        end
    ) ~fulfilled:(fun rnode ->
      let value = MiNode.(rfield rnode "value" |> Value.string) in
      begin
        if mi2.hasCobGetFieldStr && dv.isField then
          DebuggerVariable.setValuePreformatted dv value
        else
          DebuggerVariable.setValue dv value
      end;
      Promise.resolve ()
    )

and setLocalCobolVariable mi2 cobName rawValue : string list Promise.t =
  Log.debug ["setLocalCobolVariable"];
  setLocalVariable SourceMap.findLocalByCobol mi2 cobName rawValue

and setLocalCVariable mi2 cName rawValue : string list Promise.t =
  Log.debug ["setLocalCVariable"];
  setLocalVariable SourceMap.findLocalByC mi2 cName rawValue

and setLocalVariable findFunc mi2 name rawValue =
  getCurrentFunctionName mi2 |>
  Promise.then_ ~fulfilled:(fun funName_opt ->
      let dv_opt =
        Option.bind funName_opt (fun funName ->
            findFunc mi2.map funName name) in
      match dv_opt with
      | Some dv ->
          setDebuggerVariable mi2 dv rawValue
      | None ->
          log mi2 "stderr" ("Failed to set cob field value on " ^ name);
          Promise.reject (mkError ("Local variable not found: " ^ name))
    )

and lookupGlobalCobolWithCName mi2 cobName cName : DebuggerVariable.t option =
  let reFile = Str.regexp {|^'\([^']+\)'::.*$|} in
  let cFile =
    if Util.matches reFile cName then Some (Util.group cName 1)
    else None
  in
  SourceMap.findGlobalByCobol mi2.map ?cFile cobName

and setGlobalCVariable mi2 cName rawValue : string list Promise.t =
  Log.debug ["setGlobalCVariable"];
  let dv_opt = SourceMap.findGlobalByC mi2.map cName in
  match dv_opt with
  | Some dv ->
      setDebuggerVariable mi2 dv rawValue
  | None ->
      log mi2 "stderr" ("Failed to set cob field value on " ^ cName);
      Promise.reject (mkError ("Global variable not found: " ^ cName))

and setDebuggerVariable mi2 (dv : DebuggerVariable.t) rawValue =
  let cleanedRawValue = Util.cleanRawValue rawValue in
  begin
    let cName = Util.escape dv.cName in
    if dv.attribute.type_ = INTEGER then
      sendCommand mi2 ("gdb-set var " ^
                       cName ^ "=" ^ cleanedRawValue) ()
    else if mi2.hasCobPutFieldStr && dv.isField then
      sendCommand mi2
        ("data-evaluate-expression \"(int)cob_put_field_str(&" ^
         cName ^ ", \\\"" ^ cleanedRawValue ^ "\\\")\"") ()
    else
      let finalValue = DebuggerVariable.formatValue dv cleanedRawValue in
      let cName =
        if dv.isField then cName ^ ".data"
        else cName
      in
      sendCommand mi2
        ("data-evaluate-expression \"(void)strncpy(" ^ cName ^ ", \\\"" ^
         finalValue ^ "\\\", " ^ (string_of_int dv.size) ^ ")\"") ()
  end |>
  Promise.then_ ~fulfilled:(fun _rnode ->
      if dv.parent <> None || DebuggerVariable.hasChildren dv then
        Promise.resolve ["variables"]
      else
        Promise.resolve []
    ) ~rejected:(fun err ->
      let msg = Node.JsError.message err in
      (* NOTE: cob_put_field_str added (06/2020) in GC 3.1 (12/2020) *)
      let re = Str.regexp_string "No symbol \"cob_put_field_str\"" in
      if Util.matches re msg then
        begin
          mi2.hasCobPutFieldStr <- false;
          setDebuggerVariable mi2 dv rawValue
        end
      else
        begin
          log mi2 "stderr" ("Failed to set cob field value on " ^ dv.cName);
          log mi2 "stderr" msg;
          Promise.reject err
        end
    )

(* NOTE: threadId 0 => global scope *)
and evalExpression mi2 expression threadId frameLevel
  : string option Promise.t =
  Log.debug ["evalExpression"];
  getCurrentFunctionName mi2 |>
  Promise.then_ ~fulfilled:(fun funName_opt ->
      match funName_opt with
      | None ->
          Promise.resolve None
      | Some (funName) ->
          (* TODO: finish the parse expression feature *)
          let dv_opt = SourceMap.findLocalByCobol mi2.map funName expression in
          let dv_opt =
            if dv_opt = None then
              SourceMap.findGlobalByCobol mi2.map expression
            else
              dv_opt
          in
          match dv_opt with
          | None ->
              Promise.resolve None
          | Some (dv) ->
              evalVariable mi2 dv ~threadId ~frameLevel () |>
              Promise.then_ ~fulfilled:(fun () ->
                  Promise.resolve (Some (dv.value)))
    )

(* NOTE: never rejected *)
and getCurrentFunctionName mi2 =
  Log.debug ["getCurrentFunctionName"];
  sendCommand mi2 ("stack-info-frame") () |>
  Promise.then_ ~fulfilled:(fun (rnode : MiNode.result) ->
      let frame = MiNode.(rfield rnode "frame") in
      let res =
        MiNode.Value.(field "func" frame |>
                      opt (fun v -> String.lowercase_ascii (string v))) in
      (* TODO: might need to directly extract the string (because quotes) *)
      Promise.resolve res
    ) ~rejected:(fun _err ->
      Promise.resolve None
    )

and sendUserInput mi2 command ?(threadId=0) ?(frameLevel=0) ()
  : Promise.error option Promise.t =
  ignore threadId; ignore frameLevel;
  Promise.make @@ fun ~resolve ~reject ->
  match mi2.process with
  | Some _ ->
      mi2.tok <- mi2.tok + 1;
      mi2.handlers <- IntMap.add mi2.tok ignore mi2.handlers;
      stdin mi2 ~cb:resolve "%d%s" mi2.tok command
  | None ->
      reject (mkError "Child process not started" ())

(* NOTE: this is never rejected unless on_failure=Reject ;
   however this may be pending forever if the reply cannot be read/parsed *)
and sendCommand mi2 command ?(on_failure=Reject) () : MiNode.result Promise.t =
  Promise.make @@ fun ~resolve ~reject ->
  match mi2.process with
  | Some _ ->
      mi2.tok <- mi2.tok + 1;
      mi2.handlers <- IntMap.add mi2.tok (fun (rnode : MiNode.result) ->
          (* NOTE: node is always valid (if it can't be
             parsed, the handler can not even be called *)
          if rnode.class_ = Error && on_failure = Reject then (* default *)
            let msg = MiNode.(rfield rnode "msg" |> Value.string) in
            reject (mkError msg ~src:command ())
          else
            resolve rnode
        ) mi2.handlers;
      stdin mi2 "%d-%s" mi2.tok command
  | None ->
      reject (mkError "Child process not started" ())

and log mi2 type_ msg =
  let msg = if String.ends_with ~suffix:"\n" msg then msg else msg ^ "\n" in
  Event.(emit mi2 Message (type_, msg))

and resultIs expected p =
  Promise.then_ ~fulfilled:(fun (rnode : MiNode.result) ->
      Promise.resolve (rnode.class_ = expected)
    ) p

and mkError msg ?src () =
  let msg =
    match src with
    | None -> msg
    | Some (src) -> msg ^ " (from " ^ src ^ ")"
  in
  Ojs.obj [| "message", [%js.of: string] msg |]

and setupTerminal mi2 target : unit Promise.t =
  let is_win32 = Node.Process.platform = "win32" in
  match mi2.gdbTty with
  | Bool (false)
  | Other (_) ->
      Promise.return ()
  | Bool (true)
  | String (_) when is_win32 ->
      mi2.newConsole <- true;
      Promise.return ()
  | String ("vscode") -> (* NOTE: only on Unix-like *)
      let sleepCmd = "sleep " ^ (string_of_int mi2.sessionId) in
      let t = Vscode.Window.createTerminal ~name:"GnuCOBOL Debug Display" () in
      (* TODO: missing location argument *)
      Vscode.Terminal.sendText t ~text:"trap '' 2;" ();
      Vscode.Terminal.sendText t ~text:("clear; " ^ sleepCmd) ();
      Vscode.Terminal.show t ();
      findTtyRetry mi2 |>
      Promise.then_ ~fulfilled:(fun tty_pid_opt ->
          match tty_pid_opt with
          | Some (tty, pid) ->
              mi2.ttyName <- Some (tty);
              mi2.ttyPid <- Some (pid);
              Promise.return ()
          | None ->
              Vscode.Terminal.dispose t;
              Promise.return ()
        )
  | Bool (true)
  | String (_) ->
      let dispTarget =
        if String.length target > 50 then
          "..." ^ String.sub target (String.length target - 50) 50
        else
          target
      in
      let title = "GnuCOBOL Debug - " ^ dispTarget in
      let sleepCmd = "sleep " ^ (string_of_int mi2.sessionId) in
      let cmd, args =
        match mi2.gdbTty with
        | String ("gnome-terminal") ->
            "gnome-terminal",
            [| "--title"; title; "--";
               "bash"; "-c"; sleepCmd ^ ";" |]
        | String ("konsole") ->
            "konsole",
            [| "--title"; title;
               "--separate"; "--nofork"; "--hold";
               "-e"; sleepCmd |]
        | String ("xfce4-terminal") ->
            "xfce4-terminal",
            [| "--title"; title;
               "--font=DejaVu Sans Mono 14";
               "--command"; sleepCmd |]
        | Bool (true)
        | String ("xterm" | "external" | _) | _ ->
            "xterm",
            [| "-title"; title;
               "-fa"; "DejaVu Sans Mono"; "-fs"; "14";
               "-e"; sleepCmd ^ ";" |]
      in
      let process =
        Node.ChildProcess.(spawn_process cmd args
          ~options:(Options.create ~detached:true ~stdio:"ignore" ()) ()) in
      Node.ChildProcess.unref process |> ignore;
      (Promise.make @@ fun ~resolve ~reject ->
       Node.ChildProcess.on process (`Error (fun ~err -> reject err));
       Node.ChildProcess.on process (`Spawn resolve)) |>
      Promise.then_ ~fulfilled:(fun () -> findTtyRetry mi2) |>
      Promise.then_ ~fulfilled:(fun tty_pid_opt ->
          match tty_pid_opt with
          | Some (tty, pid) ->
              mi2.ttyName <- Some (tty);
              mi2.ttyPid <- Some (pid);
              Promise.return ()
          | None ->
              Promise.return ()
        )

and rePsLine = Str.regexp_case_fold
    {|\([^ 	]*\)[ 	]+\([0-9]+\)[ 	]+\(.*\)|}

and findTty mi2 : (string * int) option Promise.t =
  let ttyPrefix =
    if Node.Process.platform = "darwin" then "ttys"
    else "pts/"
  in
  let sleepVal = string_of_int mi2.sessionId in
  Node.ChildProcess.exec "ps -A -o tty,pid,command" |>
  Promise.then_ ~fulfilled:(fun (return : Node.ChildProcess.return) ->
      let sleepLine_opt =
        String.split_on_char '\n' return.stdout |>
        List.find_opt (fun l ->
            Util.indexOf l ("sleep " ^ sleepVal) >= 0 &&
            String.starts_with ~prefix:ttyPrefix l
          )
      in
      Option.bind sleepLine_opt (fun line ->
          if Util.matches rePsLine line then
            let tty = Util.group line 1 in
            let pid = Util.groupInt line 2 in
            Some (tty, pid)
          else
            None
        ) |>
      Promise.return
    )

and findTtyRetry mi2 =
  let rec loop tries : (string * int) option Promise.t =
    if tries <= 0 then
      Promise.return (None)
    else
      (Promise.make @@ fun ~resolve ~reject:_ ->
       Node.setTimeout (fun () ->
           findTty mi2 |>
           Promise.then_ ~fulfilled:(fun res ->
               resolve res;
               Promise.return ()
             ) |> ignore
         ) 300 |> ignore
      ) |>
      Promise.then_ ~fulfilled:(fun res ->
          match res with
          | Some (_pty) -> Promise.return res
          | None -> loop (tries - 1)
        )
  in
  loop 15
