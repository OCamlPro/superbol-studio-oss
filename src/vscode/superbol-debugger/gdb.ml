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
open Overrides
open DebugProtocol

module GdbLaunchArguments = struct
  open Interop
  include Interface.Extend (LaunchArguments) ()
  include LaunchArguments

  include
    [%js:
    val target : t -> string [@@js.get]
    val arguments : t -> string [@@js.get]
    val cwd : t -> string or_undefined [@@js.get]
    val group : t -> string maybe_list [@@js.get]
    val env : t -> string Dict.t [@@js.get]
    val verbose : t -> bool [@@js.get]
    val gdbtty : t -> Types.GdbTty.t [@@js.get]
    val useCobcrun : t -> bool [@@js.get]
    val cobcrunPath : t -> string [@@js.get]
    val gdbTargetWrapperPath : t -> string [@@js.get]
    val sourceDirs : t -> string maybe_list [@@js.get]]
end

module GdbAttachArguments = struct
  open Interop
  include Interface.Extend (AttachArguments) ()
  include AttachArguments

  include
    [%js:
    val pid : t -> string or_undefined [@@js.get]
    val remoteDebugger : t -> string or_undefined [@@js.get]
    val target : t -> string [@@js.get]
    val arguments : t -> string [@@js.get]
    val cwd : t -> string or_undefined [@@js.get]
    val group : t -> string maybe_list [@@js.get]
    val env : t -> string Dict.t [@@js.get]
    val verbose : t -> bool [@@js.get]
    val useCobcrun : t -> bool [@@js.get]
    val cobcrunPath : t -> string [@@js.get]
    val gdbTargetWrapperPath : t -> string [@@js.get]
    val sourceDirs : t -> string maybe_list [@@js.get]]
end

module GdbDebugSession = struct
  open Interop
  include Class.Extend (DebugAdapter.DebugSession.DebugSession) ()

  include
    [%js:
      val sendEvent : t -> event: Event.t -> unit [@@js.call]
      val sendResponse : t -> response:Response.t -> unit [@@js.call]
      val sendErrorResponse :
        t -> response:Response.t
        -> codeOrMessage:int (* | Message *)
        -> ?format:string
        -> ?variables:Js.Any.t
     (* -> dest:ErrorDestination.t *) (* User = 1, Telemetry = 2 *)
        -> unit
        -> unit [@@js.call]

      val toOjs : t -> Ojs.t [@@js.cast]
      val toVSCodeDebugAdapter : t -> Vscode.DebugAdapter.t [@@js.cast]
      val ofDebugSession :
        DebugAdapter.DebugSession.DebugSession.t -> t [@@js.cast]]

  type gdb = {
    session : t;
    miDebugger : Mi2.t;
    showDetails : bool;
    mutable needContinue : bool;
    mutable started : bool;
    mutable attached : bool;
    mutable crashed : bool;
    mutable quit : bool;
    mutable variableHandles : (int_or_string * var_cat) Handles.t;
    mutable globalVariables : DebuggerVariable.t list Promise.t IntMap.t;
  }

  and int_or_string =
    | Int of int
    | String of string

  and var_cat =
    | Local
    | Global

  let stackHandlesStart = 1000
  let varHandlesStart = 512 * 256 + stackHandlesStart

  let initLogLevel verbose =
    if verbose then
      Log.setLevel Log.Level.Debug
    else
      Log.setLevel Log.Level.Info

  let rec initializeRequest gdb iResponse _args =
    begin
      match InitializeResponse.body iResponse with
      | Some (body) ->
          InitializeResponse.M.set_supportsConfigurationDoneRequest body true;
          (* TODO: reactivate and finish this feature *)
          InitializeResponse.M.set_supportsFunctionBreakpoints body false;
          InitializeResponse.M.set_supportsSetVariable body true;
          InitializeResponse.M.set_supportsGotoTargetsRequest body true
      | None -> ()
    end;
    let response = InitializeResponse.toResponse iResponse in
    sendResponse gdb.session ~response

  and launchRequest gdb lResponse (args : GdbLaunchArguments.t) =
    let response = LaunchResponse.toResponse lResponse in
    initLogLevel (GdbLaunchArguments.verbose args);
    Mi2.init gdb.miDebugger
      (Settings.gdbPath ())
      (GdbLaunchArguments.env args)
      (GdbLaunchArguments.noDebug args |> Option.value ~default:false)
      (GdbLaunchArguments.gdbtty args)
      (GdbLaunchArguments.cobcrunPath args)
      (GdbLaunchArguments.gdbTargetWrapperPath args)
      (GdbLaunchArguments.useCobcrun args)
      (GdbLaunchArguments.sourceDirs args);
    setupEventHandlers gdb;
    let target = GdbLaunchArguments.target args in
    let cwd =
      GdbLaunchArguments.cwd args |>
      Option.value ~default:(Filename.dirname target) in
    Mi2.load gdb.miDebugger cwd target
      (GdbLaunchArguments.arguments args)
      (GdbLaunchArguments.group args) |>
    Promise.then_ ~fulfilled:(fun () ->
        sendEvent gdb.session
          ~event:InitializedEvent.(create () |> toEvent);
        Mi2.start gdb.miDebugger () |>
        Promise.then_ ~fulfilled:(fun _result ->
            gdb.started <- true; (* TODO: review this *)
            if gdb.crashed then
              handlePause gdb None;
            sendResponse gdb.session ~response;
            Promise.return ()
          ) ~rejected:(fun err ->
            sendErrorResponse gdb.session ~response ~codeOrMessage:100
              ~format:("Failed to start MI Debugger: " ^
                       (Node.JsError.message err)) ();
            Promise.return ()
          )
      ) ~rejected:(fun err ->
        sendErrorResponse gdb.session ~response ~codeOrMessage:103
          ~format:("Failed to load MI Debugger: " ^
                   (Node.JsError.message err)) ();
        Promise.return ()
      ) |> ignore

  and attachRequest gdb aResponse (args : GdbAttachArguments.t) =
    let response = AttachResponse.toResponse aResponse in
    initLogLevel (GdbAttachArguments.verbose args);
    let attachTarget =
      match GdbAttachArguments.pid args with
      | None -> GdbAttachArguments.remoteDebugger args
      | a -> a
    in
    if attachTarget = None then
      sendErrorResponse gdb.session ~response ~codeOrMessage:100
        ~format:("Failed to start MI Debugger: " ^
                 "PID or remote-debugger argument required") ()
    else
      begin
        let mi2 = gdb.miDebugger in
        Mi2.init mi2
          (Settings.gdbPath ())
          (GdbAttachArguments.env args)
          false
          (GdbTty.Bool false)
          (GdbAttachArguments.cobcrunPath args)
          (GdbAttachArguments.gdbTargetWrapperPath args)
          (GdbAttachArguments.useCobcrun args)
          (GdbAttachArguments.sourceDirs args);
        setupEventHandlers gdb;
        let target = GdbAttachArguments.target args in
        let cwd =
          GdbAttachArguments.cwd args |>
          Option.value ~default:(Filename.dirname target) in
        Mi2.attach gdb.miDebugger cwd target
          (GdbAttachArguments.group args) |>
        Promise.then_ ~fulfilled:(fun () ->
            sendEvent gdb.session
              ~event:InitializedEvent.(create () |> toEvent);
            Mi2.start gdb.miDebugger ?attachTarget () |>
            Promise.then_ ~fulfilled:(fun _result ->
                gdb.needContinue <- true; (* TODO: not sure should be here *)
                gdb.attached <- true; (* TODO: review this *)
                if gdb.crashed then
                  handlePause gdb None;
                sendResponse gdb.session ~response;
                Promise.return ()
              ) ~rejected:(fun err ->
                sendErrorResponse gdb.session ~response ~codeOrMessage:100
                  ~format:("Failed to start MI Debugger: " ^
                           (Node.JsError.message err)) ();
                Promise.return ()
              )
          ) ~rejected:(fun err ->
            sendErrorResponse gdb.session ~response ~codeOrMessage:103
              ~format:("Failed to load MI Debugger: " ^
                       (Node.JsError.message err)) ();
            Promise.return ()
          ) |> ignore
      end

  and setupEventHandlers gdb =
    let mi2 = gdb.miDebugger in
    Mi2.Event.(on mi2 Quit (fun () -> quitEvent gdb));
    Mi2.Event.(on mi2 ExitedNormally (fun _n -> quitEvent gdb));
    Mi2.Event.(on mi2 Stopped (fun n -> stopEvent gdb n));
    Mi2.Event.(on mi2 Message (fun (type_, msg) -> handleMsg gdb type_ msg));
    Mi2.Event.(on mi2 StepEnd (fun n -> handleBreak gdb n));
    Mi2.Event.(on mi2 StepOutEnd (fun n -> handleBreak gdb n));
    Mi2.Event.(on mi2 SignalStop (fun n -> handlePause gdb (Some n)));
    Mi2.Event.(on mi2 ThreadCreated (fun n -> threadCreatedEvent gdb n));
    Mi2.Event.(on mi2 ThreadExited (fun n -> threadExitedEvent gdb n))

  and handleMsg gdb type_ msg =
    let category =
      match type_ with
      | "~" -> OutputEvent.M.Console
      | "target" | "stdout" | "@" -> OutputEvent.M.Stdout
      | "log"    | "stderr" | "&" -> OutputEvent.M.Stderr
      | _                   -> OutputEvent.M.Other type_
    in
    sendEvent gdb.session
      ~event:OutputEvent.(create msg category () |> toEvent)

  and handleBreakpoint gdb (node : MiNode.async) =
    let threadId, allThreadsStopped = stoppedEventArgs node in
    sendEvent gdb.session
      ~event:StoppedEvent.(create Breakpoint
          ~threadId ~allThreadsStopped () |> toEvent)

  and handleBreak gdb (node : MiNode.async) =
    let threadId, allThreadsStopped = stoppedEventArgs node in
    sendEvent gdb.session
      ~event:StoppedEvent.(create Step
          ~threadId ~allThreadsStopped () |> toEvent)

  and handlePause gdb (node : MiNode.async option) =
    let threadId, allThreadsStopped =
      match node with
      | None -> -1, false
      | Some node -> stoppedEventArgs node
    in
    sendEvent gdb.session
      ~event:StoppedEvent.(create (Other "user request")
          ~threadId ~allThreadsStopped () |> toEvent)

  and stopEvent gdb (node : MiNode.async) =
    if not gdb.started then
      gdb.crashed <- true;
    if not gdb.quit then
      let threadId, allThreadsStopped = stoppedEventArgs node in
      sendEvent gdb.session
        ~event:StoppedEvent.(create Exception
            ~threadId ~allThreadsStopped () |> toEvent)

  and stoppedEventArgs (node : MiNode.async) =
    MiNode.(afield node "thread-id" |> Value.int,
            afield node "stopped-threads" |> Value.string = "all")

  and threadCreatedEvent gdb (node : MiNode.async) =
    let threadId = MiNode.(afield node "id" |> Value.int) in
    sendEvent gdb.session
      ~event:ThreadEvent.(create Started threadId |> toEvent)

  and threadExitedEvent gdb (node : MiNode.async) =
    let threadId = MiNode.(afield node "id" |> Value.int) in
    sendEvent gdb.session
      ~event:ThreadEvent.(create Exited threadId |> toEvent)

  and quitEvent gdb =
    if not gdb.quit then
      begin
        gdb.quit <- true;
        sendEvent gdb.session
          ~event:TerminatedEvent.(create () |> toEvent)
      end

  and disconnectRequest gdb dResponse _args =
    if gdb.attached then
      Mi2.detach gdb.miDebugger |> ignore
    else
      Mi2.stop gdb.miDebugger |> ignore;
    let response = [%js.to: Response.t]
        ([%js.of: DisconnectResponse.t] dResponse) in
    sendResponse gdb.session ~response

  (* TODO: this requires the ability to map function names to their location *)
  and setFunctionBreakPointsRequest gdb sfbResponse args =
    let response = SetFunctionBreakpointsResponse.toResponse sfbResponse in
    let bkpts = SetFunctionBreakpointsArguments.breakpoints args in
    let all = List.fold_left (fun acc bp ->
        let bp = Types.Breakpoint.{
            location = Function (FunctionBreakpoint.name bp);
            condition = FunctionBreakpoint.condition bp;
            countCondition = FunctionBreakpoint.hitCondition bp
          }
        in
        Mi2.addBreakpoint gdb.miDebugger bp :: acc
      ) [] bkpts
    in
    Promise.all_list all |>
    Promise.then_ ~fulfilled:(fun _bp_opt_list ->
        let breakpoints =
          List.map (fun _bp ->
              Breakpoint.create ~verified:true
                ~line:(-1) () (* TODO: actual location *)
            ) bkpts
        in
        let body = SetFunctionBreakpointsResponse.M.create ~breakpoints in
        SetFunctionBreakpointsResponse.set_body sfbResponse body;
        sendResponse gdb.session ~response;
        Promise.return ()
      ) ~rejected:(fun err ->
        sendErrorResponse gdb.session ~response ~codeOrMessage:8
          ~format:(Node.JsError.message err) ();
        Promise.return ()
      ) |> ignore

  (* NOTE: path is actually always defined *)
  and setBreakPointsRequest gdb sbResponse args =
    let response = SetBreakpointsResponse.toResponse sbResponse in
    let source = SetBreakpointsArguments.source args in
    let path = Source.path source |> Option.value ~default:"" in
    Mi2.clearBreakpoints gdb.miDebugger path |>
    Promise.then_ ~fulfilled:(fun _ ->
        let bkpts =
          SetBreakpointsArguments.breakpoints args |>
          Option.value ~default:[] in
        let all = List.fold_left (fun acc bp ->
            let bp = Types.Breakpoint.{
                location = FileLine {
                    file = path;
                    line = SourceBreakpoint.line bp;
                  };
                condition = SourceBreakpoint.condition bp;
                countCondition = SourceBreakpoint.hitCondition bp
              }
            in
            Mi2.addBreakpoint gdb.miDebugger bp :: acc
          ) [] bkpts
        in
        Promise.all_list all |>
        Promise.then_ ~fulfilled:(fun _bp_opt_list ->
            let breakpoints =
              List.map (fun bp ->
                  Breakpoint.create ~verified:true
                    ~line:(SourceBreakpoint.line bp) ()
                ) bkpts
            in
            let body = SetBreakpointsResponse.M.create ~breakpoints in
            SetBreakpointsResponse.set_body sbResponse body;
            sendResponse gdb.session ~response;
            Promise.return ()
          ) ~rejected:(fun err ->
            sendErrorResponse gdb.session ~response ~codeOrMessage:8
              ~format:(Node.JsError.message err) ();
            Promise.return ()
          )
      ) ~rejected:(fun err ->
        sendErrorResponse gdb.session ~response ~codeOrMessage:9
          ~format:(Node.JsError.message err) ();
        Promise.return ()
      ) |> ignore

  and threadsRequest gdb tResponse _args =
    let response = ThreadsResponse.toResponse tResponse in
    Mi2.getThreads gdb.miDebugger |>
    Promise.then_ ~fulfilled:(fun threads ->
        let threads =
          List.map (fun (t : Types.Thread.t) ->
              let name =
                string_of_int t.id ^ ":" ^ Option.value ~default:"" t.name in
              DebugProtocol.Thread.create ~id:t.id ~name
            ) threads
        in
        let body = ThreadsResponse.M.create ~threads in
        ThreadsResponse.set_body tResponse body;
        sendResponse gdb.session ~response;
        Promise.return ()
      ) ~rejected:(fun err ->
        sendErrorResponse gdb.session ~response ~codeOrMessage:13
          ~format:("Could not get threads: " ^
                   (Node.JsError.message err)) ();
        Promise.return ()
      ) |> ignore

  (* NOTE: threadId in {1,n}, frameLevel in {0,m} *)
  and encodeFrameId threadId frameLevel =
    frameLevel lsl 8 lor threadId

  (* NOTE: frameId 0 => threadId 0, frameLevel 0 (global scope) *)
  and decodeFrameId frameId =
    frameId land 0xff, frameId lsr 8

  and stackTraceRequest gdb stResponse args =
    let response = StackTraceResponse.toResponse stResponse in
    let levels = StackTraceArguments.levels args |> Option.value ~default:0 in
    let threadId = StackTraceArguments.threadId args in
    Mi2.getStack gdb.miDebugger threadId levels |>
    Promise.then_ ~fulfilled:(fun stack ->
        let stack =
          List.map (fun (elem : Stack.t) ->
              let name =
                String.trim (elem.function_ ^ " " ^ elem.lineContents) in
              let source =
                Source.create
                  ~name:(Filename.basename elem.file)
                  ~path:elem.file
                  ~sourceReference:0 ()
              in
              StackFrame.create
                ~id:(encodeFrameId threadId elem.level)
                ~name
                ~source
                ~line:elem.line
                ~column:0 ()
            ) stack
        in
        let body = StackTraceResponse.M.create ~stackFrames:stack in
        StackTraceResponse.set_body stResponse body;
        sendResponse gdb.session ~response;
        Promise.return ()
      ) ~rejected:(fun err ->
        sendErrorResponse gdb.session ~response ~codeOrMessage:12
          ~format:("Failed to get Stack Trace: " ^
                   (Node.JsError.message err)) ();
        Promise.return ()
      ) |> ignore

  and configurationDoneRequest gdb cdResponse _args =
    Mi2.Event.emit gdb.miDebugger Mi2.Event.DebugReady ();
    let response = [%js.to: Response.t]
        ([%js.of: ConfigurationDoneResponse.t] cdResponse) in
    if gdb.needContinue then
      Mi2.continue gdb.miDebugger |>
      Promise.then_ ~fulfilled:(fun _done ->
          sendResponse gdb.session ~response;
          Promise.return ()
        ) ~rejected:(fun err ->
          sendErrorResponse gdb.session ~response ~codeOrMessage:2
            ~format:("Could not continue: " ^ (Node.JsError.message err)) ();
          Promise.return ()
        ) |> ignore
    else
      sendResponse gdb.session ~response;

  (* This just prepares the VSCode scopes (Local, Globals X, Globals Y...),
     but does not populate them (although requests globals) *)
  (* TODO: move evalGlobalSymbol to VariablesRequest ? *)
  and scopesRequest gdb sResponse args =
    let response = ScopesResponse.toResponse sResponse in
    let frameId = ScopesArguments.frameId args in
    let scopes = [
      Scope.create ~name:"Local"
        ~variablesReference:(stackHandlesStart + frameId)
        ~expensive:false ()
    ] in
    Mi2.getGlobalStorageSymbols gdb.miDebugger |>
    Promise.then_ ~fulfilled:(fun (fsl : FileSymbols.t list) ->
        let (scopes, gv) =
          List.fold_left (fun (scopes, gv) (fs : FileSymbols.t) ->
              if fs.symbols <> [] then
                let base = Util.stripPathExtension fs.filename in
                let scopeId = List.length scopes in
                let scopesVariables =
                  (* NOTE: evalGlobalSymbol might return rejected promises *)
                  LocalizedSymbol.ofFileSymbols fs |>
                  List.map (fun s -> Mi2.evalGlobalSymbol gdb.miDebugger s)
                in
                let gv =
                  IntMap.add scopeId (Promise.all_list scopesVariables) gv in
                let scope =
                  Scope.create ~name:("Globals " ^ base)
                    ~variablesReference:scopeId ~expensive:false () in
                (scope :: scopes, gv)
              else
                (scopes, gv)
            ) (scopes, IntMap.empty) fsl
        in
        gdb.globalVariables <- gv;
        let scopes = List.rev scopes in
        let body = ScopesResponse.M.create ~scopes in
        ScopesResponse.set_body sResponse body;
        sendResponse gdb.session ~response;
        Promise.return ()
      ) |> ignore

  (* Retrieve a scope/frame id or variable by its VSCode reference.
     This can retrive either:
     - a scope id (global variables)
     - a frame id (local variables)
     - a variable name *)
  and lookupVariable gdb ref_ =
    if ref_ < varHandlesStart then (* < 132072 *)
      if ref_ < stackHandlesStart then (* < 1000 *)
        Int (ref_), Global (* Id: 1 - 999 *)
      else
        Int (ref_ - stackHandlesStart), Local (* Id: 1000 - 132071 *)
    else
      Handles.get ref_ gdb.variableHandles (* Id: 132072 - * *)

  and makeVarRef gdb (dv : DebuggerVariable.t) cat =
    (* NOTE: for locals, dv.cName is b_n,
       for globals it is '/path/to/prog.c.h'::b_n *)
    if gdb.showDetails || not (StrMap.is_empty dv.children) then
      Handles.set gdb.variableHandles (String dv.cName, cat)
    else
      0

  and makeVar (dv : DebuggerVariable.t) value varRef =
    Variable.create ~name:dv.cobName
      ~evaluateName:dv.cobName ~type_:dv.displayableType
      ~value:value ~variablesReference:varRef ()

  and toDebugProtocolVariable gdb (dv : DebuggerVariable.t) cat =
    let reference = makeVarRef gdb dv cat in
    let value =
      if gdb.showDetails then
        dv.value ^ " " ^ dv.displayableType
      else
        dv.value
    in
    makeVar dv value reference

  and toDebugProtocolVariableWithDetails gdb (dv : DebuggerVariable.t) =
    let vars =
      if gdb.showDetails then
        List.fold_left (fun vars (detail : VariableDetail.t) ->
            let v =
              Variable.create ~name:detail.name
                ~evaluateName:dv.cobName ~type_:detail.type_
                ~value:detail.value ~variablesReference:0 ()
            in
            v :: vars
          ) [] dv.details
      else
        []
    in
    let v =
      Variable.create ~name:"value"
        ~evaluateName:dv.cobName
        ~value:dv.value ~variablesReference:0 ()
    in
    v :: vars

  (* Retrieve the global variables for a given scope *)
  and globalStorageVariableRequest gdb vResponse scopeId =
    let response = VariablesResponse.toResponse vResponse in
    (* NOTE: should always be found, if scopesRequest is correct *)
    IntMap.find scopeId gdb.globalVariables |>
    Promise.then_ ~fulfilled:(fun scopeVariables ->
        let variables =
          List.map (fun dv ->
              toDebugProtocolVariable gdb dv Global) scopeVariables
        in
        let body = VariablesResponse.M.create ~variables in
        VariablesResponse.set_body vResponse body;
        sendResponse gdb.session ~response;
        Promise.return ()
      ) |> ignore

  (* Retrieve the local/stack variables for a given frame *)
  and localStorageVariableRequest gdb vResponse frameId =
    let response = VariablesResponse.toResponse vResponse in
    let threadId, frameLevel = decodeFrameId frameId in
    Mi2.getLocalStorageVariables gdb.miDebugger threadId frameLevel |>
    Promise.then_ ~fulfilled:(fun localVariables ->
        let variables =
          List.map (fun dv ->
              toDebugProtocolVariable gdb dv Local) localVariables
        in
        let body = VariablesResponse.M.create ~variables in
        VariablesResponse.set_body vResponse body;
        sendResponse gdb.session ~response;
        Promise.return ()
      ) |> ignore

  (* Retrive the children of a given variable *)
  and childVariableRequest gdb vResponse varName cat =
    let response = VariablesResponse.toResponse vResponse in
    let evalFunc =
      match cat with
      | Local -> (fun mi2 cName -> Mi2.evalLocalCVariable mi2 cName ())
      | Global -> Mi2.evalGlobalCVariable
    in
    evalFunc gdb.miDebugger varName |>
    Promise.then_ ~fulfilled:(fun (dv_opt : DebuggerVariable.t option) ->
        Option.fold ~none:[] ~some:(fun dv ->
            let vars_p =
              if gdb.showDetails then
                List.map Promise.resolve
                  (toDebugProtocolVariableWithDetails gdb dv)
              else
                []
            in
            StrMap.fold (fun _n (child : DebuggerVariable.t) vars_p ->
                let reference = makeVarRef gdb child cat in
                let value_p =
                  if gdb.showDetails then
                    Promise.return child.displayableType
                  else
                    evalFunc gdb.miDebugger child.cName |>
                    Promise.then_ ~fulfilled:(fun evaluatedChild_opt ->
                        Option.fold ~none:"?" ~some:(
                          fun (evaluatedChild: DebuggerVariable.t) ->
                            evaluatedChild.value) evaluatedChild_opt |>
                        Promise.return)
                in
                let v_p =
                  Promise.then_ ~fulfilled:(fun value ->
                      Promise.return (makeVar child value reference)
                    ) value_p
                in
                (v_p :: vars_p)
              ) dv.children vars_p
          ) dv_opt |>
        Promise.all_list |>
        Promise.then_ ~fulfilled:(fun variables ->
            let variables = List.rev variables in
            let body = VariablesResponse.M.create ~variables in
            VariablesResponse.set_body vResponse body;
            sendResponse gdb.session ~response;
            Promise.return ()
          )
      ) ~rejected:(fun err ->
        sendErrorResponse gdb.session ~response ~codeOrMessage:1
          ~format:("Could not expand variable: " ^
                   (Node.JsError.message err)) ();
        Promise.return ()
      ) |> ignore

  and variablesRequest gdb vResponse args =
    (* Reference to the variable or scope/frame whose child we query *)
    let varsRef = VariablesArguments.variablesReference args in
    let id, cat = lookupVariable gdb varsRef in
    match id, cat with
    | Int (scopeId), Global ->
        globalStorageVariableRequest gdb vResponse scopeId
    | Int (frameId), Local ->
        localStorageVariableRequest gdb vResponse frameId
    | String (varName), _ ->
        childVariableRequest gdb vResponse varName cat

  and setVariableRequest gdb svResponse args =
    let response = SetVariableResponse.toResponse svResponse in
    (* Get name as given by the UI (COBOL name or attribute name) *)
    let name = SetVariableArguments.name args in
    if gdb.showDetails && name <> "value" then
      sendErrorResponse gdb.session ~response ~codeOrMessage:14
        ~format:(name ^ " cannot be changed") ()
    else
      let value = SetVariableArguments.value args in
      let varsRef = SetVariableArguments.variablesReference args in
      let id, cat = lookupVariable gdb varsRef in
      begin
        match cat with
        | Local ->
            begin
              match id with
              | Int (_frameId) -> (* top / raw storage *)
                  (* NOTE: only happens when gdb.showDetails is false *)
                  (* name is COBOL name *)
                  Mi2.setLocalCobolVariable gdb.miDebugger name value
              | String (cName) when gdb.showDetails (* sub / field *) ->
                  (* name is "value", cName is short C name (b_n / f_n) *)
                  Mi2.setLocalCVariable gdb.miDebugger cName value
              | String (_cName) (* not gdb.showDrtails *) (* sub / field *) ->
                  (* name is COBOL name, cName is short C name (b_n) *)
                  Mi2.setLocalCobolVariable gdb.miDebugger name value
            end |>
            Promise.then_ ~fulfilled:(fun invalidatedAreas ->
                Promise.return (invalidatedAreas, Some (value))
              )
        | Global ->
            begin
              match id with
              | Int (scopeId) -> (* top / raw storage *)
                  (* NOTE: only happens when gdb.showDetails is false *)
                  (* NOTE: should always be found,
                     if scopesRequest is correct *)
                  IntMap.find scopeId gdb.globalVariables |>
                  Promise.then_ ~fulfilled:(fun scopeVariables ->
                      List.find_opt (fun (dv : DebuggerVariable.t) ->
                          (* name is COBOL name *)
                          dv.cobName = name) scopeVariables |>
                      Option.map (fun (dv : DebuggerVariable.t) ->
                          (* cName is full C name
                             (ie '/path/to/prog.c.h'::b_n) *)
                          dv.cName) |>
                      Promise.return
                    )
              | String (cName) when gdb.showDetails (* sub / field *) ->
                  (* name is "value", cName is full C name
                     (ie '/path/to/prog.c.h'::b_n / f_n) *)
                  Promise.return (Some (cName))
              | String (cName) (* not gdb.showDetails *) (* sub / field *) ->
                  (* name is COBOL name, cName is full C name
                     (ie '/path/to/prog.c.h'::b_n) *)
                  Mi2.lookupGlobalCobolWithCName gdb.miDebugger name cName |>
                  Option.map (fun (dv : DebuggerVariable.t) -> dv.cName) |>
                  Promise.return
            end |>
            Promise.then_ ~fulfilled:(fun cname_opt ->
                match cname_opt with
                | Some (cname) ->
                    Mi2.setGlobalCVariable gdb.miDebugger cname value |>
                    Promise.then_ ~fulfilled:(fun invalidatedAreas ->
                        Promise.return (invalidatedAreas, Some (value))
                      )
                | None ->
                    Promise.return ([], None)
              )
      end |>
      Promise.then_ ~fulfilled:(fun (invalidatedAreas, value_opt) ->
          if invalidatedAreas <> [] then
            begin
              let areas =
                List.map (fun a ->
                    InvalidatedAreas.Other a) invalidatedAreas in
              sendEvent gdb.session
                ~event:InvalidatedEvent.(create ~areas () |> toEvent)
            end;
          begin
            match value_opt with
            | Some (value) ->
                let body = SetVariableResponse.M.create ~value () in
                SetVariableResponse.set_body svResponse body;
            | None ->
                ()
          end;
          sendResponse gdb.session ~response;
          Promise.return ()
        ) ~rejected:(fun err ->
          sendErrorResponse gdb.session ~response ~codeOrMessage:11
            ~format:("Could not set " ^ name ^ ": " ^
                     (Node.JsError.message err)) ();
          Promise.return ()
        ) |> ignore

  and pauseRequest gdb pResponse _args =
    let response = [%js.to: Response.t]
        ([%js.of: PauseResponse.t] pResponse) in
    Mi2.interrupt gdb.miDebugger |>
    Promise.then_ ~fulfilled:(fun _done ->
        sendResponse gdb.session ~response;
        Promise.return ()
      ) ~rejected:(fun err ->
        sendErrorResponse gdb.session ~response ~codeOrMessage:3
          ~format:("Could not pause: " ^ (Node.JsError.message err)) ();
        Promise.return ()
      ) |> ignore

  and continueRequest gdb cResponse _args =
    let response = [%js.to: Response.t]
        ([%js.of: ContinueResponse.t] cResponse) in
    Mi2.continue gdb.miDebugger |>
    Promise.then_ ~fulfilled:(fun _done ->
        sendResponse gdb.session ~response;
        Promise.return ()
      ) ~rejected:(fun err ->
        sendErrorResponse gdb.session ~response ~codeOrMessage:2
          ~format:("Could not continue: " ^ (Node.JsError.message err)) ();
        Promise.return ()
      ) |> ignore

  and stepInRequest gdb siResponse _args =
    let response = [%js.to: Response.t]
        ([%js.of: StepInResponse.t] siResponse) in
    Mi2.stepInto gdb.miDebugger |>
    Promise.then_ ~fulfilled:(fun _done ->
        sendResponse gdb.session ~response;
        Promise.return ()
      ) ~rejected:(fun err ->
        sendErrorResponse gdb.session ~response ~codeOrMessage:4
          ~format:("Could not step in: " ^ (Node.JsError.message err)) ();
        Promise.return ()
      ) |> ignore

  and stepOutRequest gdb soResponse _args =
    let response = [%js.to: Response.t]
        ([%js.of: StepOutResponse.t] soResponse) in
    Mi2.stepOut gdb.miDebugger |>
    Promise.then_ ~fulfilled:(fun _done ->
        sendResponse gdb.session ~response;
        Promise.return ()
      ) ~rejected:(fun err ->
        sendErrorResponse gdb.session ~response ~codeOrMessage:5
          ~format:("Could not step out: " ^ (Node.JsError.message err)) ();
        Promise.return ()
      ) |> ignore

  and nextRequest gdb nResponse _args =
    let response = [%js.to: Response.t]
        ([%js.of: NextResponse.t] nResponse) in
    Mi2.stepOver gdb.miDebugger |>
    Promise.then_ ~fulfilled:(fun _done ->
        sendResponse gdb.session ~response;
        Promise.return ()
      ) ~rejected:(fun err ->
        sendErrorResponse gdb.session ~response ~codeOrMessage:6
          ~format:("Could not step over: " ^ (Node.JsError.message err)) ();
        Promise.return ()
      ) |> ignore

  and evaluateRequest gdb eResponse args =
    let response = EvaluateResponse.toResponse eResponse in
    let expression = EvaluateArguments.expression args in
    let frameId = EvaluateArguments.frameId args |> Option.value ~default:0 in
    let threadId, frameLevel = decodeFrameId frameId in
    let context_opt = EvaluateArguments.context args in
    match context_opt with
    | Some (Watch | Variables | Hover) ->
        Mi2.evalExpression gdb.miDebugger expression threadId frameLevel |>
        Promise.then_ ~fulfilled:(fun res_opt ->
            let result = Option.value ~default:"not available" res_opt in
            let presentationHint =
              VariablePresentationHint.(create ~kind:Data ()) in
            let body = EvaluateResponse.M.create
                ~variablesReference:0 ~presentationHint ~result () in
            EvaluateResponse.set_body eResponse body;
            sendResponse gdb.session ~response;
            Promise.return ()
          ) ~rejected:(fun err ->
            sendErrorResponse gdb.session ~response ~codeOrMessage:7
              ~format:(Node.JsError.message err) ();
            Promise.return ()
          ) |> ignore
    | Some (Repl | Clipboard | Other (_)) | None ->
        Mi2.sendUserInput gdb.miDebugger expression ~threadId ~frameLevel () |>
        Promise.then_ ~fulfilled:(fun err_opt ->
            let result =
              Option.fold ~none:"" ~some:Node.JsError.message err_opt in
            let body = EvaluateResponse.M.create
                ~variablesReference:0 ~result () in
            EvaluateResponse.set_body eResponse body;
            sendResponse gdb.session ~response;
            Promise.return ()
          ) ~rejected:(fun err ->
            sendErrorResponse gdb.session ~response ~codeOrMessage:8
              ~format:(Node.JsError.message err) ();
            Promise.return ()
          ) |> ignore

  (* NOTE: this actually performs the goto *)
  (* NOTE: optional values (path, name) are actually always defined *)
  and gotoTargetsRequest gdb gtResponse args =
    let response = GotoTargetsResponse.toResponse gtResponse in
    let source = GotoTargetsArguments.source args in
    let name = Source.name source |> Option.get in
    let path = Source.path source |> Option.get in
    let line = GotoTargetsArguments.line args in
    let column = GotoTargetsArguments.column args in
    Mi2.goto gdb.miDebugger path line |>
    Promise.then_ ~fulfilled:(fun _done ->
        let targets = [
          GotoTarget.create ~id:1 ~label:name ?column ~line ()
        ] in
        let body = GotoTargetsResponse.M.create ~targets () in
        GotoTargetsResponse.set_body gtResponse body;
        sendResponse gdb.session ~response;
        Promise.return ()
      ) ~rejected:(fun err ->
        sendErrorResponse gdb.session ~response ~codeOrMessage:16
          ~format:("Could not jump: " ^ (Node.JsError.message err)) ();
        Promise.return ()
      ) |> ignore

  (* NOTE: goto actually performed in gotoTargetsRequest *)
  and gotoRequest gdb gResponse _args =
    let response = [%js.to: Response.t]
        ([%js.of: GotoResponse.t] gResponse) in
    sendResponse gdb.session ~response

  let create () =
    let session =
      DebugAdapter.DebugSession.DebugSession.make ~isServer:false () in
    let gdb = {
      session = ofDebugSession session;
      miDebugger = Mi2.create ();
      showDetails = Settings.displayVariableAttributes ();
      needContinue = false;
      started = false;
      attached = false;
      crashed = false;
      quit = false;
      variableHandles = Handles.create varHandlesStart;
      globalVariables = IntMap.empty;
    }
    in
    let methods =
      let open DebugProtocol in
      [
        "initializeRequest",
        ([%js.of: (InitializeResponse.t ->
                   InitializeArguments.t -> unit)] (initializeRequest gdb));
        "launchRequest",
        ([%js.of: (LaunchResponse.t ->
                   GdbLaunchArguments.t -> unit)] (launchRequest gdb));
        "attachRequest",
        ([%js.of: (AttachResponse.t ->
                   GdbAttachArguments.t -> unit)] (attachRequest gdb));
        "disconnectRequest",
        ([%js.of: (DisconnectResponse.t ->
                   DisconnectArguments.t -> unit)] (disconnectRequest gdb));
        "setFunctionBreakPointsRequest",
        ([%js.of: (SetFunctionBreakpointsResponse.t ->
                   SetFunctionBreakpointsArguments.t -> unit)]
           (setFunctionBreakPointsRequest gdb));
        "setBreakPointsRequest",
        ([%js.of: (SetBreakpointsResponse.t ->
                   SetBreakpointsArguments.t -> unit)]
           (setBreakPointsRequest gdb));
        "threadsRequest",
        ([%js.of: (ThreadsResponse.t ->
                   Request.t -> unit)] (threadsRequest gdb));
        "stackTraceRequest",
        ([%js.of: (StackTraceResponse.t ->
                   StackTraceArguments.t -> unit)] (stackTraceRequest gdb));
        "configurationDoneRequest",
        ([%js.of: (ConfigurationDoneResponse.t ->
                   ConfigurationDoneArguments.t -> unit)]
           (configurationDoneRequest gdb));
        "scopesRequest",
        ([%js.of: (ScopesResponse.t ->
                   ScopesArguments.t -> unit)] (scopesRequest gdb));
        "variablesRequest",
        ([%js.of: (VariablesResponse.t ->
                   VariablesArguments.t -> unit)] (variablesRequest gdb));
        "setVariableRequest",
        ([%js.of: (SetVariableResponse.t ->
                   SetVariableArguments.t -> unit)] (setVariableRequest gdb));
        "pauseRequest",
        ([%js.of: (PauseResponse.t ->
                   PauseArguments.t -> unit)] (pauseRequest gdb));
        "continueRequest",
        ([%js.of: (ContinueResponse.t ->
                   ContinueArguments.t -> unit)] (continueRequest gdb));
        "stepInRequest",
        ([%js.of: (StepInResponse.t ->
                   StepInArguments.t -> unit)] (stepInRequest gdb));
        "stepOutRequest",
        ([%js.of: (StepOutResponse.t ->
                   StepOutArguments.t -> unit)] (stepOutRequest gdb));
        "nextRequest",
        ([%js.of: (NextResponse.t ->
                   NextArguments.t -> unit)] (nextRequest gdb));
        "evaluateRequest",
        ([%js.of: (EvaluateResponse.t ->
                   EvaluateArguments.t -> unit)] (evaluateRequest gdb));
        "gotoTargetsRequest",
        ([%js.of: (GotoTargetsResponse.t ->
                   GotoTargetsArguments.t -> unit)] (gotoTargetsRequest gdb));
        "gotoRequest",
        ([%js.of: (GotoResponse.t ->
                   GotoArguments.t -> unit)] (gotoRequest gdb));
      ]
    in
    List.iter (fun (name, value) ->
        Ojs.set_prop_ascii (toOjs gdb.session) name value
      ) methods;
    gdb.session

end
