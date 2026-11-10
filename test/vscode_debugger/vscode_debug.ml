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

(** Bindings to the parts of VS Code's debug and task APIs that the vendored
    [vscode] bindings do not cover. *)

open Interop

module SourceBreakpoint = struct
  include Class.Extend (Vscode.Breakpoint) ()

  include
    [%js:
    val make:
      location:Vscode.Location.t
      -> ?enabled:bool
      -> ?condition:string
      -> ?hitCondition:string
      -> unit
      -> t
      [@@js.new "__SUPERBOL__.vscode.SourceBreakpoint"]]
end

module FunctionBreakpoint = struct
  include Class.Extend (Vscode.Breakpoint) ()

  include
    [%js:
    val make:
      functionName:string
      -> ?enabled:bool
      -> ?condition:string
      -> ?hitCondition:string
      -> unit
      -> t
      [@@js.new "__SUPERBOL__.vscode.FunctionBreakpoint"]]
end

module DebugAdapterTracker = struct
  include Interface.Make ()

  include
    [%js:
    val create: onDidSendMessage:(Vscode.DebugProtocolMessage.t -> unit) -> unit -> t
      [@@js.builder]]
end

module DebugAdapterTrackerFactory = struct
  include Interface.Make ()

  include
    [%js:
    val create:
      createDebugAdapterTracker:(Vscode.DebugSession.t -> DebugAdapterTracker.t) -> unit -> t
      [@@js.builder]]
end

module Debug = struct
  module OnDidTerminateDebugSession = Vscode.Event.Make (Vscode.DebugSession)
  module OnDidStartDebugSession = Vscode.Event.Make (Vscode.DebugSession)

  include
    [%js:
    val startDebugging:
      Vscode.WorkspaceFolder.t or_undefined -> Vscode.DebugConfiguration.t -> bool Promise.t
      [@@js.global "__SUPERBOL__.vscode.debug.startDebugging"]

    val stopDebugging: unit -> unit Promise.t
      [@@js.global "__SUPERBOL__.vscode.debug.stopDebugging"]

    val activeDebugSession: unit -> Vscode.DebugSession.t or_undefined
      [@@js.get "__SUPERBOL__.vscode.debug.activeDebugSession"]

    val breakpoints: unit -> Vscode.Breakpoint.t list
      [@@js.get "__SUPERBOL__.vscode.debug.breakpoints"]

    val addBreakpoints: Vscode.Breakpoint.t list -> unit
      [@@js.global "__SUPERBOL__.vscode.debug.addBreakpoints"]

    val removeBreakpoints: Vscode.Breakpoint.t list -> unit
      [@@js.global "__SUPERBOL__.vscode.debug.removeBreakpoints"]

    val registerDebugAdapterTrackerFactory:
      debugType:string -> factory:DebugAdapterTrackerFactory.t -> Vscode.Disposable.t
      [@@js.global "__SUPERBOL__.vscode.debug.registerDebugAdapterTrackerFactory"]

    val onDidTerminateDebugSession: OnDidTerminateDebugSession.t
      [@@js.global "__SUPERBOL__.vscode.debug.onDidTerminateDebugSession"]

    val onDidStartDebugSession: OnDidStartDebugSession.t
      [@@js.global "__SUPERBOL__.vscode.debug.onDidStartDebugSession"]]
end

(** Sending DAP requests to the debug adapter of a session.  Responses are
    the bodies of DAP responses, to decode with [DebugProtocol]. *)
module DebugSession = struct
  include
    [%js:
    val customRequest: Vscode.DebugSession.t -> string -> Ojs.t -> Ojs.t Promise.t
      [@@js.call]]

  (** Arguments of DAP requests. *)
  module Args = struct
    include
      [%js:
      val stackTrace: threadId:int -> unit -> Ojs.t [@@js.builder]
      val scopes: frameId:int -> unit -> Ojs.t [@@js.builder]
      val variables: variablesReference:int -> unit -> Ojs.t [@@js.builder]
      val evaluate: expression:string -> frameId:int -> context:string -> unit -> Ojs.t
        [@@js.builder]
      val setVariable: variablesReference:int -> name:string -> value:string -> unit -> Ojs.t
        [@@js.builder]]
  end
end

module Extension = struct
  include
    [%js:
    val activate: Vscode.Extension.t -> unit Promise.t [@@js.call]]
end

module TaskExecution = struct
  include Interface.Make ()

  include
    [%js:
    val task: t -> Vscode.Task.t [@@js.get]
    val terminate: t -> unit [@@js.call]]
end

module TaskStartEvent = struct
  include Interface.Make ()

  include
    [%js:
    val execution: t -> TaskExecution.t [@@js.get]]
end

module TaskProcessEndEvent = struct
  include Interface.Make ()

  include
    [%js:
    val execution: t -> TaskExecution.t [@@js.get]
    val exitCode: t -> int or_undefined [@@js.get]]
end

module Tasks = struct
  module OnDidStartTask = Vscode.Event.Make (TaskStartEvent)
  module OnDidEndTaskProcess = Vscode.Event.Make (TaskProcessEndEvent)

  include
    [%js:
    val onDidStartTask: OnDidStartTask.t
      [@@js.global "__SUPERBOL__.vscode.tasks.onDidStartTask"]

    val onDidEndTaskProcess: OnDidEndTaskProcess.t
      [@@js.global "__SUPERBOL__.vscode.tasks.onDidEndTaskProcess"]

    val taskExecutions: unit -> TaskExecution.t list
      [@@js.get "__SUPERBOL__.vscode.tasks.taskExecutions"]]
end

