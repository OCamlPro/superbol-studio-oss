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

open Interop

module DebugProtocol = struct

  include DebugProtocol

  module OutputEvent = struct
    include OutputEvent
    include
      [%js:
        val toEvent : t -> Event.t [@@js.cast]
        val createBody :
          ?category:M.category -> output:string
          -> ?group:M.group -> ?variablesReference:int
          -> ?source:Source.t -> ?line:int -> ?column:int
          -> ?data:Js.Any.t -> unit -> body [@@js.builder]
        val create :
          ?seq: int -> type_:string -> event:string
          -> ?body:body -> unit -> t [@@js.builder]]

    let create output category ?data () =
      let body = createBody ~category ~output ?data () in
      create ~type_:"event" ~event:M.event ~body ()
  end

  module InitializedEvent = struct
    include InitializedEvent
    include
      [%js:
        val toEvent : t -> Event.t [@@js.cast]
        val create :
          ?seq: int -> type_:string -> event:string
          -> unit -> t [@@js.builder]]

    let create () =
      create ~type_:"event" ~event:M.event ()
  end

  module StoppedEvent = struct
    include StoppedEvent
    include
      [%js:
        val toEvent : t -> Event.t [@@js.cast]
        val createBody :
          reason:reason -> ?description:string -> ?threadId:int
          -> ?preserveFocusHint:bool -> ?text:string
          -> ?allThreadsStopped:bool -> ?hitBreakpointIds:(int list)
          -> unit -> body [@@js.builder]
        val create :
          ?seq: int -> type_:string -> event:string
          -> ?body:body -> unit -> t [@@js.builder]]

    let create reason ?threadId ?exceptionText ?allThreadsStopped () =
      let body =
        createBody ~reason ?threadId
          ?text:exceptionText ?allThreadsStopped () in
      create ~type_:"event" ~event:M.event ~body ()
  end

  module ThreadEvent = struct
    include ThreadEvent
    include
      [%js:
        val toEvent : t -> Event.t [@@js.cast]
        val createBody :
          reason:M.reason -> threadId:int
          -> unit -> body [@@js.builder]
        val create :
          ?seq: int -> type_:string -> event:string
          -> ?body:body -> unit -> t [@@js.builder]]

    let create reason threadId =
      let body = createBody ~reason ~threadId () in
      create ~type_:"event" ~event:M.event ~body ()

  end

  module TerminatedEvent = struct
    include TerminatedEvent
    include
      [%js:
        val toEvent : t -> Event.t [@@js.cast]
        val createBody :
          ?restart:Js.Any.t -> unit -> body [@@js.builder]
        val create :
          ?seq: int -> type_:string -> event:string
          -> ?body:body -> unit -> t [@@js.builder]]

    let create ?restart () =
      let body = createBody ?restart () in
      create ~type_:"event" ~event:M.event ~body ()
  end

  module InvalidatedEvent = struct
    include InvalidatedEvent
    include
      [%js:
        val toEvent : t -> Event.t [@@js.cast]
        val createBody :
          ?areas:(InvalidatedAreas.t list)
          -> ?threadId:int -> ?stackFrameId:int
          -> unit -> body [@@js.builder]
        val create :
          ?seq: int -> type_:string -> event:string
          -> ?body:body -> unit -> t [@@js.builder]]

    let create ?areas ?threadId ?stackFrameId () =
      let body = createBody ?areas ?threadId ?stackFrameId () in
      create ~type_:"event" ~event:M.event ~body ()
  end



  module InitializeResponse = struct
    include InitializeResponse
    module M = struct
      include M
      include
        [%js:
          val set_supportsConfigurationDoneRequest:
            Capabilities.t -> bool -> unit [@@js.set]
          val set_supportsFunctionBreakpoints:
            Capabilities.t -> bool -> unit [@@js.set]
          val set_supportsSetVariable:
            Capabilities.t -> bool -> unit [@@js.set]
          val set_supportsGotoTargetsRequest:
            Capabilities.t -> bool -> unit [@@js.set]]
    end
    include
      [%js:
        val toResponse : t -> Response.t [@@js.cast]]
  end

  module LaunchResponse = struct
    include LaunchResponse
    include
      [%js:
        val toResponse : t -> Response.t [@@js.cast]]
  end

  module AttachResponse = struct
    include AttachResponse
    include
      [%js:
        val toResponse : t -> Response.t [@@js.cast]]
  end

  module Thread = struct
    include Thread
    include
      [%js:
        val create : id:int -> name:string -> t [@@js.builder]]
  end

  module ThreadsResponse = struct
    include ThreadsResponse
    module M = struct
      include M
      include
        [%js:
          val create: threads:Thread.t list -> body [@@js.builder]]
    end
    include
      [%js:
        val set_body: t -> body -> unit [@@js.set]
        val toResponse : t -> Response.t [@@js.cast]]
  end

  module Breakpoint = struct
    include Breakpoint
    include
      [%js:
        val create :
          ?id:int -> verified:bool -> ?message:string -> ?source:string
          -> ?line:int -> ?column:int -> ?endLine:int -> ?endColumn:int
          -> ?instructionReference:string -> ?offset:int
          -> unit -> t [@@js.builder]]
  end

  module SetFunctionBreakpointsResponse = struct
    include SetFunctionBreakpointsResponse
    module M = struct
      include M
      include
        [%js:
          val create: breakpoints:Breakpoint.t list -> body [@@js.builder]]
    end
    include
      [%js:
        val set_body: t -> body -> unit [@@js.set]
        val toResponse : t -> Response.t [@@js.cast]]
  end

  module SetBreakpointsResponse = struct
    include SetBreakpointsResponse
    module M = struct
      include M
      include
        [%js:
          val create: breakpoints:Breakpoint.t list -> body [@@js.builder]]
    end
    include
      [%js:
        val set_body: t -> body -> unit [@@js.set]
        val toResponse : t -> Response.t [@@js.cast]]
  end

  module Source = struct
    include Source
    include
      [%js:
        val create :
          (*?*)name:string -> (*?*)path:string -> sourceReference:int
          -> ?presentationHint:Source.presentation_hint -> ?origin:string
          -> ?sources:(Source.t list) -> ?adapterData:Js.Any.t
          -> ?checksums:(Checksum.t list) -> unit -> t [@@js.builder]]
  end

  module StackFrame = struct
    include StackFrame
    include
      [%js:
        val create :
          id:int -> name:string -> (*?*)source:Source.t
          -> line:int -> column:int -> ?endLine:int -> ?endColumn:int
          -> ?canRestart:bool -> ?instructionPointerReference:string
          -> ?presentationHint:presentation_hint -> unit -> t [@@js.builder]]
  end

  module StackTraceResponse = struct
    include StackTraceResponse
    module M = struct
      include M
      include
        [%js:
          val create: stackFrames:StackFrame.t list -> body [@@js.builder]]
    end
    include
      [%js:
        val set_body: t -> body -> unit [@@js.set]
        val toResponse : t -> Response.t [@@js.cast]]
  end

  module Scope = struct
    include Scope
    include
      [%js:
        val create :
          name:string -> ?presentationHint:presentation_hint
          -> variablesReference:int -> ?namedVariables:int
          -> ?indexedVariables:int -> expensive:bool
          -> ?source:Source.t -> ?line:int -> ?column:int
          -> ?endLine:int -> ?endColumn:int -> unit -> t [@@js.builder]]
  end

  module ScopesResponse = struct
    include ScopesResponse
    module M = struct
      include M
      include
        [%js:
          val create: scopes:Scope.t list -> body [@@js.builder]]
    end
    include
      [%js:
        val set_body: t -> body -> unit [@@js.set]
        val toResponse : t -> Response.t [@@js.cast]]
  end

  module Variable = struct
    include Variable
    include
      [%js:
        val create :
          name:string -> value:string -> ?type_:string
          -> ?presentationHint:VariablePresentationHint.t
          -> ?evaluateName:string -> variablesReference:int
          -> ?namedVariables:int -> ?indexedVariables:int
          -> ?memoryReference:string -> unit -> t [@@js.builder]]
  end

  module VariablesResponse = struct
    include VariablesResponse
    module M = struct
      include M
      include
        [%js:
          val create: variables:Variable.t list -> body [@@js.builder]]
    end
    include
      [%js:
        val set_body: t -> body -> unit [@@js.set]
        val toResponse : t -> Response.t [@@js.cast]]
  end

  module SetVariableResponse = struct
    include SetVariableResponse
    module M = struct
      include M
      include
        [%js:
          val create:
            value:string -> ?type_:string -> ?variablesReference:int
            -> ?namedVariables:int -> ?indexedVariables:int
            -> unit -> body [@@js.builder]]
    end
    include
      [%js:
        val set_body: t -> body -> unit [@@js.set]
        val toResponse : t -> Response.t [@@js.cast]]
  end

  module VariablePresentationHint = struct
    include VariablePresentationHint
    include
      [%js:
        val create:
          ?kind:kind -> ?attributes:(attribute list) ->
          ?visibility:visibility -> ?lazy_:bool -> unit -> t [@@js.builder]]
end

  module EvaluateResponse = struct
    include EvaluateResponse
    module M = struct
      include M
      include
        [%js:
          val create:
            result:string -> ?type_:string
            -> ?presentationHint:VariablePresentationHint.t
            -> variablesReference:int -> ?namedVariables:int
            -> ?indexedVariables:int -> ?memoryReferences:string
            -> unit -> body [@@js.builder]]
    end
    include
      [%js:
        val set_body: t -> body -> unit [@@js.set]
        val toResponse : t -> Response.t [@@js.cast]]
  end

  module GotoTarget = struct
    include GotoTarget
    include
      [%js:
        val create:
          id:int -> label:string -> line:int -> ?column:int
          -> ?endLine:int -> ?endColumn:int
          -> ?instructionPointerReference:string
          -> unit -> t [@@js.builder]]
  end

  module GotoTargetsResponse = struct
    include GotoTargetsResponse
    module M = struct
      include M
      include
        [%js:
          val create:
            targets:(GotoTarget.t list) -> unit -> body [@@js.builder]]
    end
    include
      [%js:
        val set_body: t -> body -> unit [@@js.set]
        val toResponse : t -> Response.t [@@js.cast]]
  end

end
