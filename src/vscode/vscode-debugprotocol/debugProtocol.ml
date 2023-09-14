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

open Interop

module ColumnDescriptor = struct
  include Interface.Make ()

  type type_ =
    | String [@js "string"]
    | Number [@js "number"]
    | Boolean [@js "boolean"]
    | UnixTimestampUTC [@js "unixTimestampUTC"]
  [@@js.enum] [@@js]

  include
    [%js:
    val attributeName: t -> string [@@js.get]
    val label:         t -> string [@@js.get]
    val format:        t -> string or_undefined [@@js.get]
    val type_:         t -> type_ or_undefined [@@js.get "type"]
    val width:         t -> int or_undefined [@@js.get]]
end

module Thread = struct
  include Interface.Make ()

  include
    [%js:
    val id:   t -> int [@@js.get]
    val name: t -> string [@@js.get]]
end

module ExceptionBreakpointsFilter = struct
  include Interface.Make ()

  include
    [%js:
    val filter:              t -> string [@@js.get]
    val label:               t -> string [@@js.get]
    val description:         t -> string or_undefined [@@js.get]
    val default:             t -> bool or_undefined [@@js.get]
    val supportCondition:    t -> bool or_undefined [@@js.get]
    val conditionDescrption: t -> string or_undefined [@@js.get]]
end

module ChecksumAlgorithm = struct
  type t =
    | MD5       [@js  "MD5"]
    | SHA1      [@js "SHA1"]
    | SHA256    [@js "SHA256"]
    | Timestamp [@js "timestamp"]
  [@@js.enum] [@@js]
end

module Capabilities = struct
  include Interface.Make ()

  include
    [%js:
    val supportsConfigurationDoneRequest:      t -> bool or_undefined [@@js.get]
    val supportsFunctionBreakpoints:           t -> bool or_undefined [@@js.get]
    val supportsConditionalBreakpoints:        t -> bool or_undefined [@@js.get]
    val supportsHitConditionalBreakpoints:     t -> bool or_undefined [@@js.get]
    val supportsEvaluateForHovers:             t -> bool or_undefined [@@js.get]
    val exceptionBreakpointFilters:            t -> ExceptionBreakpointsFilter.t list or_undefined [@@js.get]
    val supportsStepBack:                      t -> bool or_undefined [@@js.get]
    val supportsSetVariable:                   t -> bool or_undefined [@@js.get]
    val supportsRestartFrame:                  t -> bool or_undefined [@@js.get]
    val supportsGotoTargetsRequest:            t -> bool or_undefined [@@js.get]
    val supportsStepInTargetsRequest:          t -> bool or_undefined [@@js.get]
    val supportsCompletionsRequest:            t -> bool or_undefined [@@js.get]
    val completionTriggerCharacters:           t -> string list or_undefined [@@js.get]
    val supportsModulesRequest:                t -> bool or_undefined [@@js.get]
    val additionalModuleColumns:               t -> ColumnDescriptor.t list or_undefined [@@js.get]
    val supportedChecksumAlgorithms:           t -> ChecksumAlgorithm.t list or_undefined [@@js.get]
    val supportsRestartRequest:                t -> bool or_undefined [@@js.get]
    val supportsExceptionOptions:              t -> bool or_undefined [@@js.get]
    val supportsValueFormattingOptions:        t -> bool or_undefined [@@js.get]
    val supportsExceptionInfoRequest:          t -> bool or_undefined [@@js.get]
    val supportTerminateDebuggee:              t -> bool or_undefined [@@js.get]
    val supportSuspendDebuggee:                t -> bool or_undefined [@@js.get]
    val supportsDelayedStackTraceLoading:      t -> bool or_undefined [@@js.get]
    val supportsLoadedSourcesRequest:          t -> bool or_undefined [@@js.get]
    val supportsLogPoints:                     t -> bool or_undefined [@@js.get]
    val supportsTerminateThreadsRequest:       t -> bool or_undefined [@@js.get]
    val supportsSetExpression:                 t -> bool or_undefined [@@js.get]
    val supportsTerminateRequest:              t -> bool or_undefined [@@js.get]
    val supportsDataBreakpoints:               t -> bool or_undefined [@@js.get]
    val supportsReadMemoryRequest:             t -> bool or_undefined [@@js.get]
    val supportsWriteMemoryRequest:            t -> bool or_undefined [@@js.get]
    val supportsDisassembleRequest:            t -> bool or_undefined [@@js.get]
    val supportsCancelRequest:                 t -> bool or_undefined [@@js.get]
    val supportsBreakpointLocationsRequest:    t -> bool or_undefined [@@js.get]
    val supportsClipboardContext:              t -> bool or_undefined [@@js.get]
    val supportsSteppingGranularity:           t -> bool or_undefined [@@js.get]
    val supportsInstructionBreakpoints:        t -> bool or_undefined [@@js.get]
    val supportsExceptionFilterOptions:        t -> bool or_undefined [@@js.get]
    val supportsSingleThreadExecutionRequests: t -> bool or_undefined [@@js.get]]
end

module Message = struct
  include Interface.Make ()

  include
    [%js:
    val id:            t -> int [@@js.get]
    val format:        t -> string [@@js.get]
    val variables:     t -> string Dict.t or_undefined [@@js.get]
    val sendTelemetry: t -> bool or_undefined [@@js.get]
    val showUser:      t -> bool or_undefined [@@js.get]
    val url:           t -> string or_undefined [@@js.get]
    val urlLabel:      t -> string or_undefined [@@js.get]]
end

module Module = struct
  include Interface.Make ()

  type id =
    ([ `Int of int
     | `String of string]
    [@js.union])

  let id_of_js js_val =
    match Ojs.type_of js_val with
    | "number" -> `Int ([%js.to: int] js_val)
    | "string" -> `String ([%js.to: string] js_val)
    | _ -> assert false

  include
    [%js:
    val id:             t -> id [@@js.get]
    val name:           t -> string [@@js.get]
    val path:           t -> string or_undefined [@@js.get]
    val isOptimized:    t -> bool or_undefined [@@js.get]
    val isUserCode:     t -> bool or_undefined [@@js.get]
    val version:        t -> string or_undefined [@@js.get]
    val symbolStatus:   t -> string or_undefined [@@js.get]
    val symbolFilePath: t -> string or_undefined [@@js.get]
    val dateTimeStamp:  t -> string or_undefined [@@js.get]
    val addressRange:   t -> string or_undefined [@@js.get]]
end

module Checksum = struct

  include Interface.Make ()

  include
    [%js:
      val algorithm: t -> ChecksumAlgorithm.t [@@js.get]
      val checksum:  t -> string [@@js.get]]
end

module Source = struct
  include Interface.Make ()

  type presentation_hint =
    | Normal      [@js "normal"]
    | Emphasize   [@js "emphasize"]
    | Deemphasize [@js "deemphasize"]
  [@@js.enum] [@@js]

  include
    [%js:
    val name:             t -> string or_undefined [@@js.get]
    val path:             t -> string or_undefined [@@js.get]
    val sourceReference:  t -> int or_undefined [@@js.get]
    val presentationHint: t -> presentation_hint or_undefined [@@js.get]
    val origin:           t -> string or_undefined [@@js.get]
    val sources:          t -> t list or_undefined [@@js.get]
    val adapterDate:      t -> Js.Any.t or_undefined [@@js.get]
    val checksums:        t -> Checksum.t list or_undefined [@@js.get]]
end

module StackFrame = struct
  include Interface.Make ()

  type int_or_string = ([
      | `Int of int
      | `String of string]
      [@js.union])

  let int_or_string_of_js js_val =
    match Ojs.type_of js_val with
    | "number" -> `Int ([%js.to: int] js_val)
    | "string" -> `String ([%js.to: string] js_val)
    | _ -> assert false

  type presentation_hint =
    | Normal [@js "normal"]
    | Label [@js "label"]
    | Subtle [@js "subtle"]
  [@@js.enum] [@@js]

  include
    [%js:
      val id:                          t -> int [@@js.get]
      val name:                        t -> string [@@js.get]
      val source:                      t -> Source.t or_undefined [@@js.get]
      val line:                        t -> int [@@js.get]
      val column:                      t -> int [@@js.get]
      val endLine:                     t -> int or_undefined [@@js.get]
      val endColumn:                   t -> int or_undefined [@@js.get]
      val canRestart:                  t -> bool or_undefined [@@js.get]
      val instructionPointerReference: t -> string or_undefined [@@js.get]
      val moduleId:                    t -> int_or_string or_undefined [@@js.get]
      val presentationHint:            t -> presentation_hint [@@js.get]]
end

module Scope = struct
  include Interface.Make ()

  type presentation_hint =
    | Arguments        [@js "arguments"]
    | Locals           [@js "locals"]
    | Registers        [@js "registers"]
    | String of string [@js.default]
  [@@js.enum] [@@js]

  include
    [%js:
      val name:               t -> string [@@js.get]
      val presentationHint:   t -> presentation_hint or_undefined [@@js.get]
      val variablesReference: t -> int [@@js.get]
      val namedVariables:     t -> int or_undefined [@@js.get]
      val indexedVariables:   t -> int or_undefined [@@js.get]
      val expensive:          t -> bool [@@js.get]
      val source:             t -> Source.t or_undefined [@@js.get]
      val line:               t -> int or_undefined [@@js.get]
      val column:             t -> int or_undefined [@@js.get]
      val endLine:            t -> int or_undefined [@@js.get]
      val endColumn:          t -> int or_undefined [@@js.get]]
end

module VariablePresentationHint = struct
  include Interface.Make ()

  type kind =
    | Property         [@js "property"]
    | Method           [@js "method"]
    | Class            [@js "class"]
    | Data             [@js "data"]
    | Event            [@js "event"]
    | BaseClass        [@js "baseClass"]
    | InnerClass       [@js "innerClass"]
    | Interface        [@js "interface"]
    | MostDerivedClass [@js "mostDerivedClass"]
    | Virtual          [@js "virtual"]
    | DataBreakpoint   [@js "dataBreakpoint"]
    | Other of string  [@js.default]
  [@@js.enum] [@@js]

  type attribute =
    | Static            [@js "static"]
    | Constant          [@js "constant"]
    | ReadOnly          [@js "readOnly"]
    | RawString         [@js "rawString"]
    | HasObjectId       [@js "hasObjectId"]
    | CanHaveObjectId   [@js "canHaveObjectId"]
    | HasSideEffects    [@js "hasSideEffects"]
    | HasDataBreakpoint [@js "hasDataBreakpoint"]
    | Other of string   [@js.default]
  [@@js.enum] [@@js]

  type visibility =
    | Public          [@js "public"]
    | Private         [@js "private"]
    | Protected       [@js "protected"]
    | Internal        [@js "internal"]
    | Final           [@js "final"]
    | Other of string [@js.default]
  [@@js.enum] [@@js]

  include
    [%js:
    val kind:       t -> kind or_undefined [@@js.get]
    val attributes: t -> attribute list or_undefined [@@js.get]
    val visibility: t -> visibility or_undefined [@@js.get]
    val lazy_:      t -> bool or_undefined [@@js.get "lazy"]]
end

module Variable = struct
  include Interface.Make ()

  include
    [%js:
    val name:             t -> string [@@js.get]
    val value:            t -> string [@@js.get]
    val type_:            t -> string or_undefined [@@js.get "type"]
    val presentationHint: t -> VariablePresentationHint.t or_undefined [@@js.get]
    val namedVariables:   t -> int or_undefined [@@js.get]
    val indexedVariables: t -> int or_undefined [@@js.get]
    val memoryReference:  t -> string or_undefined [@@js.get]]
end

module BreakpointLocation = struct
  include Interface.Make ()

  include
    [%js:
    val line:      t -> int [@@js.get]
    val column:    t -> int or_undefined [@@js.get]
    val endLine:   t -> int or_undefined [@@js.get]
    val endColumn: t -> int or_undefined [@@js.get]]
end

module SourceBreakpoint = struct
  include Interface.Make ()

  include
    [%js:
    val line:         t -> int [@@js.get]
    val column:       t -> int or_undefined [@@js.get]
    val condition:    t -> string or_undefined [@@js.get]
    val hitCondition: t -> string or_undefined [@@js.get]
    val logMessage:   t -> string or_undefined [@@js.get]]
end

module FunctionBreakpoint = struct
  include Interface.Make ()

  include
    [%js:
    val name:         t -> string [@@js.get]
    val condition:    t -> string or_undefined [@@js.get]
    val hitCondition: t -> string or_undefined [@@js.get]]
end

module DataBreakpointAccessType = struct
  type t =
    | Read      [@js  "read"]
    | Write     [@js "write"]
    | ReadWrite [@js "readWrite"]
  [@@js.enum] [@@js]
end

module DataBreakpoint = struct
  include Interface.Make ()

  include
    [%js:
    val dataId:       t -> string [@@js.get]
    val accessType:   t -> DataBreakpointAccessType.t or_undefined [@@js.get]
    val condition:    t -> string or_undefined [@@js.get]
    val hitCondition: t -> string or_undefined [@@js.get]]
end

module InstructionBreakpoint = struct
  include Interface.Make ()

  include
    [%js:
    val instructionReference: t -> string [@@js.get]
    val offset:               t -> int or_undefined [@@js.get]
    val condition:            t -> string or_undefined [@@js.get]
    val hitCondition:         t -> string or_undefined [@@js.get]]
end

module Breakpoint = struct
  include Interface.Make ()

  include
    [%js:
    val id:                   t -> int or_undefined [@@js.get]
    val verified:             t -> bool [@@js.get]
    val message:              t -> string or_undefined [@@js.get]
    val source:               t -> Source.t or_undefined [@@js.get]
    val line:                 t -> int or_undefined [@@js.get]
    val column:               t -> int or_undefined [@@js.get]
    val endLine:              t -> int or_undefined [@@js.get]
    val endColumn:            t -> int or_undefined [@@js.get]
    val instructionReference: t -> string or_undefined [@@js.get]
    val offset:               t -> int or_undefined [@@js.get]]
end

module StepInTarget = struct
  include Interface.Make ()

  include
    [%js:
    val id:        t -> int [@@js.get]
    val label:     t -> string [@@js.get]
    val line:      t -> int or_undefined [@@js.get]
    val column:    t -> int or_undefined [@@js.get]
    val endLine:   t -> int or_undefined [@@js.get]
    val endColumn: t -> int or_undefined [@@js.get]]
end

module GotoTarget = struct
  include Interface.Make ()

  include
    [%js:
    val id:                          t -> int [@@js.get]
    val label:                       t -> string [@@js.get]
    val line:                        t -> int [@@js.get]
    val column:                      t -> int or_undefined [@@js.get]
    val endLine:                     t -> int or_undefined [@@js.get]
    val endColumn:                   t -> int or_undefined [@@js.get]
    val instructionPointerReference: t -> string or_undefined [@@js.get]]
end

module CompletionItemType = struct
  type t =
    | Method      [@js "method"]
    | Function    [@js "function"]
    | Constructor [@js "constructor"]
    | Field       [@js "field"]
    | Variable    [@js "variable"]
    | Class       [@js "class"]
    | Interface   [@js "interface"]
    | Module      [@js "module"]
    | Property    [@js "property"]
    | Unit        [@js "unit"]
    | Value       [@js "value"]
    | Enum        [@js "enum"]
    | Keyword     [@js "keyword"]
    | Snippet     [@js "snippet"]
    | Text        [@js "text"]
    | Color       [@js "color"]
    | File        [@js "file"]
    | Reference   [@js "reference"]
    | Customcolor [@js "customcolor"]
  [@@js.enum] [@@js]
end

module CompletionItem = struct
  include Interface.Make ()

  include
    [%js:
    val label:           t -> string [@@js.get]
    val text:            t -> string or_undefined [@@js.get]
    val sortText:        t -> string or_undefined [@@js.get]
    val detail:          t -> string or_undefined [@@js.get]
    val type_:           t -> CompletionItemType.t or_undefined [@@js.get "type"]
    val start:           t -> int or_undefined [@@js.get]
    val length:          t -> int or_undefined [@@js.get]
    val selectionStart:  t -> int or_undefined [@@js.get]
    val selectionLength: t -> int or_undefined [@@js.get]]
end

module ValueFormat = struct
  include Interface.Make ()

  include
    [%js:
    val hex: t -> bool or_undefined [@@js.get]]
end

module StackFrameFormat = struct
  include Interface.Extend (ValueFormat) ()

  include
    [%js:
    val paramters:       t -> bool or_undefined [@@js.get]
    val parameterTypes:  t -> bool or_undefined [@@js.get]
    val parameterNames:  t -> bool or_undefined [@@js.get]
    val parameterValues: t -> bool or_undefined [@@js.get]
    val line:            t -> bool or_undefined [@@js.get]
    val module_:         t -> bool or_undefined [@@js.get "module"]
    val includeAll:      t -> bool or_undefined [@@js.get]]
end

module ExceptionFilterOptions = struct
  include Interface.Make ()

  include
    [%js:
    val filterId:  t -> string [@@js.get]
    val condition: t -> string or_undefined [@@js.get]]
end

module ExceptionPathSegment = struct
  include Interface.Make ()

  include
    [%js:
    val negate: t -> bool or_undefined [@@js.get]
    val names:  t -> string list [@@js.get]]
end

module ExceptionBreakMode = struct
  type t =
    | Never         [@js "never"]
    | Always        [@js "always"]
    | Unhandled     [@js "unhandled"]
    | UserUnhandled [@js "userUnhandled"]
  [@@js.enum] [@@js]
end

module ExceptionDetails = struct
  include Interface.Make ()

  include
    [%js:
    val message:        t -> string or_undefined [@@js.get]
    val typeName:       t -> string or_undefined [@@js.get]
    val fullTypeName:   t -> string or_undefined [@@js.get]
    val evaluateName:   t -> string or_undefined [@@js.get]
    val stackTrace:     t -> string or_undefined [@@js.get]
    val innerException: t -> t or_undefined [@@js.get]]
end

module ExceptionOptions = struct
  include Interface.Make ()

  include
    [%js:
    val path:      t -> ExceptionPathSegment.t list or_undefined [@@js.get]
    val breakMode: t -> ExceptionBreakMode.t [@@js.get]]
end

module DisassembleInstruction = struct
  include Interface.Make ()

  include
    [%js:
    val address:          t -> string [@@js.get]
    val instructionBytes: t -> string or_undefined [@@js.get]
    val instruction:      t -> string [@@js.get]
    val symbol:           t -> string or_undefined [@@js.get]
    val location:         t -> Source.t or_undefined [@@js.get]
    val line:             t -> int or_undefined [@@js.get]
    val column:           t -> int or_undefined [@@js.get]
    val endLine:          t -> int or_undefined [@@js.get]
    val endColumn:        t -> int or_undefined [@@js.get]]
end

module ProtocolMessage = struct
  include Interface.Make ()

  type type_ =
    | Request  [@js "request"]
    | Response [@js "response"]
    | Event    [@js "event"]
    | Other    [@js.default]
  [@@js.enum] [@@js]

  include
    [%js:
    val seq:   t -> int [@@js.get]
    val type_: t -> type_ [@@js.get "type"]]
end

module Request = struct
  module M = struct
    include Interface.Extend (ProtocolMessage) ()

    let type_ (_:t) = ProtocolMessage.Request

    include
      [%js:
        val command:   t -> string [@@js.get]
        val arguments: t -> Js.Any.t or_undefined [@@js.get]]
  end

  module Make (Request: sig
      type arguments
      val command: string

      val arguments_of_js: Ojs.t -> arguments
      val arguments_to_js: arguments -> Ojs.t
    end) () = struct
    include Interface.Extend (M) ()

    include Request

    let command (_:t) = command

    include
      [%js:
        val arguments: t -> arguments [@@js.get]]
  end

  include M
end

module Event = struct
  module M = struct
    include Interface.Extend (ProtocolMessage) ()

    let type_ (_:t) = ProtocolMessage.Event

    include
      [%js:
        val event: t -> string [@@js.get]
        val body:  t -> Js.Any.t or_undefined [@@js.get]]
  end

  module Make (Event: sig
      type body
      val event: string

      val body_of_js: Ojs.t -> body
      val body_to_js: body -> Ojs.t
    end) () = struct
    include Interface.Extend (M) ()

    include Event

    let event (_:t) = event

    include
      [%js:
        val body: t -> body [@@js.get]]
  end

  include M
end

module Response = struct
  module M = struct
    include Interface.Extend (ProtocolMessage) ()

    let type_ (_:t) = ProtocolMessage.Response

    include
      [%js:
        val request_seq: t -> int [@@js.get]
        val success:     t -> bool [@@js.get]
        val command:     t -> string [@@js.get]
        val message:     t -> string or_undefined [@@js.get]
        val body:        t -> Js.Any.t or_undefined [@@js.get]]
  end

  module Make (Response: sig
      type body

      val body_of_js: Ojs.t -> body
      val body_to_js: body -> Ojs.t
    end) () = struct
    include Interface.Extend (M) ()

    include Response

    include
      [%js:
      val body: t -> body [@@js.get]]
  end

  include M
end

module ErrorResponse = struct
  module M = struct
    type body = {
      error: Message.t or_undefined
    } [@@js]
  end

  include Response.Make (M) ()
end

module CancelArguments = struct
  include Interface.Make ()

  include
    [%js:
    val requestId:  t -> int [@@js.get]
    val progressId: t -> string [@@js.get]]
end

module CancelRequest = struct
  module M = struct
    type arguments = CancelArguments.t [@@js]
    let command = "cancel"
  end

  include Request.Make (M) ()
end

module CancelResponse = struct
  module M = struct
    type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module InitializedEvent = struct
  module M = struct
    type body = unit [@@js]
    let event = "initialized"
  end
  
  include Event.Make (M) ()
end

module StoppedEvent = struct

  type reason =
    | Step                  [@js "step"]
    | Breakpoint            [@js "breakpoint"]
    | Exception             [@js "exception"]
    | Pause                 [@js "pause"]
    | Entry                 [@js "entry"]
    | Goto                  [@js "goto"]
    | FunctionBreakpoint    [@js "function breakpoint"]
    | DataBreakpoint        [@js "data breakpoint"]
    | InstructionBreakpoint [@js "instruction breakpoint"]
    | Other of string       [@js.default]
  [@@js.enum] [@@js]

  module M = struct
    type body = {
      reason:            reason;
      description:       string or_undefined;
      threadId:          string or_undefined;
      preserveFocusHint: string or_undefined;
      text:              string or_undefined;
      allThreadStopped:  bool or_undefined;
      hitBreakpointIds:  int list or_undefined;
    } [@@js]

    let event = "stopped"
  end

  include Event.Make (M) ()
end

module ContinuedEvent = struct
  module M = struct
    type body = {
      threadId:         int;
      allThreadStopped: bool or_undefined;
    } [@@js]

    let event = "continued"
  end

  include Event.Make (M) ()
end

module ExitedEvent = struct
  module M = struct
    type body = {
      exitCode: int;
    } [@@js]

    let event = "exited"
  end

  include Event.Make (M) ()
end

module TerminatedEvent = struct
  module M = struct
    type b = {
      restart: Js.Any.t or_undefined
    } [@@js]
    type body = b or_undefined [@@js]

    let event = "terminated"
  end

  include Event.Make (M) ()
end

module ThreadEvent = struct
  module M = struct
    type reason =
      | Started         [@js "started"]
      | Exited          [@js "exited"]
      | Other of string [@js.default]
    [@@js.enum] [@@js]

    type body = {
      reason:   reason;
      threadId: int;
    } [@@js]

    let event = "thread"
  end

  include Event.Make (M) ()
end

module OutputEvent = struct
  module M = struct
    type category =
      | Console         [@js "console"]
      | Important       [@js "important"]
      | Stdout          [@js "stdout"]
      | Stderr          [@js "stderr"]
      | Telemetry       [@js "telemetry"]
      | Other of string [@js.default]
    [@@js.enum] [@@js]

    type group =
      | Start [@js "start"]
      | StartCollapsed [@js "StartCollapsed"]
      | End [@js "end"]
    [@@js.enum] [@@js]

    type body = {
      category:           category or_undefined;
      output:             string;
      group:              group or_undefined;
      variablesReference: int or_undefined;
      source:             Source.t or_undefined;
      line:               int or_undefined;
      column:             int or_undefined;
      data:               Js.Any.t or_undefined;
    } [@@js]

    let event = "output"
  end

  include Event.Make (M) ()
end

module Reason = struct
  type t =
    | New     [@js "new"]
    | Changed [@js "changed"]
    | Removed [@js "removed"]
  [@@js.enum] [@@js]
end


module BreakpointEvent = struct
  module M = struct
    type reason =
      | Changed         [@js "changed"]
      | New             [@js "new"]
      | Removed         [@js "removed"]
      | Other of string [@js.default]
    [@@js.enum] [@@js]

    type body = {
      reason:     reason;
      breakpoint: Breakpoint.t
    } [@@js]

    let event = "breakpoint"
  end

  include Event.Make (M) ()
end

module ModuleEvent = struct
  module M = struct
    type body = {
      reason: Reason.t;
      source: Module.t;
    } [@@js]

    let event = "module"
  end

  include Event.Make (M) ()
end

module LoadedSourceEvent = struct
  module M = struct
    type body = {
      reason: Reason.t;
      source: Source.t
    } [@@js]

    let event = "loadedSource"
  end

  include Event.Make (M) ()
end

module ProcessEvent = struct
  module M = struct
    type start_method =
      | Launch [@js "launch"]
      | Attach [@js "attach"]
      | AttachForSuspendedLaunch [@js "attachForSuspendedLaunch"]
    [@@js.enum] [@@js]

    type body = {
      name:            string;
      systemProcessId: int or_undefined;
      isLocalProcess:  bool or_undefined;
      startMethod:     start_method or_undefined;
      pointerSize:     int or_undefined;
    } [@@js]

    let event = "process"
  end

  include Event.Make (M) ()
end

module CapabilitesEvent = struct
  module M = struct
    type body = {
      capabilities: Capabilities.t;
    } [@@js]

    let event = "capabilities"
  end

  include Event.Make (M) ()
end

module ProgressStartEvent = struct
  module M = struct
    type body = {
      progressId:  string;
      title:       string;
      requestId:   int or_undefined;
      cancellable: bool or_undefined;
      message:     string or_undefined;
      percentage:  int or_undefined;
    } [@@js]

    let event = "progressStart"
  end

  include Event.Make (M) ()
end

module ProgressUpdateEvent = struct
  module M = struct
    type body = {
      progressId: string;
      message:    string or_undefined;
      percentage: int or_undefined;
    } [@@js]

    let event = "progressUpdate"
  end

  include Event.Make (M) ()
end

module ProgressEndEvent = struct
  module M = struct
    type body = {
      progressId: string;
      message:    string
    } [@@js]

    let event = "progressEnd"
  end

  include Event.Make (M) ()
end

module InvalidatedAreas = struct
  type t =
    | All             [@js "all"]
    | Stacks          [@js "stacks"]
    | Threads         [@js "threads"]
    | Variables       [@js "variables"]
    | Other of string [@js.default]
  [@@js.enum] [@@js]
end

module InvalidatedEvent = struct
  module M = struct
    type body = {
      areas:        InvalidatedAreas.t list or_undefined;
      threadId:     int or_undefined;
      stackFrameId: int or_undefined;
    } [@@js]

    let event = "invalidated"
  end

  include Event.Make (M) ()
end

module MemoryEvent = struct
  module M = struct
     type body = {
       memoryReference: string;
       offset:          int;
       count:           int;
     } [@@js]

     let event = "memory"
  end

  include Event.Make (M) ()
end

module RunInTerminalArguments = struct
  include Interface.Make ()

  type kind =
    | Integrated [@js "integrated"]
    | External   [@js "external"]
  [@@js.enum] [@@js]

  include
    [%js:
    val kind:                        t -> kind or_undefined [@@js.get]
    val title:                       t -> string or_undefined [@@js.get]
    val cwd:                         t -> string [@@js.get]
    val args:                        t -> string list [@@js.get]
    val env:                         t -> string or_undefined Dict.t or_undefined [@@js.get]
    val argsCanBeInterpretedByShell: t -> bool or_undefined [@@js.get]]
end

module RunInTerminalRequest = struct
  module M = struct
    type arguments = RunInTerminalArguments.t [@@js]
    let command = "runInTerminal"
  end

  include Request.Make (M) ()
end

module RunInTerminalResponse = struct
  module M = struct
    type body = {
      processId:      string or_undefined;
      shellProcessId: string or_undefined
    } [@@js]
  end

  include Response.Make (M) ()
end

module StartDebuggingArguments = struct
  include Interface.Make ()

  type request =
    | Launch [@js "launch"]
    | Attach [@js "attach"]
  [@@js.enum] [@@js]

  include
    [%js:
    val configuration: t -> Js.Any.t Dict.t [@@js.get]
    val request:       t -> request [@@js.get]]
end

module StartDebuggingRequest = struct
  module M = struct
    type arguments = StartDebuggingArguments.t [@@js]

    let command = "startDebugging"
  end

  include Request.Make (M) ()
end

module StartDebuggingResponse = struct
   module M = struct
     type body = unit [@@js]
   end

   include Response.Make (M) ()
end

module InitializeArguments = struct
  include Interface.Make ()

  type path_format =
    | Path [@js "path"]
    | Uri [@js "uri"]
    | Other of string [@js.default]
  [@@js.enum] [@@js]

  include
    [%js:
      val clientID:                            t -> string or_undefined [@@js.get]
      val clientName:                          t -> string or_undefined [@@js.get]
      val adapterID:                           t -> string [@@js.get]
      val locale:                              t -> string or_undefined [@@js.get]
      val linesStartAt1:                       t -> bool or_undefined [@@js.get]
      val columnsStartAt1:                     t -> bool or_undefined [@@js.get]
      val pathFormat:                          t -> path_format or_undefined [@@js.get]
      val supportsVariableType:                t -> bool or_undefined [@@js.get]
      val supportsVariablePaging:              t -> bool or_undefined [@@js.get]
      val supportsRunInTerminalRequest:        t -> bool or_undefined [@@js.get]
      val supportsMemoryReferences:            t -> bool or_undefined [@@js.get]
      val supportsProgressReporting:           t -> bool or_undefined [@@js.get]
      val supportsInvalidatedEvent:            t -> bool or_undefined [@@js.get]
      val supportsMemoryEvent:                 t -> bool or_undefined [@@js.get]
      val supportsArgsCanBeInterpretedByShell: t -> bool or_undefined [@@js.get]
      val supportsStartDebuggingRequest:       t -> bool or_undefined [@@js.get]
    ]
end

module InitializeRequest = struct
  module M = struct
    type arguments = InitializeArguments.t [@@js]
    let command = "initialized"
  end

  include Request.Make (M) ()
end

module InitializeResponse = struct
  module M = struct
    type body = Capabilities.t or_undefined [@@js]
  end

  include Response.Make (M) ()
end

module ConfigurationDoneArguments = struct
  include Interface.Make ()
end

module ConfigurationDoneRequest = struct
  module M = struct
    type arguments = ConfigurationDoneArguments.t [@@js]

    let command = "configurationDone"
  end

  include Request.Make (M) ()
end

module ConfigurationDoneResponse = struct
  module M = struct
    type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module LaunchArguments = struct
  include Interface.Make ()

  include
    [%js:
    val noDebug:   t -> bool or_undefined [@@js.get]
    val __restart: t -> Js.Any.t or_undefined [@@js.get]]
end

module LaunchRequest = struct
  module M = struct
    type arguments = LaunchArguments.t [@@js]
    let command = "launch"
  end

  include Request.Make (M) ()
end

module LaunchResponse = struct
  module M = struct
     type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module AttachArguments = struct
  include Interface.Make ()

  include
    [%js:
    val __restart: t -> Js.Any.t or_undefined [@@js.get]]
end

module AttachRequest = struct
  module M = struct
    type arguments = AttachArguments.t [@@js]
    let command = "attach"
  end

  include Request.Make (M) ()
end

module AttachResponse = struct
  module M = struct
    type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module RestartArguments = struct
  include Interface.Make ()

  type arguments = ([
      | `LaunchArguments of LaunchArguments.t
      | `AttachArguments of AttachArguments.t]
      [@js.union])

  let arguments_of_js js_val =
    if Ojs.has_property js_val "noDebug" then
      `LaunchArguments ([%js.to: LaunchArguments.t] js_val)
    else
      `AttachArguments ([%js.to: AttachArguments.t] js_val)

  include
    [%js:
    val arugments: t -> arguments or_undefined [@@js.get]]
end

module RestartRequest = struct
  module M = struct
     type arguments = RestartArguments.t or_undefined [@@js]
     let command = "restart"
  end

  include Request.Make (M) ()
end

module DisconnectArguments = struct
  include Interface.Make ()

  include
    [%js:
    val restart:           t -> bool or_undefined [@@js.get]
    val terminateDebuggee: t -> bool or_undefined [@@js.get]
    val suspendDebuggee:   t -> bool or_undefined [@@js.get]]
end

module DisconnectRequest = struct
  module M = struct
    type arguments = DisconnectArguments.t or_undefined [@@js]
    let command = "disconnect"
  end

  include Request.Make (M) ()
end

module DisconnectResponse = struct
  module M = struct
    type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module TerminateArguments = struct
  include Interface.Make ()

  include
    [%js:
    val restart: t -> bool or_undefined [@@js.get]]
end

module TerminateRequest = struct
  module M = struct
    type arguments = TerminateArguments.t or_undefined [@@js]
    let command = "disconnect"
  end

  include Request.Make (M) ()
end

module TerminateResponse = struct
  module M = struct
    type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module BreakpointLocationsArguments = struct
  include Interface.Make ()

  include
    [%js:
    val source:    t -> Source.t [@@js.get]
    val line:      t -> int [@@js.get]
    val column:    t -> int or_undefined [@@js.get]
    val endLine:   t -> int or_undefined [@@js.get]
    val endColumn: t -> int or_undefined [@@js.get]]
end

module BreakpointLocationsRequest = struct
  module M = struct
    type arguments = BreakpointLocationsArguments.t or_undefined [@@js]
    let command = "breakpointLocations"
  end

  include Request.Make (M) ()
end

module BreakpointLocationsResponse = struct
  module M = struct
     type body = {
       breakpoints: BreakpointLocation.t list;
     } [@@js]
  end

  include Response.Make (M) ()
end

module SetBreakpointsArguments = struct
  include Interface.Make ()

  include
    [%js:
    val source:         t -> Source.t [@@js.get]
    val breakpoints:    t -> SourceBreakpoint.t list or_undefined [@@js.get]
    val lines:          t -> int list or_undefined [@@js.get]
    val sourceModified: t -> bool or_undefined [@@js.get]]
end

module SetBreakpointsRequest = struct
  module M = struct
    type arguments = SetBreakpointsArguments.t [@@js]
    let  command = "setBreakpoints"
  end

  include Request.Make (M) ()
end

module SetBreakpointsResponse = struct
  module M = struct
    type body = {
      breakpoints: Breakpoint.t list
    } [@@js]
  end

  include Response.Make (M) ()
end

module SetFunctionBreakpointsArguments = struct
  include Interface.Make ()

  include
    [%js:
      val breakpoints: t -> FunctionBreakpoint.t list [@@js.get]]
end

module SetFunctionBreakpointsRequest = struct
  module M = struct
    type arguments = SetFunctionBreakpointsArguments.t [@@js]
    let command = "setFunctionBreakpoints"
  end

  include Request.Make (M) ()
end

module SetFunctionBreakpointsResponse = struct
  module M = struct
    type b = {
      breakpoints: Breakpoint.t list
    } [@@js]
    type body = b or_undefined [@@js]
  end
end

module SetExceptionBreakpointsArguments = struct
  include Interface.Make ()

  include
    [%js:
    val filters:          t -> string list [@@js.get]
    val fiterOptions:     t -> ExceptionFilterOptions.t list or_undefined [@@js.get]
    val exceptionOptions: t -> ExceptionOptions.t list or_undefined [@@js.get]]
end

module SetExceptionBreakpointsRequest = struct
  module M = struct
    type arguments = SetExceptionBreakpointsArguments.t [@@js]
    let command = "setExceptionBreakpoints"
  end

  include Request.Make (M) ()
end

module SetExceptionBreakpointsResponse = struct
  module M = struct
     type b = {
       breakpoints: Breakpoint.t list or_undefined;
     } [@@js]

     type body = b or_undefined [@@js]
  end

  include Response.Make (M) ()
end

module DataBreakpointInfoArguments = struct
  include Interface.Make ()

  include
    [%js:
    val variablesReference: t -> int or_undefined [@@js.get]
    val name:               t -> string [@@js.get]
    val frameId:            t -> int or_undefined [@@js.get]]
end

module DataBreakpointInfoRequest = struct
  module M = struct
    type arguments = DataBreakpointInfoArguments.t [@@js]
    let command = "dataBreakpointInfo"
  end

  include Request.Make (M) ()
end

module DataBreakpointInfoResponse = struct
   module M = struct
     type body = {
       dataId:      string or_undefined;
       description: string;
       accessType:  DataBreakpointAccessType.t list or_undefined;
       canPersist:  bool or_undefined;
     } [@@js]
   end

   include Response.Make (M) ()
end

module SetDataBreakpointsArguments = struct
  include Interface.Make ()

  include
    [%js:
      val breakpoints: t -> DataBreakpoint.t list [@@js.get]]
end

module SetDataBreakpointsRequest = struct
  module M = struct
    type arguments = SetDataBreakpointsArguments.t [@@js]
    let command = "setDataBreakpoints"
  end

  include Request.Make (M) ()
end

module SetDataBreakpointsResponse = struct
  module M = struct
    type body = {
      breakpoints: Breakpoint.t list
    } [@@js]
  end
end

module SetInstructionBreakpointsArguments = struct
  include Interface.Make ()

  include
    [%js:
      val breakpoints: t -> InstructionBreakpoint.t list [@@js.get]]
end

module SetInstructionBreakpointsRequest = struct
  module M = struct
    type arguments = SetInstructionBreakpointsArguments.t [@@js]
    let command = "setInstructionBreakpoints"
  end

  include Request.Make (M) ()
end

module SetInstructionBreakpointsResponse = struct
  module M = struct
    type body = {
      breakpoints: Breakpoint.t list
    } [@@js]
  end
end

module ContinueArguments = struct
  include Interface.Make ()

  include
    [%js:
    val threadId:     t -> int [@@js.get]
    val singleThread: t -> bool or_undefined [@@js.get]]
end

module ContinueRequest = struct
  module M = struct
    type arguments = ContinueArguments.t [@@js]
    let command = "continue"
  end

  include Request.Make (M) ()
end

module ContinueResponse = struct
  module M = struct
    type body = {
      allThreadsContinued: bool or_undefined
    } [@@js]
  end

  include Response.Make (M) ()
end

module SteppingGranularity = struct
   type t =
     | Statement   [@js "statement"]
     | Line        [@js "line"]
     | Instruction [@js "instruction"]
   [@@js.enum] [@@js]
end

module NextArguments = struct
  include Interface.Make ()

  include
    [%js:
    val threadId:     t -> int [@@js.get]
    val singleThread: t -> bool or_undefined [@@js.get]
    val granularity:  t -> SteppingGranularity.t or_undefined [@@js.get]]
end

module NextRequest = struct
  module M = struct
    type arguments = NextArguments.t [@@js]
    let command = "next"
  end

  include Request.Make (M) ()
end

module NextResponse = struct
  module M = struct
     type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module StepInArguments = struct
  include Interface.Make ()

  include
    [%js:
    val threadId:     t -> int [@@js.get]
    val singleThread: t -> bool or_undefined [@@js.get]
    val targetId:     t -> int or_undefined [@@js.get]
    val granularity:  t -> SteppingGranularity.t or_undefined [@@js.get]]
end

module StepInRequest = struct
  module M = struct
    type arguments = StepInArguments.t [@@js]
    let command = "stepIn"
  end

  include Request.Make (M) ()
end

module StepInResponse = struct
  module M = struct
     type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module StepOutArguments = struct
  include Interface.Make ()

  include
    [%js:
    val threadId:     t -> int [@@js.get]
    val singleThread: t -> bool or_undefined [@@js.get]
    val granularity:  t -> SteppingGranularity.t or_undefined [@@js.get]]
end

module StepOutRequest = struct
  module M = struct
    type arguments = StepOutArguments.t [@@js]
    let command = "stepOut"
  end

  include Request.Make (M) ()
end

module StepOutResponse = struct
  module M = struct
     type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module StepBackArguments = struct
  include Interface.Make ()

  include
    [%js:
    val threadId:     t -> int [@@js.get]
    val singleThread: t -> bool or_undefined [@@js.get]
    val granularity:  t -> SteppingGranularity.t or_undefined [@@js.get]]
end

module StepBackRequest = struct
  module M = struct
    type arguments = StepBackArguments.t [@@js]
    let command = "stepBack"
  end

  include Request.Make (M) ()
end

module StepBackResponse = struct
  module M = struct
     type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module ReverseContinueArguments = struct
  include Interface.Make ()

  include
    [%js:
    val threadId:     t -> int [@@js.get]
    val singleThread: t -> bool or_undefined [@@js.get]]
end

module ReverseContinueRequest = struct
  module M = struct
    type arguments = ReverseContinueArguments.t [@@js]
    let command = "reverseContinue"
  end

  include Request.Make (M) ()
end

module ReverseContinueResponse = struct
  module M = struct
     type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module RestartFrameArguments = struct
  include Interface.Make ()

  include
    [%js:
    val frameId: t -> int [@@js.get]]
end

module RestartFrameRequest = struct
  module M = struct
    type arguments = RestartFrameArguments.t [@@js]
    let command = "restartFrame"
  end

  include Request.Make (M) ()
end

module RestartFrameResponse = struct
  module M = struct
     type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module GotoArguments = struct
  include Interface.Make ()

  include
    [%js:
    val threadId: t -> int [@@js.get]
    val targetId: t -> int [@@js.get]]
end

module GotoRequest = struct
  module M = struct
    type arguments = GotoArguments.t [@@js]
    let command = "goto"
  end

  include Request.Make (M) ()
end

module GotoResponse = struct
  module M = struct
     type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module PauseArguments = struct
  include Interface.Make ()

  include
    [%js:
    val threadId: t -> int [@@js.get]]
end

module PauseRequest = struct
  module M = struct
    type arguments = PauseArguments.t [@@js]
    let command = "pause"
  end

  include Request.Make (M) ()
end

module PauseResponse = struct
  module M = struct
     type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module StackTraceArguments = struct
  include Interface.Make ()

  include
    [%js:
    val threadId:   t -> int [@@js.get]
    val startFrame: t -> int or_undefined [@@js.get]
    val levels:     t -> int or_undefined [@@js.get]
    val format:     t -> StackFrameFormat.t or_undefined [@@js.get]]
end

module StackTraceRequest = struct
  module M = struct
    type arguments = StackTraceArguments.t [@@js]
    let command = "stackTrace"
  end

  include Request.Make (M) ()
end

module StackTraceResponse = struct
  module M = struct
    type body = {
      stackFrames: StackFrame.t list;
      totalFrames: int or_undefined;
    } [@@js]
  end

  include Response.Make (M) ()
end

module ScopesArguments = struct
  include Interface.Make ()

  include
    [%js:
    val frameId: t -> int [@@js.get]]
end

module ScopesRequest = struct
  module M = struct
    type arguments = ScopesArguments.t [@@js]
    let command = "scopes"
  end

  include Request.Make (M) ()
end

module ScopesResponse = struct
  module M = struct
    type body = {
      scopes: Scope.t list;
    } [@@js]
  end

  include Response.Make (M) ()
end

module VariablesArguments = struct
  include Interface.Make ()

  type filter =
    | Indexed [@js "indexed"]
    | Named   [@js "named"]
  [@@js.enum] [@@js]

  include
    [%js:
      val variablesReference: t -> int [@@js.get]
      val filter:             t -> filter or_undefined [@@js.get]
      val start:              t -> int or_undefined [@@js.get]
      val count:              t -> int or_undefined [@@js.get]
      val format:             t -> ValueFormat.t or_undefined [@@js.get]]
end

module VariablesRequest = struct
  module M = struct
    type arguments = VariablesArguments.t [@@js]
    let command = "variables"
  end

  include Request.Make (M) ()
end

module VariablesResponse = struct
  module M = struct
    type body = {
      variables: Variable.t list;
    } [@@js]
  end

  include Response.Make (M) ()
end

module SetVariableArguments = struct
  include Interface.Make ()

  include
    [%js:
      val variablesReference: t -> int [@@js.get]
      val name:               t -> string [@@js.get]
      val value:              t -> string [@@js.get]
      val format:             t -> ValueFormat.t or_undefined [@@js.get]]
end

module SetVariableRequest = struct
  module M = struct
    type arguments = SetVariableArguments.t [@@js]
    let command = "setVariable"
  end

  include Request.Make (M) ()
end

module SetVariableResponse = struct
  module M = struct
    type body = {
      value:              string;
      type_:              string or_undefined [@js "type"];
      variablesReference: int or_undefined;
      namedVariables:     int or_undefined;
      indexedVariables:   int or_undefined;
    } [@@js]
  end

  include Response.Make (M) ()
end

module SourceArguments = struct
  include Interface.Make ()

  include
    [%js:
    val source:          t -> Source.t or_undefined [@@js.get]
    val sourceReference: t -> int [@@js.get]]
end

module SourceRequest = struct
  module M = struct
    type arguments = SourceArguments.t [@@js]
    let command = "source"
  end

  include Request.Make (M) ()
end

module SourceResponse = struct
  module M = struct
    type body = {
      content: string;
      mimeType: string or_undefined;
    } [@@js]
  end

  include Response.Make (M) ()
end

module ThreadsRequest = struct
  module M = struct
    type arguments = unit [@@js]
    let command = "threads"
  end

  include Request.Make (M) ()
end

module ThreadsResponse = struct
  module M = struct
    type body = {
      threads: Thread.t list;
    } [@@js]
  end

  include Response.Make (M) ()
end

module TerminateThreadsArguments = struct
  include Interface.Make ()

  include
    [%js:
    val threadIds: t -> int list or_undefined [@@js.get]]
end

module TerminateThreadsRequest = struct
  module M = struct
    type arguments = TerminateThreadsArguments.t [@@js]
    let command = "terminateThreads"
  end

  include Request.Make (M) ()
end

module TerminateThreadsResponse = struct
  module M = struct
    type body = unit [@@js]
  end

  include Response.Make (M) ()
end

module ModulesArguments = struct
  include Interface.Make ()

  include
    [%js:
    val startModule: t -> int or_undefined [@@js.get]
    val moduleCount: t -> int or_undefined [@@js.get]]
end

module ModulesRequest = struct
  module M = struct
    type arguments = ModulesArguments.t [@@js]
    let command = "modules"
  end

  include Request.Make (M) ()
end

module ModulesResponse = struct
  module M = struct
    type body = {
      modules:      Module.t list;
      totalModules: int or_undefined;
    } [@@js]
  end

  include Response.Make (M) ()
end

module LoadedSourcesArguments = struct
  include Interface.Make ()
end

module LoadedSourcesRequest = struct
  module M = struct
    type arguments = LoadedSourcesArguments.t [@@js]
    let command = "loadedSources"
  end

  include Request.Make (M) ()
end

module LoadedSourcesResponse = struct
  module M = struct
    type body = {
      sources: Source.t list;
    } [@@js]
  end

  include Response.Make (M) ()
end

module EvaluateArguments = struct
  include Interface.Make ()

  type context =
    | Watch           [@js "watch"]
    | Repl            [@js "repl"]
    | Hover           [@js "hover"]
    | Clipboard       [@js "clipboard"]
    | Variables       [@js "variables"]
    | Other of string [@js.default]
  [@@js.enum] [@@js]

  include
    [%js:
    val expression: t -> string [@@js.get]
    val frameId:    t -> int or_undefined [@@js.get]
    val context:    t -> context or_undefined [@@js.get]
    val format:     t -> ValueFormat.t or_undefined [@@js.get]]
end

module EvaluateRequest = struct
  module M = struct
    type arguments = EvaluateArguments.t [@@js]
    let command = "evaluate"
  end

  include Request.Make (M) ()
end

module EvaluateResponse = struct
  module M = struct
    type body = {
      result:             string;
      type_:              string or_undefined [@js "type"];
      presentationHint:   VariablePresentationHint.t or_undefined;
      variablesReference: int;
      namedVariables:     int or_undefined;
      indexedVariables:   int or_undefined;
      memoryReference:    string or_undefined;
    } [@@js]
  end

  include Response.Make (M) ()
end

module SetExpressionArguments = struct
  include Interface.Make ()

  include
    [%js:
    val expression: t -> string [@@js.get]
    val value:      t -> string [@@js.get]
    val frameId:    t -> int or_undefined [@@js.get]
    val format:     t -> ValueFormat.t or_undefined [@@js.get]]
end

module SetExpressionRequest = struct
  module M = struct
    type arguments = SetExpressionArguments.t [@@js]
    let command = "setExpression"
  end

  include Request.Make (M) ()
end

module SetExpressionResponse = struct
  module M = struct
    type body = {
      value:              string;
      type_:              string or_undefined [@js "type"];
      presentationHint:   VariablePresentationHint.t or_undefined;
      variablesReference: int or_undefined;
      namedVariables:     int or_undefined;
      indexedVariables:   int or_undefined;
    } [@@js]
  end

  include Response.Make (M) ()
end

module StepInTargetsArguments = struct
  include Interface.Make ()

  include
    [%js:
    val frameId: t -> int or_undefined [@@js.get]]
end

module StepInTargetsRequest = struct
  module M = struct
    type arguments = StepInTargetsArguments.t [@@js]
    let command = "stepInTargets"
  end

  include Request.Make (M) ()
end

module StepInTargetsResponse = struct
  module M = struct
    type body = {
      targets: StepInTarget.t list;
    } [@@js]
  end

  include Response.Make (M) ()
end

module GotoTargetsArguments = struct
  include Interface.Make ()

  include
    [%js:
    val source: t -> Source.t [@@js.get]
    val line:   t -> int [@@js.get]
    val column: t -> int or_undefined [@@js.get]]
end

module GotoTargetsRequest = struct
  module M = struct
    type arguments = GotoTargetsArguments.t [@@js]
    let command = "gotoTargets"
  end

  include Request.Make (M) ()
end

module GotoTargetsResponse = struct
  module M = struct
    type body = {
      targets: GotoTarget.t list;
    } [@@js]
  end

  include Response.Make (M) ()
end

module CompletionsArguments = struct
  include Interface.Make ()

  include
    [%js:
    val frameId: t -> int or_undefined [@@js.get]
    val text:    t -> string [@@js.get]
    val column:  t -> int [@@js.get]
    val line:    t -> int or_undefined [@@js.get]]
end

module CompletionsRequest = struct
  module M = struct
    type arguments = CompletionsArguments.t [@@js]
    let command = "completions"
  end

  include Request.Make (M) ()
end

module CompletionsResponse = struct
  module M = struct
    type body = {
      targets: CompletionItem.t list;
    } [@@js]
  end

  include Response.Make (M) ()
end

module ExceptionInfoArguments = struct
  include Interface.Make ()

  include
    [%js:
    val threadId: t -> string [@@js.get]]
end

module ExceptionInfoRequest = struct
  module M = struct
    type arguments = ExceptionInfoArguments.t [@@js]
    let command = "exceptionInfo"
  end

  include Request.Make (M) ()
end

module ExceptionInfoResponse = struct
  module M = struct
    type body = {
      exceptionId: string;
      description: string or_undefined;
      breakMode:   ExceptionBreakMode.t;
      details:     ExceptionDetails.t or_undefined;
    } [@@js]
  end

  include Response.Make (M) ()
end

module ReadMemoryArguments = struct
  include Interface.Make ()

  include
    [%js:
    val memoryReference: t -> string [@@js.get]
    val offset:          t -> int or_undefined [@@js.get]
    val count:           t -> int [@@js.get]]
end

module ReadMemoryRequest = struct
  module M = struct
    type arguments = ReadMemoryArguments.t [@@js]
    let command = "readMemory"
  end

  include Request.Make (M) ()
end

module ReadMemoryResponse = struct
  module M = struct
    type b ={
      address:         string;
      unreadableBytes: int or_undefined;
      data:            string or_undefined;
    } [@@js]
    type body = b or_undefined [@@js]
  end

  include Response.Make (M) ()
end

module WriteMemoryArguments = struct
  include Interface.Make ()

  include
    [%js:
    val memoryReference: t -> string [@@js.get]
    val offset:          t -> int or_undefined [@@js.get]
    val allowPartial:    t -> bool or_undefined [@@js.get]
    val data:            t -> string [@@js.get]]
end

module WriteMemoryRequest = struct
  module M = struct
    type arguments = WriteMemoryArguments.t [@@js]
    let command = "writeMemory"
  end

  include Request.Make (M) ()
end

module WriteMemoryResponse = struct
  module M = struct
    type b = {
      offset:       int or_undefined;
      bytesWritten: int or_undefined;
    } [@@js]
    type body = b or_undefined  [@@js]
  end

  include Response.Make (M) ()
end

module DisassembleArguments = struct
  include Interface.Make ()

  include
    [%js:
    val memoryReference:   t -> string [@@js.get]
    val offset:            t -> int or_undefined [@@js.get]
    val instructionOffset: t -> int or_undefined [@@js.get]
    val instructionCount:  t -> int [@@js.get]
    val resolveSymbol:     t -> bool or_undefined [@@js.get]]
end

module DisassembleRequest = struct
  module M = struct
    type arguments = DisassembleArguments.t [@@js]
    let command = "disassemble"
  end

  include Request.Make (M) ()
end

module DisassembleResponse = struct
  module M = struct
    type b = {
      instructions: DisassembleInstruction.t list;
    } [@@js]
    type body = b or_undefined  [@@js]
  end

  include Response.Make (M) ()
end

