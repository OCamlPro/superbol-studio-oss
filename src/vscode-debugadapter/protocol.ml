(**************************************************************************)
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

module DebugProtocalMessage = struct
  include Interface.Make ()
end

module IDisposable = struct
  include Interface.Make ()

  include
    [%js:
    val dispose: t -> unit [@@js.call]]
end

module Disposable0 = struct
  include Interface.Extend (IDisposable) ()

  include
    [%js:
      val dispose: t -> Js.Any.t [@@js.call]]
end

module Event0 = struct
  type 'a t =
       listener: (e: 'a -> Js.Any.t)
    -> ?thisArgs: Js.Any.t
    -> unit
    -> Disposable0.t
  [@@js]

  module Make (T: Js.T) = struct
    type t =
        listener: (e: T.t -> Js.Any.t)
      -> ?thisArgs: Js.Any.t
      -> unit
      -> Disposable0.t
    [@@js]
  end
end

module Emitter = struct
  include Class.Generic (Ojs) ()

  module Make (T: Js.T) = struct
    type t = T.t generic [@@js]

    module Event = Event0.Make (T)

    include
      [%js:
      val event: t -> Event.t [@@js.get]
      val fire: t -> event:T.t -> t [@@js.call]
      val hasListener: t -> bool [@@js.call]
      val dispose: t -> unit [@@js.call]]
  end
end

module VSCodeDebugAdapter = struct
  include Interface.Extend (Disposable0) ()

  include
    [%js:
    val onDidSendMessage: t -> DebugProtocalMessage.t Event0.t [@@js.get]
    val handleMessage: t -> message: DebugProtocol.ProtocolMessage.t -> unit [@@js.call]]
end

module ProtocolServer = struct
  include Class.Extend (Node.Events.EventEmitter) ()
  include Class.Extend (VSCodeDebugAdapter) ()

  include
    [%js:
    val dispose: t -> Js.Any.t [@@js.call]
    val onDidSendMessage: t -> DebugProtocalMessage.t Event0.t [@@js.get]
    val handleMessage: t -> msg: DebugProtocol.ProtocolMessage.t -> unit [@@js.call]
    val start:
         t
      -> inStream: Node.Stream.Readable.t
      -> outStream: Node.Stream.Writable.t
      -> unit [@@js.call]
    val stop: t -> unit [@@js.call]
    val sendEvent: t -> event: DebugProtocol.Event.t -> unit [@@js.call]
    val sendResponse: t -> response: DebugProtocol.Response.t -> unit [@@js.call]
    val sendRequest:
         t
      -> command: string
      -> args: 'a
      -> timeout: int
      -> cb: (response: DebugProtocol.Response.t -> unit)
      -> unit [@@js.call]
    val dispatchRequest: t -> request: DebugProtocol.Request.t -> unit [@@js.call]]
end
