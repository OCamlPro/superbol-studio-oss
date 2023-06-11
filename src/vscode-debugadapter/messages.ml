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

module Message = struct
  include Interface.Extend (DebugProtocol.ProtocolMessage) ()

  include
    [%js:
    val seq: t -> int [@@js.get]
    val type_: t -> string [@@js.get "type"]]
end

module Response = struct
  include Interface.Extend (DebugProtocol.Response) ()
  include Class.Extend (Message) ()

  include
    [%js:
    val request_seq: t -> int [@@js.get]
    val success: t -> bool [@@js.get]
    val command: t -> string [@@js.get]
    val create:
         request: DebugProtocol.Request.t
      -> ?message: string
      -> unit
      -> t
    [@@js.new "debugadapter.Response"]]
end

module Event = struct
  include Interface.Extend (DebugProtocol.Event) ()
  include Class.Extend (Message) ()

  include
    [%js:
    val event: t -> string [@@js.get]
    val create: event: string -> ?body: 'a -> unit -> t [@@js.new "debugadapter.Event"]]
end
