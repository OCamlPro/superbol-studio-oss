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

module DebugSession = struct
  include Class.Extend (Protocol.ProtocolServer) ()

  include
    [%js:
    val make:
         ?debuggerLinesAndColumnsStartAt1: bool
      -> isServer: bool
      -> unit
      -> t [@@js.new "debugadapter.DebugSession"]]
end
