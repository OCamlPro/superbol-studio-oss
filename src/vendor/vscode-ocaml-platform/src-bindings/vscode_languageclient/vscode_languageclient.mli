(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*  Copyright (c) 2019-2023 OCaml Labs                                    *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the ISC license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Interop

module RevealOutputChannelOn : sig
  type t =
    | Info
    | Warn
    | Error
    | Never

  include Js.T with type t := t
end

module ServerCapabilities : sig
  include Js.T

  val experimental : t -> Jsonoo.t option

  val create : ?experimental:Jsonoo.t -> unit -> t
end

module InitializeResult : sig
  include Js.T

  val capabilities : t -> ServerCapabilities.t

  type serverInfo =
    { name : string
    ; version : string option
    }

  val serverInfo : t -> serverInfo option
end

module DocumentFilter : sig
  include Js.T

  val language : t -> string option

  val scheme : t -> string option

  val pattern : t -> string option

  val createLanguage :
    language:string -> ?scheme:string -> ?pattern:string -> unit -> t

  val createScheme :
    ?language:string -> scheme:string -> ?pattern:string -> unit -> t

  val createPattern :
    ?language:string -> ?scheme:string -> pattern:string -> unit -> t
end

module DocumentSelector : sig
  type selector =
    [ `Filter of DocumentFilter.t
    | `String of string
    ]

  type t = selector array

  include Js.T with type t := t

  val language : ?scheme:string -> ?pattern:string -> string -> selector
end

module ClientOptions : sig
  include Js.T

  val documentSelector : t -> DocumentSelector.t option

  val outputChannel : t -> Vscode.OutputChannel.t option

  val revealOutputChannelOn : t -> RevealOutputChannelOn.t

  val create :
       ?documentSelector:DocumentSelector.t
    -> ?outputChannel:Vscode.OutputChannel.t
    -> ?revealOutputChannelOn:RevealOutputChannelOn.t
    -> unit
    -> t
end

module TransportKind : sig
  type t =
    [ `stdio
    | `ipc
    | `pipe
    | `socket
    ]
  include Js.T with type t := t
end

module Transport = TransportKind

module ExecutableOptions : sig
  include Js.T

  val cwd : t -> string option

  val env : t -> string Interop.Dict.t option

  val detached : t -> bool option

  val shell : t -> bool option

  val create :
       ?cwd:string
    -> ?env:string Interop.Dict.t
    -> ?detached:bool
    -> ?shell:bool
    -> unit
    -> t
end

module Executable : sig
  include Js.T

  val command : t -> string

  val transport : t -> Transport.t option

  val args : t -> string list option

  val options : t -> ExecutableOptions.t option

  val create :
       command:string
    -> ?transport: Transport.t
    -> ?args:string list
    -> ?options:ExecutableOptions.t
    -> unit
    -> t
end

module ForkOptions : sig
  include Js.T

  val cwd : t -> string option

  val env : t -> string Interop.Dict.t option

  val encoding : t -> string option

  val execArgv : t -> string list option

  val create :
       ?cwd:string
    -> ?env:string Interop.Dict.t
    -> ?encoding:string
    -> ?execArgv:string list
    -> unit
    -> t
end

module NodeModule : sig
  include Js.T

  val module_: t -> string

  val transport : t -> Transport.t or_undefined [@@js.get]

  val args : t -> string list option

  val runtime : t -> string option

  val options : t -> ForkOptions.t option

  val create :
    module_:string
    -> ?transport: Transport.t
    -> ?args:string list
    -> ?runtime:string
    -> ?options:ForkOptions.t
    -> unit
    -> t
end

module ServerOptions : sig
  type t =
    [ `Executable of Executable.t
    | `NodeModule of NodeModule.t
    ]

  include Js.T with type t := t
end

module InitializeParams : sig
  include Js.T
end

module ClientCapabilities : sig
  include Js.T

  val experimental : t -> Jsonoo.t or_undefined

  val set_experimental : t -> Jsonoo.t or_undefined -> unit
end

module StaticFeature : sig
  include Js.T

  val make :
       ?fillInitializeParams:(params:InitializeParams.t -> unit)
    -> fillClientCapabilities:(capabilities:ClientCapabilities.t -> unit)
    -> initialize:
         (   capabilities:ServerCapabilities.t
          -> documentSelector:DocumentSelector.t or_undefined
          -> unit)
    -> dispose:(unit -> unit)
    -> unit
    -> t
end

module DidChangeConfiguration : sig
  include Ojs.T

  val create : settings:Ojs.t -> unit -> t
end

module StreamInfo: sig
  include Js.T

  val writer : t -> Node.Net.Socket.t [@@js.get]

  val reader : t -> Node.Net.Socket.t [@@js.get]

  val detached : t -> bool option [@@js.get]

  val create :
    writer: Node.Net.Socket.t
    -> reader: Node.Net.Socket.t
    -> ?detached:bool
    -> unit
    -> t
         [@@js.builder]
end

module LanguageClient : sig
  include Js.T

  val make :
       id:string
    -> name:string
    -> serverOptions:ServerOptions.t
    -> clientOptions:ClientOptions.t
    -> ?forceDebug:bool
    -> unit
    -> t

  val from_stream :
    id:string
    -> name:string
    -> (unit -> StreamInfo.t Promise.t)
    -> t

  val start : t -> unit Promise.t

  val isRunning : t -> bool

  val stop : t -> unit Promise.t

  (* TODO: the return type should be [InitializeResult.t option] *)
  val initializeResult : t -> InitializeResult.t

  val sendRequest :
       t
    -> meth:string
    -> data:Jsonoo.t
    -> ?token:Vscode.CancellationToken.t
    -> unit
    -> Jsonoo.t Promise.t

  val registerFeature : t -> feature:StaticFeature.t -> unit

  val sendNotification : t -> string -> Ojs.t -> unit
end
