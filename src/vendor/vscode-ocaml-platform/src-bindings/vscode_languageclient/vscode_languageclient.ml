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

module RevealOutputChannelOn = struct
  type t =
    | Info [@js 1]
    | Warn [@js 2]
    | Error [@js 3]
    | Never [@js 4]
  [@@js.enum] [@@js]
end

module ServerCapabilities = struct
  include Interface.Make ()

  include
    [%js:
    val experimental : t -> Jsonoo.t or_undefined [@@js.get]

    val create : ?experimental:Jsonoo.t -> unit -> t [@@js.builder]]
end

module InitializeResult = struct
  include Interface.Make ()

  type serverInfo =
    { name : string
    ; version : string or_undefined
    }
  [@@js]

  include
    [%js:
    val capabilities : t -> ServerCapabilities.t [@@js.get]

    val serverInfo : t -> serverInfo or_undefined [@@js.get]]
end

module DocumentFilter = struct
  include Interface.Make ()

  include
    [%js:
    val language : t -> string or_undefined [@@js.get]

    val scheme : t -> string or_undefined [@@js.get]

    val pattern : t -> string or_undefined [@@js.get]

    val createLanguage :
      language:string -> ?scheme:string -> ?pattern:string -> unit -> t
      [@@js.builder]

    val createScheme :
      ?language:string -> scheme:string -> ?pattern:string -> unit -> t
      [@@js.builder]

    val createPattern :
      ?language:string -> ?scheme:string -> pattern:string -> unit -> t
      [@@js.builder]]
end

module DocumentSelector = struct
  type selector =
    ([ `Filter of DocumentFilter.t
     | `String of string
     ]
    [@js.union])
  [@@js]

  let selector_of_js js_val =
    match Ojs.type_of js_val with
    | "string" -> `String ([%js.to: string] js_val)
    | _ -> `Filter ([%js.to: DocumentFilter.t] js_val)

  type t = selector array [@@js]

  let language ?(scheme = "file") ?pattern l =
    `Filter (DocumentFilter.createLanguage ~language:l ~scheme ?pattern ())
end

module ClientOptions = struct
  include Interface.Make ()

  include
    [%js:
    val documentSelector : t -> DocumentSelector.t or_undefined [@@js.get]

    val outputChannel : t -> Vscode.OutputChannel.t or_undefined [@@js.get]

    val revealOutputChannelOn : t -> RevealOutputChannelOn.t [@@js.get]

    val create :
         ?documentSelector:DocumentSelector.t
      -> ?outputChannel:Vscode.OutputChannel.t
      -> ?revealOutputChannelOn:RevealOutputChannelOn.t
      -> unit
      -> t
      [@@js.builder]]
end

module TransportKind = struct
  type t =
    [ `stdio  [@js 0]
    | `ipc    [@js 1]
    | `pipe   [@js 2]
    | `socket [@js 3]
    ] [@@js.enum] [@@js]
end

module Transport = TransportKind

module ExecutableOptions = struct
  include Interface.Make ()

  include
    [%js:
    val cwd : t -> string or_undefined [@@js.get]

    val env : t -> string Dict.t or_undefined [@@js.get]

    val detached : t -> bool or_undefined [@@js.get]

    val shell : t -> bool or_undefined [@@js.get]

    val create :
         ?cwd:string
      -> ?env:string Dict.t
      -> ?detached:bool
      -> ?shell:bool
      -> unit
      -> t
      [@@js.builder]]
end

module Executable = struct
  include Interface.Make ()

  include
    [%js:
    val command : t -> string [@@js.get]

    val transport : t -> Transport.t or_undefined [@@js.get]

    val args : t -> string list or_undefined [@@js.get]

    val options : t -> ExecutableOptions.t or_undefined [@@js.get]

    val create :
         command:string
      -> ?transport: Transport.t
      -> ?args:string list
      -> ?options:ExecutableOptions.t
      -> unit
      -> t
      [@@js.builder]]
end

module ForkOptions = struct
  include Interface.Make ()

  include
    [%js:
    val cwd : t -> string or_undefined [@@js.get]

    val env : t -> string Dict.t or_undefined [@@js.get]

    val encoding : t -> string or_undefined [@@js.get]

    val execArgv : t -> string list or_undefined [@@js.get]

    val create :
      ?cwd:string
      -> ?env:string Dict.t
      -> ?encoding:string
      -> ?execArgv:string list
      -> unit
      -> t
      [@@js.builder]]
end

module NodeModule = struct
  include Interface.Make ()

  include
    [%js:
    val module_: t -> string [@@js.get "module"]

    val transport : t -> Transport.t or_undefined [@@js.get]

    val args : t -> string list or_undefined [@@js.get]

    val runtime : t -> string or_undefined [@@js.get]

    val options : t -> ForkOptions.t or_undefined [@@js.get]

    val create :
      module_:string
      -> ?transport: Transport.t
      -> ?args:string list
      -> ?runtime:string
      -> ?options:ForkOptions.t
      -> unit
      -> t
      [@@js.builder]]
end

module ServerOptions = struct
  type t =
    ([ `Executable of Executable.t
     | `NodeModule of NodeModule.t
     ]
     [@js.union])
  [@@js]

  let t_of_js js_val =
    if Ojs.has_property js_val "module"
    then `NodeModule ([%js.to: NodeModule.t] js_val)
    else `Executable ([%js.to: Executable.t] js_val)
end

module ClientCapabilities = struct
  include Interface.Make ()

  include
    [%js:
    val experimental : t -> Jsonoo.t or_undefined [@@js.get]

    val set_experimental : t -> Jsonoo.t or_undefined -> unit [@@js.set]]
end

module InitializeParams = struct
  include Interface.Make ()
end

module StaticFeature = struct
  include Interface.Make ()

  include
    [%js:
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
      [@@js.builder]]
end

module StreamInfo = struct
  include Interface.Make ()

  include
    [%js:
    val writer : t -> Node.Net.Socket.t [@@js.get]

    val reader : t -> Node.Net.Socket.t [@@js.get]

    val detached : t -> bool option [@@js.get]

    val create :
         writer: Node.Net.Socket.t
      -> reader: Node.Net.Socket.t
      -> ?detached:bool
      -> unit
      -> t
      [@@js.builder]]
end

module DidChangeConfiguration = struct
  include Interface.Make ()

  include [%js: val create : settings:Ojs.t -> unit -> t [@@js.builder]]
end

module LanguageClient = struct
  include Class.Make ()

  include
    [%js:
    val make :
         id:string
      -> name:string
      -> serverOptions:ServerOptions.t
      -> clientOptions:ClientOptions.t
      -> ?forceDebug:bool
      -> unit
      -> t
      [@@js.new "vscode_languageclient.LanguageClient"]

    val from_stream :
         id:string
      -> name:string
      -> (unit -> StreamInfo.t Promise.t)
      -> t
      [@@js.new "vscode_languageclient.LanguageClient"]

    val start : t -> unit Promise.t [@@js.call]

    val isRunning : t -> bool [@@js.call]

    val stop : t -> unit Promise.t [@@js.call]

    val initializeResult : t -> InitializeResult.t [@@js.get]

    val sendRequest :
         t
      -> meth:string
      -> data:Jsonoo.t
      -> ?token:Vscode.CancellationToken.t
      -> unit
      -> Jsonoo.t Promise.t
      [@@js.call]

    val registerFeature : t -> feature:StaticFeature.t -> unit [@@js.call]

    val sendNotification : t -> string -> Ojs.t -> unit [@@js.call]]
end
