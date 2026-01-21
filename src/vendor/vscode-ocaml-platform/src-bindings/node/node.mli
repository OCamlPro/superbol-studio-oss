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

val __filename : unit -> string

val __dirname : unit -> string

module Timeout : sig
  include Js.T

  val hasRef : t -> bool

  val ref : t -> t

  val refresh : t -> t

  val unref : t -> t
end

val setInterval : (unit -> unit) -> int -> Timeout.t

val setTimeout : (unit -> unit) -> int -> Timeout.t

val clearTimeout : Timeout.t -> unit

module Process : sig
  val cwd : unit -> string

  val platform : string

  val arch : string

  module Env : sig
    val get : string -> string option

    val set : string -> string -> unit

    val env : string Interop.Dict.t
  end
end

module JsError : sig
  include Js.T with type t = Promise.error

  val message : t -> string
end

module Buffer : sig
  include Js.T

  val toString : t -> string

  val from : string -> t

  val concat : t list -> t

  val append : t ref -> t -> unit

  val write :
       t
    -> string:string
    -> ?offset:int
    -> ?length:int
    -> ?encoding:string
    -> unit
    -> unit
end

module Stream : sig
  module Readable : sig
    include Js.T

    type chunk =
      [ `String of string
      | `Buffer of Buffer.t
      ]

    val on :
         t
      -> [ `Close of unit -> unit
         | `Data of chunk:chunk -> unit
         | `End of unit -> unit
         | `Error of err:JsError.t -> unit
         | `Pause of unit -> unit
         | `Readable of unit -> unit
         | `Resume of unit -> unit
         ]
      -> unit
  end

  module Writable : sig
    include Js.T

    val on :
         t
      -> [ `Close of unit -> unit
         | `Drain of unit -> unit
         | `Error of err:JsError.t -> unit
         | `Finish of unit -> unit
         | `Pipe of src:t -> unit
         | `Unpipe of src:t -> unit
         ]
      -> unit

    val write : t -> string -> unit

    val end_ : t -> unit
  end
end

module Path : sig
  val delimiter : char

  val sep : char

  val basename : string -> string

  val dirname : string -> string

  val extname : string -> string

  val isAbsolute : string -> bool

  val join : string list -> string
end

module Os : sig
  val homedir : unit -> string
end

module Fs : sig
  val readDir : string -> (string list, string) result Promise.t

  val readFile : string -> string Promise.t

  val exists : string -> bool Promise.t

  val existsSync : string -> bool
end

module Net : sig
  module Socket : sig
    type t

    val t_of_js : Ojs.t -> t
    val t_to_js : t -> Ojs.t

    val make : unit -> t

    val isPaused : t -> bool

    val destroy : t -> unit

    val connect : t -> port:int -> host:string -> t

    val setTimeout : t -> int -> t

    val on :
         t
      -> [ `Connect of unit -> unit
         | `Timeout of unit -> unit
         | `Error of err:JsError.t -> unit
         ]
      -> unit
  end
end

module ChildProcess : sig
  type t

  module Options : sig
    type t

    val create : ?cwd:string -> ?env:string Interop.Dict.t -> unit -> t
  end

  type return =
    { exitCode : int
    ; stdout : string
    ; stderr : string
    }

  type event =
    | Spawned
    | Stdout of string
    | Stderr of string
    | Closed
    | ProcessError of JsError.t

  (** Low-level API for long-running processes *)

  val spawn_process : string -> string array -> ?options:Options.t -> unit -> t

  val get_stdout : t -> Stream.Readable.t

  val get_stderr : t -> Stream.Readable.t

  val kill : t -> ?signal:string -> unit -> unit

  val on :
       t
    -> [ `Close of code:int -> ?signal:string -> unit -> unit
       | `Disconnect of unit -> unit
       | `Error of err:JsError.t -> unit
       | `Exit of code:int -> signal:string -> unit
       ]
    -> unit

  (** High-level API that waits for process to complete *)

  val exec :
       ?logger:(event -> unit)
    -> ?stdin:string
    -> ?options:Options.t
    -> string
    -> return Promise.t

  val spawn :
       ?logger:(event -> unit)
    -> ?stdin:string
    -> ?options:Options.t
    -> string
    -> string array
    -> return Promise.t
end

module Events : sig
  module EventEmitterOptions : sig
    include Js.T

    val captureRejections: t -> bool or_undefined
  end

  module EventEmitter : sig
    include Js.T

    type listener =
      args:(Js.Any.t list [@js.variadic])
      -> unit

    val create: ?options: EventEmitterOptions.t -> unit -> t
    val addListener: t -> eventName: string -> listener: listener -> t
    val on: t -> eventName: string -> listener: listener -> t
    val once: t -> eventName: string -> listener: listener -> t
    val removeListener: t -> eventName: string -> listener: listener -> t
    val off: t -> eventName: string -> listener: listener -> t
    val removeAllListeners: t -> ?event: string -> unit -> t
    val setMaxListener: t -> n: int -> t
    val getMaxListener: t -> int
    val listeners: t -> eventName: string -> listener list
    val rawListeners: t -> eventName: string -> listener list
    val emit: t -> eventName: string -> args:(Js.Any.t list [@js.variadic]) -> bool
    val listenerCount: t -> eventName: string -> ?listener: listener -> unit -> int
    val prependListener: t -> eventName: string -> listener: listener -> t
    val prependOnceListener: t -> eventName: string -> listener: listener -> t
    val eventNames: t -> string list
  end
end

module Console : sig
  val log : string -> unit
  val warn : string -> unit
  val error : string -> unit
end
