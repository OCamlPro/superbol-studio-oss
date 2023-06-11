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

(** From: github.com/microsoft/vscode/src/typings/*.d.ts*)

open Interop

module Thenable = struct
  include Interface.Generic (Ojs) ()

  module Make (T: Js.T) = struct
    type t = T.t generic [@@js]

    type 'a tresult_or_thenable = ([
        | `TResult of 'a
        | `Thenable of 'a generic
      ]
        [@js.union])
    [@@js]

    include
      [%js:
      val then_:
           t
        -> ?onfulfilled:(value: t -> 'a tresult_or_thenable)
        -> ?onrejected:(reason: Js.Any.t -> 'a tresult_or_thenable)
        -> unit
        -> 'a generic
          [@@js.call "then"]
      val then_rej_unit:
           t
        -> ?onfulfilled:(value: t -> 'a tresult_or_thenable)
        -> ?onrejected:(reason: Js.Any.t -> unit)
        -> unit
        -> 'a generic
          [@@js.call "then"]]
  end
end
