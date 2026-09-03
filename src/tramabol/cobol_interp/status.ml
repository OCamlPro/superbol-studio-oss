(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Cir_logic.Types
open Types

module NEL = Cobol_common.Basics.NEL

let ok x = Ok x
let build_error e = Error (NEL.one e)
let lift_ezlibcob_build_error ?loc = function
  | Ok _ as x -> x
  | Error e -> build_error @@ Ezlibcob_build_error { loc; error = e }
let runtime_error ?loc error = Error (NEL.one { loc; error })
let lift_ezlibcob_runtime_error ?loc = function
  | Ok x -> Ok x
  | Error e -> runtime_error ?loc @@ Ezlibcob_runtime_error e

let union a b =
  match a, b with
  | Ok (), e | e, Ok () ->
      e
  | Error e, Error e' ->
      Error (NEL.append e' e)
