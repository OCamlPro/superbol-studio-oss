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

open Types

module NEL = Cobol_common.Basics.NEL

let ok x = Ok x
let errors e = Error e
let error e = errors (NEL.one e)
let lift_ezlibcob_build_error = function
  | Ok _ as x -> x
  | Error e -> error (Ezlibcob_build_error e)
let lift_ezlibcob_runtime_error s = function
  | Ok x -> Ok (s, x)
  | Error e -> error (Ezlibcob_runtime_error e)
