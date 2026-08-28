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

include Cobol_common.Srcloc.INFIX

let ( let* ) = Result.bind
let ( let+ ) = Result.map
let ( and* ) r s =
  match r, s with
  | Ok r, Ok s -> Ok (r, s)
  | Error e, Ok _ |  Ok _, Error e -> Error e
  | Error e, Error f -> Error Types.NEL.(append e f)
let ( and*^ ) r s =
  match r, s with
  | Ok r, Ok s -> Ok (r, s)
  | Error e, Ok _ |  Ok _, Error e -> Error (Types.NEL.one e)
  | Error e, Error f -> Error Types.NEL.(e :: one f)
(* let return = Result.ok *)
