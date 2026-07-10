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

module LIST = Cobol_common.Basics.LIST

let fatal e =
  raise @@ Types.FATAL e

let cons_error r acc =
  match r, acc with
  | Ok r, Ok acc ->
      Ok (r :: acc)
  | Ok _, Error _ ->
      acc
  | Error e, Ok _ ->
      Error (Types.NEL.one e)
  | Error e, Error e' ->
      Error (e :: e')

let cons_errors r acc =
  match r, acc with
  | Ok r, Ok acc ->
      Ok (r :: acc)
  | Ok _, acc ->
      acc
  | Error e, Ok _ ->
      Error e
  | Error e, Error e' ->
      Error (Types.NEL.append e e')

let append_lists r acc =
  match r, acc with
  | Ok r, Ok acc ->
      Ok (LIST.append r acc)
  | Ok _, acc ->
      acc
  | Error e, Ok _ ->
      Error e
  | Error e, Error e' ->
      Error (Types.NEL.append e e')

let acc_error r acc =
  match r, acc with
  | Ok (), Ok () ->
      Ok ()
  | Ok (), acc ->
      acc
  | Error e, Ok () ->
      Error (Types.NEL.one e)
  | Error e, Error e' ->
      Error (e :: e')

let acc_errors r s =
  match r, s with
  | Ok (), Ok () ->
      Ok ()
  | Ok (), x | x, Ok () ->
      x
  | Error e, Error e' ->
      Error (Types.NEL.append e e')
