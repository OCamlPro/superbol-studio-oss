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

(* --- *)

let single: type k. (_, _, k) mode -> 'a -> ('a, k) branches = function
  | Det_mode -> fun a -> Det a
  | Nondet_mode _ -> fun a -> Nondet (NEL.one a)

let map: type k. f:('a -> 'b) -> ('a, k) branches -> ('b, k) branches =
  fun ~f -> function
    | Det a -> Det (f a)
    | Nondet l -> Nondet (NEL.map ~f l)

let to_step_outcomes: type k. ('a, k) branches -> 'a NEL.t = function
  | Det a -> NEL.one a
  | Nondet l -> l
