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

open Cir_logic.Syntax

let status_field i =
  Result.get_ok @@                     (* note: always fits in a binary field *)
  Field.int32_field ~digits:10 i

let stop ~vm:_ ?status state =
  match status with
  | None ->
      ok (state, status_field 0)
  | Some f ->
      let+ status = Field.as_int f in
      state, status_field status

let decide ~vm:_ c state =
  ok @@ Det (state, c)

(** Values of the counters of [PERFORM ... TIMES] statements. *)
let counters: (Cir_types.counter, int) Hashtbl.t = Hashtbl.create 7

let set_counter ~vm:_ counter f state =
  let+ i = Field.as_int f in
  Hashtbl.replace counters counter i;
  state

let count_down ~vm:_ counter state =
  match Hashtbl.find_opt counters counter with
  | Some n when n > 0 ->
      Hashtbl.replace counters counter (pred n);
      ok @@ Det (state, true)
  | _ ->
      ok @@ Det (state, false)

let decide_int_value ~vm f ~bounds state =
  let+ state, i = Field.int_value ~vm f ~bounds state in
  Det (state, i)
