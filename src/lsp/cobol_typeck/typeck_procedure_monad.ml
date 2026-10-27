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

(** Typing computations that collect every diagnostic and resolved reference.
    Computations never fail: ill-typed parts of the typed tree are represented
    with {!Cobol_procedure.Types.or_error}, and the reason is reported in the
    diagnostics. *)

module NEL = Cobol_common.Basics.NEL

(** Read-only typing environment *)
type env = { data_definitions: Cobol_unit.Types.data_definitions }

type acc = Typeck_outputs.references_acc

type 'a t = env -> acc -> 'a * acc

(** [return ~diags x] gives [x], after adding the diagnostics [diags] (in
    order). *)
let return ?(diags = []) x : _ t =
  fun _ acc -> x, { acc with diags = List.rev_append diags acc.diags }

(** Adds the given diagnostic. *)
let report d : unit t =
  fun _ acc -> (), { acc with diags = d :: acc.diags }

let ( let+ ) (m : _ t) f : _ t =
  fun env acc ->
  let r, acc = m env acc in
  f r, acc

let ( and+ ) (m : _ t) (n : _ t) : _ t =
  fun env acc ->
  let a, acc = m env acc in
  let b, acc = n env acc in
  (a, b), acc

let ( let* ) (m : _ t) (f : _ -> _ t) : _ t =
  fun env acc ->
  let x, acc = m env acc in
  f x env acc

let rec map_list f = function
  | [] -> return []
  | x :: l ->
      let+ y = f x
      and+ l = map_list f l in
      y :: l

let rec map_nel f : _ NEL.t -> _ t = function
  | One x ->
      let+ y = f x in
      NEL.One y
  | x :: l ->
      let+ y = f x
      and+ l = map_nel f l in
      NEL.(y :: l)

let map_option f = function
  | None -> return None
  | Some x ->
      let+ y = f x in
      Some y
