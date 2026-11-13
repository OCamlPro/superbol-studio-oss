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

(** {2 Iterators}

    Note: the folds below are written with explicit monadic state on purpose:
    iterating with a recursive function and [let*] instead (e.g., [let* v = f
    v x in aux v l]) is stack-safe with native code, but not once compiled
    into JavaScript, where tail-calls to unknown functions (here, closures
    built by [aux v l]) are not optimized.  Use these folds to iterate over
    potentially long sequences. *)

(** [fold_list f init l] folds [f] over the elements of [l], from left to
    right. *)
let fold_list f init l : _ t =
  fun env acc ->
  List.fold_left (fun (v, acc) x -> f v x env acc) (init, acc) l

(** [fold_nel f init l] folds [f] over the elements of [l], from left to
    right. *)
let fold_nel f init l : _ t =
  fun env acc ->
  NEL.fold_left ~f:(fun (v, acc) x -> f v x env acc) (init, acc) l

let map_list f l =
  let+ rev = fold_list (fun rev x -> let+ y = f x in y :: rev) [] l in
  List.rev rev

let map_nel f l =
  let+ rev = fold_nel (fun rev x -> let+ y = f x in y :: rev) [] l in
  NEL.of_rev_list rev

let map_option f = function
  | None -> return None
  | Some x ->
      let+ y = f x in
      Some y
