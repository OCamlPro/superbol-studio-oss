(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open EzCompat (* for StringMap and Stringset *)

(* CHECKE: Is it worth having this long name in addition to StrMap *)
module StringMap = StringMap
module StringSet = StringSet
module Strings = StringSet             (** alias of {!StringSet} *)
module StrMap = StringMap              (** alias of {!StringMap} *)
module IntMap = Map.Make (Int)
module CharSet = Set.Make (Char)

module Pair = struct

  let with_fst l r = (l, r)
  let with_snd r l = (l, r)

  let map_fst ~f (l, r) = (f l, r)
  let map_snd ~f (l, r) = (l, f r)

  (** [filter_fst (Some l, r) = Some (l, r) ] and [filter_fst (None, _) = None] *)
  let if_fst (l, r) = Option.map (fun l -> l, r) l

  (** [filter_snd (l, Some r) = Some (l, r)] and [filter_snd (_, None) = None] *)
  let if_snd (l, r) = Option.map (fun r -> l, r) r

  let filter = function
    | Some l, Some r -> Some (l, r)
    | _ -> None

  let filter_map_fst ~f (l, r) = Option.map (fun l -> f l, r) l

  let filter_snd_map_pair ~f (l, r) = Option.map (fun r -> f (l, r) ) r

  let filter_map_snd ~f (l, r) = Option.map (fun r -> l, f r) r

  let filter_map ~fl ~fr = function
    | Some l, Some r -> Some (fl l, fr r)
    | _ -> None

  let swap (f, s) = (s, f)
end

(* Fabrice: we should upstream such functions in ocplib-stuff, within
   the EzList module *)
module LIST = struct
  (** [split_at_first ~prefix ~where p list] splits [list] right after, right
      before, or around the first element [e] that satisfies [p e].

      [prefix] indicates whether or not to keep the prefix in revered order, and
      [where] instructs where to split ([`Around] discards the element). *)
  let split_at_first
      ~(prefix: [`Same | `Rev])
      ~(where: [`After | `Before | `Around])
      p
    =
    let prefix = match prefix with
      | `Same -> List.rev
      | `Rev -> fun l -> l
    in
    let rec aux acc l = match l, where with
      | [], _ -> Error ()
      | x :: tl, _ when not (p x) -> aux (x :: acc) tl
      | x :: tl, `After -> Ok (prefix (x :: acc), tl)
      | x :: tl, `Before -> Ok (prefix acc, x :: tl)
      | _ :: tl, `Around -> Ok (prefix acc, tl)
    in
    aux []

  (** [take_while pred l] returns all the successive elements of [l] while [pred elt] is
      is satisfied, [elt] being the first element of the remaining of the list. *)
  let take_while pred list =
    let rec aux acc l =
      match l with
      | hd::tl when pred hd ->
          aux (hd::acc) tl
      | _ ->
           List.rev acc
    in
    aux [] list

  (*TODO: Remove this and edit its occurences with List.fold_left_map *)
  let foldmap ~f (l, acc) =
    let l, acc = List.fold_left
        (fun (l, acc) x -> let x, acc = f acc x in x::l, acc) ([], acc) l
    in
    List.rev l, acc


  (** [fold_left_while pred f acc l] is (f (... (f acc l1) ...) ln) with [l1] [ln] the elements of
      [l] for which [pred acc] is satisfied. *)
  let rec fold_left_while pred f acc l =
    match l with
    | [] -> acc
    | hd::tl when pred acc -> fold_left_while pred f (f acc hd) tl
    | _ -> acc

  (** [fold_left_whilei pred f acc l] is (f n (... (f 0 acc l0) ...) ln) with [l0] [ln] the elements of
      [l] for which [pred acc] is satisfied. *)
  let fold_left_whilei pred f acc l =
    let rec aux idx pred f acc l =
      match l with
      | [] -> acc
      | hd::tl when pred acc -> aux (idx + 1) pred f (f idx acc hd) tl
      | _ -> acc
    in
    aux 0 pred f acc l
end

(** This operator maps a ['a option * 'b] to the function [f]. The function [f] must be of type
    ['b -> 'a -> 'c * 'b]. [(x, acc) >>= f] returns [None, acc] if [x = None] or [(Some x', acc')]
    if [x = Some y] with [x', acc' = f acc y]. *)
let (>>=) (x, acc) f =
  Option.fold ~none:(None, acc) ~some:(fun x -> let x, acc = f acc x in Some x, acc) x


(*CHECKME: Is there an already defined operator for this? If not maybe we can keep these
 * somewhere else, it might be useful in more than one places, or maybe it's too confusing. *)
let (>>) f g = (fun x -> f x |> g)
