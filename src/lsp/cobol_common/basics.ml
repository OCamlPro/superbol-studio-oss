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

module CharSet = Set.Make (Char)

(* Fabrice: we should upstream such functions in ocplib-stuff, within
   the EzList module *)
module LIST = struct

  (** [split_at_first ~prefix ~where p list] splits [list] right after, right
      before, or around the first element [e] that satisfies [p e].

      [prefix] indicates whether or not to keep the prefix in revered order, and
      [where] instructs where to split ([`Around] discards the element). *)
  let split_at_first p
      ~(prefix: [`Same | `Rev])
      ~(where: [`After | `Before | `Around]) =
    let prefix = match prefix with `Same -> List.rev | `Rev -> Fun.id in
    let rec aux acc l = match l, where with
      | [], _ -> Error ()
      | x :: tl, _ when not (p x) -> aux (x :: acc) tl
      | x :: tl, `After -> Ok (prefix (x :: acc), tl)
      | x :: tl, `Before -> Ok (prefix acc, x :: tl)
      | _ :: tl, `Around -> Ok (prefix acc, tl)
    in
    aux []

  (** [fold_left_while pred f acc l] is (f (... (f acc l1) ...) ln) with [l1]
      [ln] the elements of [l] for which [pred acc] is satisfied. *)
  let fold_left_while pred f acc l =
    let rec aux acc = function
      | hd :: tl when pred acc -> aux (f acc hd) tl
      | _ -> acc
    in
    aux acc l

end

(** Representation for non-empty lists *)
module NEL = struct
  type 'a t =
    | One of 'a
    | (::) of 'a * 'a t
  let compare_lazy cmp a b =
    (** [compare_lazy cmp nel0 nel1] compares [nel0] and [nel1] using [cmp].
      [compare_lazy] is slightly more lazy than its [compare] counterpart,
      but the order is not lexicographical. *)
    let rec aux a b = match a, b with
      | One a, One b -> cmp a b
      | One _, _ -> -1
      | _, One _ -> 1
      | a :: a', b :: b' ->
        let c = cmp a b in
        if c = 0 then aux a' b' else c
    in
    aux a b
  let compare cmp a b =
    (** [compare_std cmp nel0 nel1] compares [nel0] and [nel1] using [cmp]
      in lexicographical order. *)
    let rec aux a b = match a, b with
      | One a, One b -> cmp a b
      | a :: _, One b  ->
        let c = cmp a b in
        if c = 0 then 1 else c
      | One a, b :: _ ->
        let c = cmp a b in
        if c = 0 then -1 else c
      | a :: a', b :: b' ->
        let c = cmp a b in
        if c = 0 then aux a' b' else c
    in
    aux a b
  let equal eq a b =
    let rec aux a b = match a, b with
      | One a, One b -> eq a b
      | a :: a', b :: b' when eq a b -> aux a' b'
      | _ -> false
    in
    aux a b
  let hd = function
    | One x
    | x :: _ -> x
  let rec last = function
    | One x -> x
    | _ :: tl -> last tl
  let fold_left ~f acc l =
    let rec aux acc = function
      | One x -> f acc x
      | x :: tl -> aux (f acc x) tl
    in
    aux acc l
  let rec of_list = function
    | [] -> Pretty.invalid_arg "of_list"
    | [x] -> One x
    | [x; y] -> x :: One y
    | x :: tl -> x :: of_list tl
  let to_list l =
    let rec aux acc = function
      | One x -> List.rev_append acc [x]
      | x :: tl -> aux (List.cons x acc) tl
    in
    aux [] l
  let rev = function
    | One x -> One x
    | hd :: tl ->
      let rec aux acc = function
        | One x -> x :: acc
        | x :: tl -> aux (x::acc) tl
      in aux (One hd) tl
  let ( @ ) a b =
    let rec aux b = function
      | One x -> x :: b
      | x :: tl -> x :: aux b tl
    in aux b a
  let rev_to_list l =
    let rec aux acc = function
      | One x -> List.cons x acc
      | x :: tl -> aux (List.cons x acc) tl
    in
    aux [] l
  let of_rev_list =
    let rec aux acc = function
      | [] -> acc
      | x :: tl -> aux (x :: acc) tl
    in
    function
    | [] -> Pretty.invalid_arg "of_rev_list"
    | last :: tl -> aux (One last) tl
  let map ~f l =
    let rec aux acc = function
      | One x -> of_rev_list (List.cons (f x) acc)
      | x :: tl -> aux (List.cons (f x) acc) tl
    in
    aux [] l
  let exists ~f l =
    let rec aux = function
      | One x -> f x
      | x :: tl -> f x || (aux[@tailcall]) tl
    in
    aux l
  let pp ?fsep ?fopen ?fclose pp_e ppf list =
    Pretty.list ?fopen ?fsep ?fclose pp_e ppf (to_list list)
end
type 'a nel = 'a NEL.t
let pp_nel pp = NEL.pp pp
let compare_nel = NEL.compare
