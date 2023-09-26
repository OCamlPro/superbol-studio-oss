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

(* module IntMap = Map.Make (Int) *)
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
