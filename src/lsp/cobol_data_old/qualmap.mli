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

open Cobol_ptree

type 'a t [@@deriving show]

val empty: 'a t

(** [find qualname map] returns the unique element [e] of [map] that can be qualified with
    [qualname]. Raises [Not_found] if the element is not unique, or does not exists in [map]. *)
val find: qualname -> 'a t -> 'a

(** [find_opt qualname map] returns [Some elt] if [elt] is the unique element of [map] that
    can be qualified by [qualname]. It returns [None] if there is no element or the uniqueness
    is not respected. *)
val find_opt: qualname -> 'a t -> 'a option

(** [find_all qualname map] returns all the elements that can be qualified with [qualname] in [map]
    with their fully qualified name. *)
val find_all: qualname -> 'a t -> (qualname * 'a) list

(** [find_full_qualname qualname map] returns the full name of [qualname] if it is unique *)
val find_full_qualname: qualname -> 'a t -> qualname

(** [find_full_qualname_opt qualname map] returns [Some full_name] with [full_name] being the
    fully qualified name of [qualname] if it exists, [None] otherwise*)
val find_full_qualname_opt: qualname -> 'a t -> qualname option

(** [add qualname value map] returns [map] with [value] bound to the key [qualname]. [qualname] is
    assumed to be the fully qualified name of [value].*)
val add: qualname -> 'a -> 'a t -> 'a t

(** [iter f map] iters over all the values of [map]. *)
val iter: (qualname -> 'a -> unit) -> 'a t -> unit

(** [fold f map init] folds over all the key values bindings of [map] *)
val fold: (qualname -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
