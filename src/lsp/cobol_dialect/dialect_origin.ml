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

open Dialect_types

let of_id id =
  { id; display_name = Dialect_id.to_string id; file = None; ancestry = [id] }

let pp ppf { display_name; _ } =
  Fmt.string ppf display_name

(** Last dialect of [ancestry] that satisfies [f], if any. *)
let closest_ancestor f { ancestry; _ } =
  List.fold_left (fun found id -> if f id then Some id else found)
    None ancestry
