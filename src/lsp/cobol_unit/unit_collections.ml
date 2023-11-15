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

open EzCompat

open Unit_types
open Cobol_common.Srcloc.INFIX

let compare_with_name u name =
  String.compare ~&(u.unit_name) name
let compare_by_name u1 u2 =
  String.compare ~&(u1.unit_name) ~&(u2.unit_name)

module M = struct
  type t = cobol_unit
  let compare = compare_by_name
end

module SET: sig
  include Set.S with type elt = cobol_unit
                 and type t = Set.Make (M).t
  val find_by_name: string -> t -> cobol_unit
  val find_at_loc: Cobol_common.Srcloc.srcloc -> t -> cobol_unit
  val assoc: (cobol_unit -> 'a) -> t -> 'a Map.Make (M).t
  type register = private cobol_unit StringMap.t
  val register: t -> register
end = struct
  include Set.Make (M)
  module MAP = Map.Make (M)

  let find_by_name name us =
    let u = find_first (fun u -> compare_with_name u name >= 0) us in
    if ~&(u.unit_name) = name then u else raise Not_found
  let find_at_loc loc us =
    let u = find_first (fun u -> Stdlib.compare u.unit_loc loc >= 0) us in
    if u.unit_loc = loc then u else raise Not_found
  let assoc f us =
    to_seq us |> Seq.map (fun u -> u, f u) |> MAP.of_seq

  type register = cobol_unit StringMap.t
  let register us : register =
    to_seq us |> Seq.map (fun u -> ~&(u.unit_name), u) |>
    StringMap.of_seq

end

module MAP: sig
  include Map.S with type key = cobol_unit
                 and type +'a t = 'a Map.Make (M).t
  val units: 'a t -> SET.t
  val find_by_name: string -> 'a t -> cobol_unit * 'a
end = struct
  include Map.Make (M)
  let units map =
    to_seq map |> Seq.map fst |> SET.of_seq
  let find_by_name name map =
    let u, v = find_first (fun u -> compare_with_name u name >= 0) map in
    if ~&(u.unit_name) = name then u, v else raise Not_found
end
