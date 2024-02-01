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

module TYPES = struct
  (** Representation of compilation units *)
  type compilation_unit =
    {
      cu_name: string;
      cu_loc: Cobol_common.Srcloc.srcloc;
      cu_env: Env.PROG_ENV.t;                          (* TODO: Env{ironment}.t *)
      cu_wss: Group.t list;
    }
end
include TYPES

type t = compilation_unit

let compare_by_name { cu_name = n1; _ } { cu_name = n2; _ } =
  String.compare n1 n2

(* --- *)

module M = struct
  type nonrec t = t
  let compare = compare_by_name
end

module SET: sig
  include Set.S with type elt = compilation_unit
                 and type t = Set.Make (M).t
  val find_by_name: string -> t -> compilation_unit
  val find_at_loc: Cobol_common.Srcloc.srcloc -> t -> compilation_unit
  val assoc: (compilation_unit -> 'a) -> t -> 'a Map.Make (M).t
  type register = private compilation_unit StringMap.t
  val register: t -> register
end = struct
  include Set.Make (M)
  module MAP = Map.Make (M)

  let find_by_name name cus =
    let cu = find_first (fun cu -> String.compare cu.cu_name name >= 0) cus in
    if cu.cu_name = name then cu else raise Not_found
  let find_at_loc loc cus =
    let cu = find_first (fun cu -> Stdlib.compare cu.cu_loc loc >= 0) cus in
    if cu.cu_loc = loc then cu else raise Not_found
  let assoc f cus =
    to_seq cus |> Seq.map (fun cu -> cu, f cu) |> MAP.of_seq

  type register = compilation_unit StringMap.t
  let register cus : register =
    to_seq cus |> Seq.map (fun cu -> cu.cu_name, cu) |>
    StringMap.of_seq

end

module MAP: sig
  include Map.S with type key = compilation_unit
                 and type +'a t = 'a Map.Make (M).t
  val compilation_units: 'a t -> SET.t
  val find_by_name: string -> 'a t -> compilation_unit * 'a
end = struct
  include Map.Make (M)
  let compilation_units map =
    to_seq map |> Seq.map fst |> SET.of_seq
  let find_by_name name map =
    let cu, v = find_first (fun cu -> String.compare cu.cu_name name >= 0) map in
    if cu.cu_name = name then cu, v else raise Not_found
end
