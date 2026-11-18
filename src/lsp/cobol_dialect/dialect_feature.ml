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

let uncapitalize_heuristics s =
  match s.[1] with
  | 'A' .. 'Z' | '-' -> s
  | _ -> String.uncapitalize_ascii s
  | exception Invalid_argument _ -> String.uncapitalize_ascii s

let short = function
  | { short = Some s; _ } -> s
  | { doc; _ } -> Pretty.Simple.map uncapitalize_heuristics doc

let def ~of_level ~name ?short doc =
  { info = { name; short; doc }; of_level }

let unit ~name ?short doc =
  def ~of_level:Fun.id ~name ?short doc

let bind ~origin { info; _ } level =
  { feature = info; level; origin }

let bind_level ~origin feature level =
  bind ~origin feature (feature.of_level level)

let verify { feature; level; origin } ~loc : _ verification =
  let usage reason = Some { loc; feature; reason } in
  match level with
  | Ok s -> Ok (s, None)
  | Warning s -> Ok (s, usage `Used)
  | Archaic s -> Ok (s, usage (`Archaic origin))
  | Obsolete s -> Ok (s, usage (`Obsolete origin))
  | Skip -> Error None
  | Ignore -> Error (usage `Ignored)
  | Error -> Error (usage `Used)
  | Unconformable -> Error (usage (`Unconformable origin))
