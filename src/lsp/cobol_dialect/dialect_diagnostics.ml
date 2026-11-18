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

let decompose
  : 'a verification -> 'a option * [`Error of usage | `Warning of usage] option
  = function
    | Ok (result, usage) -> Some result, Option.map (fun u -> `Warning u) usage
    | Error (Some ({ reason = `Ignored; _ } as u)) -> None, Some (`Warning u)
    | Error (Some u) -> None, Some (`Error u)
    | Error None -> None, None

let pp_usage ppf { feature; reason; _ } =
  let short = Dialect_feature.short feature in
  match reason with
  | `Used | `Ignored ->
      Pretty.print ppf "%(%)@ used" short
  | `Archaic o ->
      Pretty.print ppf "%(%)@ is@ archaic@ in@ %a" short Dialect_origin.pp o
  | `Obsolete o ->
      Pretty.print ppf "%(%)@ is@ obsolete@ in@ %a" short Dialect_origin.pp o
  | `Unconformable o ->
      Pretty.print ppf "%(%)@ does@ not@ conform@ to@ %a"
        short Dialect_origin.pp o
