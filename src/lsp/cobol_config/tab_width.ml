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

let default = [8]

let next_stop ?(tab_width = default) col =
  let rec go pos = function
    | [w] ->
      if pos + w > col then pos + w
      else pos + ((col - pos) / w  + 1) * w
    | w :: rest ->
      let stop = pos + w in
      if stop > col then stop else go stop rest
    | [] -> col + 1
  in
  go 1 tab_width
