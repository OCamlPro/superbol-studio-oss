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

open Data_types
open Cobol_common.Srcloc.INFIX

(* ignores redefs *)
let fold_definitions ~f def acc =
  let rec aux acc def =
    structure ~&(def.item_layout) (f def acc)
  and structure = function
    | Elementary _ ->
        Fun.id
    | Struct { fields = items }
    | FixedTable { items; _ }
    | DependingTable { items; _ }
    | DynamicTable { items; _ } ->
        fun acc -> NEL.fold_left ~f:aux acc items
  in
  aux acc def

let offset: item_definition -> Data_memory.offset = fun def -> def.item_offset
let size: item_definition -> Data_memory.size = fun def -> def.item_size
