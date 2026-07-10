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

open Ezlibcob.V1

module RawField = struct
  type t = Types.field
  let create ~size ~data ~attr = CobField.create ~size ~data ~attr
  let free = CobField.free
  let size = CobField.get_size
  let data = CobField.get_data
  let attr = CobField.get_attr
  (* let with_temp ~f ~size ~data ~attr = *)
  (*   let f = create *)
end

module Field = struct
  type t =
    | Fixed of Types.field           (* for now (we may need base record as w *)
    (* | Sliding of Types.field *)
  let size = function
    | Fixed field -> RawField.size field
  let data = function
    | Fixed field -> RawField.data field
end
