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

module TYPES = struct
  type exec_block = ..

  type registered_exec_block =
    {
      name: string;
      compare: exec_block -> exec_block -> int option;
      pp: exec_block -> Pretty.delayed option;
    }
end
include TYPES

type t = exec_block

let registry: (string, registered_exec_block) Hashtbl.t =
  Hashtbl.create 1

let register_exec_block_type ~name ~compare ~pp =
  Hashtbl.replace registry name { name; compare; pp }

let compare a b =
  let open struct exception Res of int end in
  try
    Hashtbl.iter begin fun _ { compare; _ } ->
      match compare a b with
      | Some r -> raise @@ Res r
      | None -> ()
    end registry;
    Stdlib.compare a b                    (* should just compare tags, right? *)
  with Res r -> r

let pp: t Pretty.printer = fun ppf exec_block ->
  try
    Hashtbl.iter begin fun _ { pp; _ } ->
      match pp exec_block with
      | Some pp -> pp ppf; raise Exit
      | None -> ()
    end registry;
    Pretty.string ppf "<unknown EXEC block>"
  with Exit -> ()
