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

module TYPES: sig
  type exec_block = ..
end
include module type of TYPES
  with type exec_block = TYPES.exec_block

type t = exec_block

val register_exec_block_type
  : name: string
  -> compare: (t -> t -> int option)
  -> pp: (t -> Pretty.delayed option)
  -> unit

val compare: t -> t -> int
val pp: t Pretty.printer
