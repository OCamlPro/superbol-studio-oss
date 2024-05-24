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
  type diagnostic = ..
end
include module type of TYPES
  with type exec_block = TYPES.exec_block
   and type diagnostic = TYPES.diagnostic

type t = exec_block

val register_exec_block_type
  : name: string
  -> compare: (t -> t -> int option)
  -> pp: (t -> Pretty.delayed option)
  -> unit
val compare: t -> t -> int
val pp: t Pretty.printer

val register_diagnostic_type
  : name: string
  -> severity: (diagnostic -> Diagnostics.severity option)
  -> loc: (diagnostic -> Srcloc.srcloc option)
  -> pp: (diagnostic -> Pretty.delayed option)
  -> unit
val diagnostic_severity: diagnostic -> Diagnostics.severity
val diagnostic_loc: diagnostic -> Srcloc.srcloc option
val pp_diagnostic: diagnostic Pretty.printer
