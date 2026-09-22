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

open Cobol_ir.Types
open Types

let () =
  Printer.register_printers ()

let errors e =
  Error e
let error e =
  errors (NEL.one e)

let vm =
  Values.manager

let unit (unit: Cobol_unit.Types.t) =
  try Runtime.run ~f:(fun () -> Cobol_ir.Main.run_unit ~vm unit) with
  | FATAL e ->
      errors e
  | e ->
      raise e

let group (group: Cobol_unit.Types.group) =
  match Cobol_unit.Group.cardinal group with
  | 1 ->
      unit @@ Cobol_unit.Group.choose group
  | 0 ->
      error @@ Invalid_compilation_group { reason = `empty_group }
  | _ ->
      error @@ Invalid_compilation_group { reason = `non_singleton_group }
