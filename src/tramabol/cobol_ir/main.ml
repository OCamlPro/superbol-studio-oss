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

open Types

open Syntax

(* --- *)

let () =
  Printer.register_printers ()

let rec run_block ~vm module_handle = function
  | [] ->
      Ok 0
  | stmt :: next_statments ->
      let* state = run_statement ~vm module_handle stmt in
      match state with
      | Running ->
          run_block ~vm module_handle next_statments
      | Stopping status ->
          Ok status

and run_statement ~vm _module_handle stmt =
  match ~&stmt with
  | Core_display { fields; advancing } ->
      let* () = vm.display_fields ~vm ~advancing fields in
      Ok Running
  | Core_stop { optional_status = None } ->
      Ok (Stopping 0)
  | Core_stop { optional_status = Some f } ->
      let* status = vm.field_as_int ~vm f in
      Ok (Stopping status)

let run_proc = run_block                                           (* for now *)

let run_unit ~vm (unit: Cobol_unit.Types.t) =
  let* m = Module.of_cobol_unit ~vm unit in
  let* () = Module.init ~vm m in
  vm.enter_module m ~params:[| |] ;
  let* status = run_proc ~vm m m.module_proc in
  vm.leave_module m;
  Ok status
