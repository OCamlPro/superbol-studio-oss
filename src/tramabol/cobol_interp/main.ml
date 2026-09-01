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

open Cir_types
open Types

let () =
  Cir_builder.Printer.register_printers ();
  Cir_logic.Printer.register_printers ();
  Printer.register_printers ()

let build_error e =
  Status.errors (Initialization_errors (NEL.one e))

let builder =
  Values.builder
and vm =
  Values.manager

let unit unit =
  try
    Runtime.run ~f:begin fun () ->
      match Cir_builder.Module.of_cobol_unit ~builder unit with
      | Error errs ->
          Error (Initialization_errors errs)
      | Ok m ->
          match Cir_logic.Main.run_module ~vm m () with
          | Error errs ->
              Error (Runtime_errors errs)
          | Ok (_state, x) ->
              Ok x
    end
  with
  | Cir_logic.Types.FATAL errs ->
      Error (Runtime_errors errs)
  | e ->
      raise e

let group (group: Cobol_unit.Types.group) =
  match Cobol_unit.Group.cardinal group with
  | 1 ->
      unit @@ Cobol_unit.Group.choose group
  | 0 ->
      build_error @@ Invalid_compilation_group { reason = `empty_group }
  | _ ->
      build_error @@ Invalid_compilation_group { reason = `non_singleton_group }
