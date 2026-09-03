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
  Cir_builder.Error.register_loc_retrievers ();
  Cir_builder.Printer.register_printers ();
  Cir_logic.Error.register_loc_retrievers ();
  Cir_logic.Printer.register_printers ();
  Error.register_loc_retrievers ();
  Printer.register_printers ()

let build_error e =
  Error (Initialization_errors (NEL.one e))

let default_options =
  {
    integer_literals = `binary_when_small_enough;
  }

let exec_unit ~builder unit =
  match Cir_builder.Module.of_cobol_unit ~builder unit with
  | Error errs ->
      Error (Initialization_errors errs)
  | Ok m ->
      match Cir_logic.Main.run_module ~vm:Values.manager m () with
      | Error errs ->
          Error (Runtime_errors errs)
      | Ok (_state, x) ->
          Ok x

let run_unit ?(options = default_options) unit =
  Runtime.run ~f:begin fun () ->
    exec_unit ~builder:(Values.builder ~options) unit
  end

let run_group ?(options = default_options) group =
  Runtime.run ~f:begin fun () ->
    let builder = Values.builder ~options in
    match Cobol_unit.Group.cardinal group with
    | 1 ->
        exec_unit ~builder @@ Cobol_unit.Group.choose group
    | 0 ->
        build_error @@ Invalid_compilation_group { reason = `empty_group }
    | _ ->
        build_error @@ Invalid_compilation_group { reason = `non_singleton_group }
  end

let print_unit ?(options = default_options) ppf unit =
  Runtime.run ~f:begin fun () ->
    Printer.pp_unit ~builder:(Values.builder ~options) ppf unit
  end

let print_group ?(options = default_options) ppf group =
  Runtime.run ~f:begin fun () ->
    let builder = Values.builder ~options in
    Cobol_unit.Group.fold begin fun unit acc ->
      Status.union acc @@ Printer.pp_unit ~builder ppf unit
    end group (Ok ())
  end
