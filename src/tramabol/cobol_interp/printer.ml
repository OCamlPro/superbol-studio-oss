(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Types

let pp_libcob_error ppf = function
  | Ezlibcob.V1.IntegerOverflow ->
      Pretty.print ppf "integer@ overflow"

let pp_errors ppf = function
  | Initialization_errors errors ->
      Cobol_common.Basics.NEL.iter ~f:begin fun e ->
        Pretty.print ppf "Error: @[%a@]@."
          Cir_builder.Printer.pp_error e;
      end errors
  | Runtime_errors errors ->
      Cobol_common.Basics.NEL.iter ~f:begin fun e ->
        Pretty.print ppf "Error: @[%a@]@."
          Cir_logic.Printer.pp_runtime_error e;
      end errors

let register_printers () =

  Cir_builder.Printer.register_unsupported_stuff_printer begin fun ppf -> function
    | Literal l ->
        Pretty.print ppf "literal@ %a" Cobol_data.Printer.pp_value l
    | Field_usage ->
        Pretty.print ppf "field@ usage"
    | _ ->
        raise Exit
  end;

  Cir_builder.Printer.register_error_printer begin fun ppf -> function
    | Invalid_compilation_group { reason = `empty_group } ->
        Pretty.print ppf "Empty@ compilation@ group@ given"
    | Invalid_compilation_group { reason = `non_singleton_group } ->
        Pretty.print ppf "Multiple@ units@ found@ in@ compilation@ group"
    | Ezlibcob_build_error e ->
        pp_libcob_error ppf e
    | _ ->
        raise Exit
  end;

  Cir_logic.Printer.register_runtime_operation_printer begin fun ppf -> function
    | Module_cancellation ->
        Pretty.print ppf "module@ cancellation"
    | _ ->
        raise Exit
  end;

  Cir_logic.Printer.register_runtime_error_printer begin fun ppf -> function
    | Module_reinitialzation { module_name } ->
        Pretty.print ppf "Invalid@ reinitialization@ of@ module@ `%s'"
          module_name
    | Ezlibcob_runtime_error e ->
        pp_libcob_error ppf e
    | _ ->
        raise Exit
  end
