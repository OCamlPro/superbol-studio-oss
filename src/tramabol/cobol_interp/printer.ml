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

open Cir_builder.Syntax

let pp_unit ~builder ppf unit =
  let* m = Cir_builder.Module.of_cobol_unit ~builder unit in
  Pretty.print ppf "@[%a@]@\n" Types.pp_module_handle m;
  Ok ()

(* --- *)

let pp_libcob_error ppf = function
  | Ezlibcob.V1.IntegerOverflow ->
      Pretty.print ppf "integer@ overflow"

let pp_errors ?platform ppf = function
  | Initialization_errors errors ->
      Cir_builder.Printer.pp_errors ?platform ppf errors
  | Runtime_errors errors ->
      Cir_logic.Printer.pp_localized_runtime_errors ?platform ppf errors

(* --- *)

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
    | Ezlibcob_build_error { error; _ } ->
        pp_libcob_error ppf error
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
    | Ezlibcob_runtime_error e ->
        pp_libcob_error ppf e
    | Invalid_field_type { expected_descr; got } ->
        ignore got;                                                (* for now *)
        Pretty.print ppf "Invalid@ data-type@ encountered@ (%a@ expected)"
          Fmt.text expected_descr
    | Invalid_refmod { what = `offset; got; expected_max } ->
        Pretty.print ppf "Invalid@ offset@ in@ reference@ modification:@;got@ %d,@ \
                          expected@ in@ [1..%d]"
          got expected_max
    | Invalid_refmod { what = `length offset; got; expected_max } ->
        Pretty.print ppf "Invalid@ length@ in@ reference@ modification:@;got@ \
                          %d,@ expected@ in@ [1..%d] when given offset is %d"
          got expected_max offset
    | Module_reinitialzation { module_name } ->
        Pretty.print ppf "Invalid@ reinitialization@ of@ module@ `%s'"
          module_name
    | Table_index_out_of_bounds { index_given; index_min; index_max } ->
        Pretty.print ppf "Index@ is@ out@ of@ bounds:@;got@ %d,@ expected@ in@ \
                          [%d..%d]" index_given index_min index_max
    | _ ->
        raise Exit
  end
