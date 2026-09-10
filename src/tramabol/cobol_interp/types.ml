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

open Ezlibcob.V1
open Cobol_common.Srcloc.TYPES

(* --- *)

(** {2 Fields} *)

type cob_field = CobField.t cptr [@printer Field_printer.pp_cob_field]
[@@deriving show]

type cob_field_handle = cob_field Cir_types.field
type cob_field_access = cob_field Cir_types.field_access

(** {2 Records} *)

type cob_record_handle =
  cob_record_data Cir_types.record_handle

and cob_record_data =
  {
    record_data_ptr: U8.t cptr [@opaque];
    record_data_size: int;                                             (* > 0 *)
  }
[@@deriving show]

(** {2 Modules} *)

type module_handle =
  (cob_field, cob_record_data, cob_module_memory) Cir_types.module_handle

and cob_module_memory =
  {
    module_ptr: CobModule.t cptr;
    module_globals: CobGlobal.t cptr;
    mutable module_initialized: bool;
  }

let pp_cob_module_memory ppf _ =
  Pretty.string ppf "<cob_module>"
let pp_module_handle ppf =
  Cir_types.pp_module_handle pp_cob_field pp_cob_record_data
    pp_cob_module_memory ppf

(** {2 Builder & State} *)

(** CIR builder with values that are managed by libcob. *)
type builder =
  (cob_field, cob_record_data, cob_module_memory) Cir_builder.Types.builder

(** Value manager for the libcob-based CIR *)
type manager =
  (cob_field, cob_record_data, cob_module_memory,
   state, state Cir_logic.Types.branch) Cir_logic.Types.manager

(** The libcob-based CIR is purely imperative, so it's state is [unit]. *)
and state = unit

(** State, or else runtime errors *)
type evaluation_result =
  state Cir_logic.Types.evaluation_result

(* --- *)

(** Options for the libcob-based CIR interpreter. *)
type options =
  {
    integer_literals: [`numeric_display | `binary_when_small_enough];
  }

(* --- *)

(* Note: edit the corresponding functions in `printer.ml` and `error.ml` when
   adjusting this type. *)
type Cir_builder.Types.unsupported_stuff +=
  | Literal of Cobol_data.Types.literal_value
  | Field_usage
  | Non_constant_size

(* Note: edit the corresponding functions in `printer.ml` and `error.ml` when
   adjusting this type. *)
type Cir_builder.Types.error +=
  | Invalid_compilation_group of
      {
        reason: [ `empty_group | `non_singleton_group ];
      }
  | Ezlibcob_build_error of
      {
        loc: srcloc option;
        error: Ezlibcob.V1.error;
      }

(* Note: edit the corresponding function in `printer.ml` when adjusting this
   type. *)
type Cir_logic.Types.runtime_operation +=
  | Module_cancellation

(* Note: edit the corresponding function in `printer.ml` when adjusting this
   type. *)
type Cir_logic.Types.runtime_error +=
  | Ezlibcob_runtime_error of Ezlibcob.V1.error
  | Invalid_field_type of
      {
        expected_descr: string;
        got: cob_field;
      }
  | Invalid_refmod of
      {
        what: [`offset | `length of (*given_offset:*) int ];
        got: int;
        expected_max: int;
      }
  | Module_reinitialzation of
      {
        module_name: string;
      }
  | Table_index_out_of_bounds of
      {
        index_given: int;                            (* Note: ints for now... *)
        index_min: int;
        index_max: int;
      }

type errors =
  | Initialization_errors of Cir_builder.Types.errors
  | Runtime_errors of Cir_logic.Types.localized_runtime_errors
