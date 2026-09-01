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

type cob_field = CobField.t cptr
type cob_field_handle = cob_field Cir_types.field
type cob_field_mutable = cob_field Cir_types.mutable_field
type cob_field_immutable = cob_field Cir_types.immutable_field

type cob_record_handle = cob_record_data Cir_types.record_handle
and cob_record_data =
  {
    record_data_ptr: U8.t cptr;
    record_data_size: int;                                             (* > 0 *)
  }

type fields_map = cob_field Cir_types.fields_map

type module_handle =
  (cob_field, cob_module_memory) Cir_types.module_handle
and cob_module_memory =
  {
    module_ptr: CobModule.t cptr;
    module_globals: CobGlobal.t cptr;
    mutable module_initialized: bool;
  }

type value_builder =
  (cob_field, cob_record_data, cob_module_memory) Cir_builder.Types.value_builder

type state = unit

type manager =
  (cob_field, cob_record_data, cob_module_memory, state) Cir_logic.Types.manager

type evaluation_result =
  state Cir_logic.Types.evaluation_result

(* --- *)

(* Note: edit the corresponding function in `printer.ml` when adjusting this
   type. *)
type Cir_builder.Types.unsupported_stuff +=
  | Literal of Cobol_data.Types.literal_value
  | Field_usage

(* Note: edit the corresponding function in `printer.ml` when adjusting this
   type. *)
type Cir_builder.Types.error +=
  | Invalid_compilation_group of
      {
        reason: [ `empty_group | `non_singleton_group ];
      }
  | Ezlibcob_build_error of Ezlibcob.V1.error

(* Note: edit the corresponding function in `printer.ml` when adjusting this
   type. *)
type Cir_logic.Types.runtime_operation +=
  | Module_cancellation

(* Note: edit the corresponding function in `printer.ml` when adjusting this
   type. *)
type Cir_logic.Types.runtime_error +=
  | Module_reinitialzation of
      {
        module_name: string;
      }
  | Ezlibcob_runtime_error of Ezlibcob.V1.error
  (* | Deferred_build_error of Cir_builder.Types.error *)

type errors =
  | Initialization_errors of Cir_builder.Types.errors
  | Runtime_errors of Cir_logic.Types.runtime_errors
