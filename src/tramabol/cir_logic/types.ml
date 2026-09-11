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

(* In addition to the type variables mentioned in {!Cir_types}, this file uses:

   - ['bool] to denote the type of evaluated conditions; a concrete interpreter
     will typically use ['bool = bool];

   - ['s] to denote the type of runtime states; a concrete imperative
     interpreter will typically use ['s = unit];

   - ['branch] to denote control-flow branching behaviors. *)

open Cir_types

module NEL = Cobol_common.Basics.NEL

(* --- *)

type runtime_operation = ..

(*  *)

(* Note: call `Printer.register_runtime_error_printer` and
   `Error.register_runtime_error_loc_retriever` when extending this type. *)
type runtime_error = ..

(* Note: edit the corresponding functions in `printer.ml` and `error.ml` when
   adjusting this type. *)
type runtime_error +=
  | Unsupported_runtime_operation of runtime_operation

type localized_runtime_errors = localized_runtime_error NEL.t
and localized_runtime_error =
  {
    loc: Cobol_common.Srcloc.TYPES.srcloc option;
    error: runtime_error;
  }

(* --- *)

(** Type of {b concrete} branches. *)
type ('s, 'code_target) branch =
  | Continue of 's
  | Stop of 's * int
  | Perform of 's * 'code_target

(** COBOL exceptions... not represented for now.  May need to be part of
    Cir_type if this kind of data needs to be manipulated explicitly by COBOL
    operations... *)
type cobol_exception = |

type ('f, 'bool, 'r, 'm, 's, 'branch) manager =
  {
    enter_module:
      'm -> params: 'f array -> unit;
    leave_module:
      'm -> unit;
    module_ws_needs_initialization:
      'm -> bool;
    module_ws_initialization_done:
      'm -> 's -> 's evaluation_result;

    init_field:
      vm: 'vm -> 'f field_access -> 's -> 's evaluation_result;
    field_value:
      vm: 'vm -> 'f field_reference -> 's -> ('s * 'f) evaluation_result;
    data_value:
      vm: 'vm -> 'f data_reference -> 's -> ('s * 'f) evaluation_result;

    eval_condition:
      vm: 'vm -> 'f condition -> 's -> ('s * 'bool) evaluation_result;

    display_fields:
      vm: 'vm -> advancing: bool -> 'f array -> 's -> 'branch evaluation_result;
    stop:
      vm: 'vm -> ?status: 'f -> 's -> 'branch evaluation_result;
    conditional:
      vm: 'vm -> 'bool -> 'f code_block -> 'f code_block -> 's -> 'branch evaluation_result;

    proceed:
      'branch -> ('s, 'f code_block) branch evaluation_result;
  }
  constraint 'vm = ('f, 'bool, 'r, 'm, 's, 'branch) manager

and 's evaluation_result = ('s, localized_runtime_errors) result
