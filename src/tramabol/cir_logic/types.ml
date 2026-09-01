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

(* In addition to the type variables mentioned in {!Cir_types}, this file uses
   ['s] to denote the type of runtime states. *)

open Cir_types

module NEL = Cobol_common.Basics.NEL

(* --- *)

type runtime_operation = ..

type runtime_error = ..
type runtime_error +=
  | Unsupported_runtime_operation of runtime_operation

type runtime_errors = runtime_error NEL.t

exception FATAL of runtime_errors

(* --- *)

(** COBOL exceptions... not represented for now.  May need to be part of
    Cir_type if this kind of data needs to be manipulated explicitly by COBOL
    operations... *)
type cobol_exception = |

(* TODO: We may need to add more type parameters to make the value domain more
   agnostic to branching behaviors.  *)

type ('f, 'r, 'm, 's) manager =
  {
    enter_module:
      'm -> params:'f array -> unit;
    leave_module:
      'm -> unit;
    module_ws_needs_initialization:
      'm -> bool;
    module_ws_initialization_done:
      'm ->
      's -> 's evaluation_result;

    init_field:
      vm:('f, 'r, 'm, 's) manager -> 'f mutable_field ->
      's -> 's evaluation_result;
    field_as_int:
      vm:('f, 'r, 'm, 's) manager -> 'f field ->
      's -> ('s * int) evaluation_result;
    display_fields:
      vm:('f, 'r, 'm, 's) manager -> advancing:bool -> 'f field array ->
      's -> 's evaluation_result;
  }

and 's evaluation_result = ('s, runtime_errors) result

(* (\* and ('s, 'result) unique_update = 's -> ('s, 'result) unique_outcome *\) *)
(* and ('s, 'result, 'b) update = 's -> ('s, 'result, 'b) outcome *)

(* and ('s, 'result, 'b) outcome = *)
(*   | Result of ('s * 'result) *)
(*   | Exception of ('s * cobol_exception) *)
(*   | Runtime_errors of runtime_errors *)
(*   | Choice of ('s, 'result) unique_outcome NEL.t * 'b *)

(* and ('s, 'result) unique_outcome = ('s, 'result, impossible) outcome *)

(* and impossible = |                       (\* to rule out choices when relevant *\) *)

(* type ('f, 'r, 'm, 's) manager = *)
(*   { *)
(*     enter_module: *)
(*       'm -> params:'f array -> unit; *)
(*     leave_module: *)
(*       'm -> unit; *)
(*     module_ws_needs_initialization: *)
(*       'm -> bool; *)
(*     module_ws_initialization_done: *)
(*       'b. ('m -> ('s, unit, 'b) update); *)

(*     init_field: *)
(*       'b. *)
(*         (vm:('f, 'r, 'm, 's) manager -> 'f mutable_field -> *)
(*          ('s, unit, 'b) update); *)
(*     field_as_int: *)
(*       'b. *)
(*         (vm:('f, 'r, 'm, 's) manager -> 'f field -> *)
(*          ('s, int, 'b) update); *)
(*     display_fields: *)
(*       'b. *)
(*         (vm:('f, 'r, 'm, 's) manager -> advancing:bool -> 'f field array -> *)
(*          ('s, unit, 'b) update); *)

(*     (\* apply_unit_update: *\) *)
(*     (\*   'b 'result. ('s * (unit, 'b) outcomes -> 's * 'result unique_outcome); *\) *)
(*     (\* join: *\) *)
(*     (\*   'b. *\) *)
(*     (\*     (('s, unit, 'b) outcome -> ('s, runtime_errors) result); *\) *)
(*     (\* join_val: *\) *)
(*     (\*   'b 'result. *\) *)
(*     (\*     (('s, 'result, 'b) outcome -> ('s * 'result, runtime_errors) result); *\) *)
(*     (\* forget_result: *\) *)
(*     (\*   ('s, unit) unique_outcome -> 's * unit unique_outcome; *\) *)
(*     (\* merge_outcomes: *\) *)
(*     (\*   'b. ('s * (unit, 'b) outcomes -> 's * unit unique_outcome); *\) *)
(*     unit_result: *)
(*       ('s, unit) unique_outcome -> 's evaluation_result; *)
(*   } *)

(* and 's evaluation_result = ('s, runtime_errors) result *)

(* (\* and ('s, 'result) unique_update = 's -> ('s, 'result) unique_outcome *\) *)
(* and ('s, 'result, 'b) update = 's -> ('s, 'result, 'b) outcome *)

(* and ('s, 'result, 'b) outcome = *)
(*   | Result of ('s * 'result) *)
(*   | Exception of ('s * cobol_exception) *)
(*   | Runtime_errors of runtime_errors *)
(*   | Choice of ('s, 'result) unique_outcome NEL.t * 'b *)

(* and ('s, 'result) unique_outcome = ('s, 'result, impossible) outcome *)

(* and impossible = |                       (\* to rule out choices when relevant *\) *)
