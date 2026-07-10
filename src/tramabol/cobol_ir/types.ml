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

(* Type variables used in this file:

   - 'f: type of field values;

   - 'r: type of record memory, where named fields are stored;

   - 'm: type of module-specifc memory. *)

open Cobol_common.Srcloc.TYPES

module NEL = Cobol_common.Basics.NEL

(* --- *)

(** {2 Data representation}

    Representations are parametric in the type ['f] of field values, as well as
    the type ['r] of record memory. *)

(** Manipulated COBOL fields may either be constant or lie in memory. *)
type 'f field =
  | Field_constant of 'f immutable_field
  | Field_in_memory of 'f mutable_field

(** A field in memory is addressable and may have an initial value.  It always
    comes from a definition in a COBOL source. *)
and 'f mutable_field =
  {                               (* CHECKME: may we need the record handle?  *)
    field_value: 'f addressable_field;
    field_initial_value: 'f immutable_field option;
    field_definition: Cobol_data.Types.field_definition with_loc;
  }

(** Adressable fields may either be fixed, or require subscript data. *)
and 'f addressable_field =
  | Fixed_field of 'f                       (* field with constant offset/size *)
(* TODO: | Occur_field of ... *)

(** We directly map immutable fields with their value representation. *)
and 'f immutable_field =
  'f                                                                (* for now *)

(** Handle for record memory; we just keep the definition. *)
and 'r record_handle =
  {
    record_memory: 'r;
    record_definition: Cobol_data.Types.record;
  }

(** {2 Module representation}

    In addition to the type variables above, the type of modules is parametric
    in the type ['m] of module memory. *)

module FIELDS_MAP = Cobol_unit.Resolver_map

(** Named fields are always associated with fields that lie in memory (immutable
    fields typically come from literals in source programs). *)
type 'f fields_map = 'f mutable_field FIELDS_MAP.t

(** Structure that gathers elements from the DATA DIVISION of a module. *)
type 'f fields_data =
  {
    map: 'f fields_map;
    working_storage: 'f mutable_field list;
    local_storage: 'f mutable_field list;
  }

(** High-level statements for the PROCEDURE DIVISION. *)
type 'f statement =
  | Core_display of                 (* Note: may actually branch on exception *)
      {
        fields: 'f field array;
        advancing: bool;
      }
  | Core_stop of
      {
        optional_status: 'f field option;
      }

(** A block of code that is amenable to interpretation; for now, only a list of
    statements. *)
type 'f code_block =
  'f statement with_loc list                                        (* for now *)

type ('f, 'm) module_handle =
  {
    module_memory: 'm;
    module_unit: Cobol_unit.Types.t;
    module_fields: 'f fields_data;
    module_proc: 'f code_block;                                    (* for now *)
    mutable module_initialized: bool;
  }

(* --- *)

type unsupported_stuff = ..
type unsupported_stuff +=
  | Statement of Cobol_ptree.statement
  | Term: _ Cobol_ptree.term -> unsupported_stuff
  | Field_in_occurs
  | Variable_length_field

type undefined_stuff =
  | Data_reference of Cobol_ptree.qualname

type ambiguous_stuff =
  | Data_reference of Cobol_ptree.qualname

type error = ..
type error +=
  | Unsupported of
      {
        loc: srcloc;
        stuff: unsupported_stuff;
      }
  | Undefined of
      {
        loc: srcloc;
        stuff: undefined_stuff;
      }
  | Ambiguous of
      {
        loc: srcloc;
        stuff: ambiguous_stuff;
        candidates: Cobol_ptree.qualname NEL.t;
      }

type errors = error NEL.t

exception FATAL of errors

(* --- *)

(* TODO: should appear in functions below, in a parametric way. *)
type computation_state =
  | Running
  | Stopping of int                                 (* int status... for now? *)

(* TODO: We may need to add more type parameters to make the value domain more
   agnostic to branching behaviors.  In addition, many imperative-style
   operations should be given a functional style to go beyond pure concrete
   interpretation...  *)
type ('f, 'r, 'm) value_manager =
  {
    create_record_data:
      Cobol_data.Types.record -> 'r record_handle;
    create_mutable_field:
      Cobol_data.Types.field_definition with_loc -> 'r record_handle ->
      ('f mutable_field, errors) result;
    create_field_from_literal:
      Cobol_ptree.literal with_loc ->
      ('f immutable_field, errors) result;

    create_module:
      name:string -> source_file:string -> 'm;
    enter_module:
      ('f, 'm) module_handle -> params:'f array -> unit;
    leave_module:
      ('f, 'm) module_handle -> unit;

    init_field:
      vm:('f, 'r, 'm) value_manager -> 'f mutable_field -> (unit, errors) result;
    field_as_int:
      vm:('f, 'r, 'm) value_manager -> 'f field -> (int, errors) result;
    display_fields:
      vm:('f, 'r, 'm) value_manager -> advancing:bool -> 'f field array ->
      (unit, errors) result;
  }
