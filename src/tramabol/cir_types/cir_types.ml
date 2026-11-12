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

   - ['f]: type of field values;

   - ['r]: type of record memory, where named fields are stored;

   - ['m]: type of module-specifc memory. *)

open Cobol_common.Srcloc.TYPES

module NEL = Cobol_common.Basics.NEL

(* --- *)

(** {2 Data representation}

    Representations are parametric in the type ['f] of field values, as well as
    the type ['r] of record memory. *)

(** General-purpose reference to a data field; comes with a source location. *)
type 'f field_reference =
  {
    field_ref: 'f field;
    field_ref_loc: srcloc [@opaque];
  }

(** Manipulated COBOL fields may either be constant or lie in memory. *)
and 'f field =
  | Constant_field of 'f immutable_field
  | Field_in_memory of
      {
        field: 'f resolved_field;
        field_info: 'f field_definition_info;
      } (** A field for which we can compute the location in the record. *)

(** We directly map immutable fields with their value representation. *)
and 'f immutable_field =
  'f

(** A field in memory is addressable and may have an initial value.  It always
    comes from a definition in a COBOL source. *)
and 'f field_definition_info =
  {                               (* CHECKME: may we need the record handle?  *)
    field_definition:
      Cobol_data.Types.field_definition with_loc
      [@printer Cobol_data.Printer.pp_field_definition'];
    field_initial_value: 'f immutable_field option;
  }

(** Resolved fields may either be fixed, or require access to already known
    indexing data. *)
and 'f resolved_field =
  | Fixed_field of 'f
  | Table_field of 'f resolved_table_cell

and 'f resolved_table_cell =
  {
    cell_first_field: 'f resolved_field;
    cell_index_field: 'f field_reference;
    cell_index_max: int;
    cell_stride: int;
  }

(** Handle for record memory: associates a memory with its definition. *)
and 'r record_handle =
  {
    record_memory: 'r;
    record_definition:
      Cobol_data.Types.record
      [@printer Cobol_data.Printer.pp_record];
  }

[@@deriving show { with_path = false }]

(** {2 Module representation}

    In addition to the type variables above, the type of modules is parametric
    in the type ['m] of module memory. *)

module FIELDS_MAP = Cobol_unit.Resolver_map

(** Named fields are always associated with fields that lie in memory (immutable
    fields typically come from literals in source programs). *)
type 'f fields_map = 'f field_access FIELDS_MAP.t

and 'f field_access =
  {
    access_field: 'f fixed_mutable_field;
    access_ranges: 'f access_range list;
  }

(** A field that is at a fixed location in a record. *)
and 'f fixed_mutable_field =
  {
    fixed_field: 'f;
    fixed_field_info: 'f field_definition_info;
  }

and 'f access_range =
  | Fixed_range of
      {
        max: int;
      }
  | Depending_range of
      {
        min: int;
        max: int;
        odo_field: 'f fixed_mutable_field;
      }

[@@deriving show { with_path = false }]

(** General-purpose reference to data, based on a given field, with optional
    reference modification. *)
type 'f data_reference =
  {
    data_field: 'f field;
    data_ref_loc: srcloc [@opaque];
    data_refmod: 'f refmod option;
  }

and 'f refmod =
  {
    refmod_left: 'f field_reference;
    refmod_length: 'f field_reference option;
  }

[@@deriving show { with_path = false }]

type 'f condition =                 (* WIP implicit equality condition for now *)
  bool * 'f field_reference * 'f field_reference

[@@deriving show { with_path = false }]

let pp_fields_map pe =
  FIELDS_MAP.pp @@ pp_field_access pe

(** Structure that gathers elements from the DATA DIVISION of a module. *)
type ('f, 'r) data =
  {
    map: 'f fields_map [@opaque];
    working_storage: ('f, 'r) memory_storage;
    local_storage: ('f, 'r) memory_storage;
  }

and ('f, 'r) memory_storage =
  {
    storage_records: 'r record_handle list;
    storage_fields: 'f field_access list;
  }

[@@deriving show { with_path = false }]

(** Straight-line statements for the PROCEDURE DIVISION, that cannot branch. *)
type 'f straight_statement =
  | IR_display of                   (* Note: may actually branch on exception *)
      {
        data_refs: 'f data_reference array;
        advancing: bool;
      }
  | IR_move of
      {
        source: 'f data_reference;
        targets: 'f data_reference NEL.t;
      }
  | IR_arithmetic of                       (* truncates targets on size error *)
      'f arithmetic_operation

and 'f arithmetic_operation =
  {
    operator: arithmetic_operator;
    operand: 'f field_reference;
    targets: 'f arithmetic_target NEL.t;
  }

and arithmetic_operator =
  | Add
  | Subtract

and 'f arithmetic_target =
  {
    target: 'f field_reference;
    rounded: bool;
  }

[@@deriving show { with_path = false }]

(** Control-flow graph of basic blocks, where every block is designated by its
    index in [proc_blocks]. *)
type 'f procedure =
  {
    proc_blocks: 'f basic_block array;
    proc_entry: label;
  }

and 'f basic_block =
  {
    block_stmts: 'f straight_statement with_loc array;
    block_term: 'f terminator with_loc;
  }

(** Index of a {e basic block} in its enclosing procedure; see {!procedure}. *)
and label = int

(** Statements that end a basic block. *)
and 'f terminator =
  | IR_goto of label
  | IR_branch of
      {
        condition: 'f condition;
        if_true: label;
        if_false: label;
      }
  | IR_goto_depending of
      {
        selector: 'f field_reference;
        targets: label NEL.t;
        default: label;
      }
  | IR_perform of
      {
        entry: label;
        exit_point: exit_point;
        return_to: label;
      }
  | IR_perform_exit of
      {
        exit_points: exit_point NEL.t;
        fallthrough: label;
      }
  | IR_set_counter of                                (* initializes a counter *)
      {
        counter: counter;
        count: 'f field_reference;
        next: label;
      }
  | IR_count_down of                                (* decrements the counter *)
      {
        counter: counter;
        if_positive: label;                 (* where to jump if positive *)
        if_zero: label;                     (* where to jump if zero already  *)
      }
  | IR_checked_arithmetic of        (* leaves targets unchanged on size error *)
      {
        arithmetic: 'f arithmetic_operation;
        on_size_error: label;
        not_on_size_error: label;
      }
  | IR_stop of
      {
        optional_status: 'f data_reference option;
      }
  | IR_goback
  | IR_exit_program of
      {
        fallthrough: label;                (* continuation for a main program *)
      }

(** Iteration counter, for [PERFORM ... TIMES] statements. *)
and counter = int

(** End of a paragraph or section, designated by the label of its first
    block, that may terminate a PERFORM. *)
and exit_point =
  | Paragraph_end of label
  | Section_end of label

[@@deriving show { with_path = false }]

type ('f, 'r, 'm) module_handle =
  {
    module_memory: 'm;
    module_unit: Cobol_unit.Types.t [@opaque];
    module_data: ('f, 'r) data;
    module_proc: 'f procedure;
  }

[@@deriving show { with_path = false }]

(* --- *)
