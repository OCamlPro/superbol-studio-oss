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
type 'f data_reference =
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
(* Decimal_field? *)

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
    cell_index_field: 'f data_reference;
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
  (* TODO: use a struct with optional ranges... *)
  | Direct_access of 'f fixed_mutable_field
  | Indirect_access of
      {
        base_field: 'f fixed_mutable_field;
        ranges: 'f access_range NEL.t;
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

(** High-level statements for the PROCEDURE DIVISION. *)
type 'f statement =
  | IR_display of                   (* Note: may actually branch on exception *)
      {
        data_refs: 'f data_reference array;
        advancing: bool;
      }
  | IR_stop of
      {
        optional_status: 'f data_reference option;
      }
  (* | IR_local_bind of                                             (\* SSA value *\) *)
  (*     { *)
  (*       symbol_binding: 'f symbol_binding; *)
  (*       block: 'f code_block;  (\* where [symbol -> 'f immutable_field \in env] *\) *)
  (*     } *)

(* and 'f symbol_binding = *)
(*   { *)
(*     symbol: SYMBOL.t; *)
(*     (\* symbol_field: 'f mutable_field; *\) *)
(*     symbol_value: 'f expr; *)
(*   } *)

(* and 'f expr = *)
(*   | IR_expr_field of 'f field *)

(** A block of code that is amenable to interpretation; for now, only a list of
    statements. *)
and 'f code_block =
  'f statement with_loc list                                        (* for now *)

[@@deriving show { with_path = false }]

type ('f, 'r, 'm) module_handle =
  {
    module_memory: 'm (* [@opaque] *);
    module_unit: Cobol_unit.Types.t
                  (* [@printer Cobol_unit.Printer.pp_cobol_unit']) *)[@opaque];
    module_data: ('f, 'r) data;
    module_proc: 'f code_block;                           (* one block for now *)
  }

[@@deriving show { with_path = false }]

(* --- *)
