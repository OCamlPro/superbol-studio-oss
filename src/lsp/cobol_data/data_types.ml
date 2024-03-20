(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Representation of COBOL data items *)

(* Note: location of qualnames often correspond to the *unqualified* name, with
   implicit qualification based on item groups. *)

open Cobol_common.Srcloc.TYPES

module NEL = Cobol_common.Basics.NEL
type 'a nel = 'a NEL.t

type picture_config = Data_picture.TYPES.config
type picture = Data_picture.t

type usage =
  | Binary of (* [`numeric] *) picture
  | Binary_C_long of signedness                         (* GnuCOBOL *)
  | Binary_char of signedness                           (* +COB2002 *)
  | Binary_double of signedness                         (* +COB2002 *)
  | Binary_long of signedness                           (* +COB2002 *)
  | Binary_short of signedness                          (* +COB2002 *)
  | Bit of (* [`boolean] *) picture
  | Display of (* [any] *) picture
  | Float_binary of { width: [`W32|`W64|`W128];                   (* +COB2002 *)
                      endian: Cobol_ptree.endianness_mode }
  | Float_decimal of { width: [`W16 | `W34];                      (* +COB2002 *)
                       endian: Cobol_ptree.endianness_mode;
                       encoding: Cobol_ptree.encoding_mode }
  | Float_extended                                                (* +COB2002 *)
  | Float_long                                                    (* +COB2002 *)
  | Float_short                                                   (* +COB2002 *)
  | Procedure_pointer                                             (* MF *)
  | Function_pointer of Cobol_ptree.name with_loc                 (* tmp *)
  | Index
  | National of (* [any] *) picture
  | Object_reference of Cobol_ptree.object_reference_kind option       (* tmp *)
  | Packed_decimal of (* [`numeric] *) picture
  | Pointer of Cobol_ptree.name with_loc option                        (* tmp *)
  | Program_pointer of Cobol_ptree.name with_loc option                (* tmp *)
and signedness = { signed: bool }

type data_storage =
  | File
  | Local_storage
  | Working_storage
  | Linkage                                                          (* file? *)

let pp_data_storage ppf s =
  Fmt.string ppf @@ match s with
  | File -> "FILE"
  | Local_storage -> "LOCAL-STORAGE"
  | Working_storage -> "WORKING-STORAGE"
  | Linkage -> "LINKAGE"

type length =
  | Fixed_length
  | Variable_length
  (* Note: OCCURS DYNAMIC is considered fixed-length in ISO/IEC *)

type record =
  {
    record_name: string;
    record_storage: data_storage;
    record_item: item_definition with_loc;
    record_renamings: record_renamings;
  }
and item_definitions = item_definition with_loc nel
and item_redefinitions = item_definition with_loc list

and item_definition =
  | Field of field_definition
  | Table of table_definition

and field_definition =
  {
    field_qualname: Cobol_ptree.qualname with_loc option;
    field_redefines: Cobol_ptree.qualname with_loc option;      (* redef only *)
    field_leading_ranges: table_range list;
    field_offset: Data_memory.offset;         (** offset w.r.t record address *)
    field_size: Data_memory.size;
    field_layout: field_layout;
    field_length: length;
    field_conditions: condition_names;
    field_redefinitions: item_redefinitions;
  }

and field_layout =
  | Elementary_field of
      {
        usage: usage;
        init_value: Cobol_ptree.literal with_loc option;
      }
  | Struct_field of
      {
        subfields: item_definitions;
      }

and table_definition =
  {
    table_field: field_definition with_loc;
    table_offset: Data_memory.offset;
    table_size: Data_memory.size;
    table_range: table_range;
    table_init_values: Cobol_ptree.literal with_loc list;     (* list for now *)
    table_redefines: Cobol_ptree.qualname with_loc option;    (* redef only *)
    table_redefinitions: item_redefinitions;
  }
and table_range =
  {
    range_span: span;
    range_indexes: Cobol_ptree.qualname with_loc list;
  }
and span =
  | Fixed_span of fixed_span         (* OCCURS _ TIMES *)
  | Depending_span of depending_span (* OCCURS _ TO _ TIMES DEPENDING ON _ *)
  | Dynamic_span of dynamic_span     (* OCCURS DYNAMIC CAPACITY _ FROM _ TO _ *)

and fixed_span =
  {
    occurs_times: int with_loc;                                (* int for now *)
  }
and depending_span =
  {
    occurs_depending_min: int with_loc;                        (* int for now *)
    occurs_depending_max: int with_loc;                        (* ditto *)
    occurs_depending: Cobol_ptree.qualname with_loc;
  }
and dynamic_span =
  {
    occurs_dynamic_capacity: Cobol_ptree.qualname with_loc option;
    occurs_dynamic_capacity_min: int with_loc option;
    occurs_dynamic_capacity_max: int with_loc option;
    occurs_dynamic_initialized: bool with_loc;
  }

and condition_names = condition_name with_loc list
and condition_name =
  {
    condition_name_qualname: Cobol_ptree.qualname with_loc;
    condition_name_item: Cobol_ptree.condition_name_item;          (* for now *)
  }

(** Note: RENAMES could be represented by simply adding an (optional,
    non-constant) offset to redefinitions (and use group layouts with FILLERs
    throughout to forbid using the new name as a qualifier).

    Such a representation would be much more general than what typical COBOL
    data definitions allow; in particular, one could have "shifted" redefintions
    of any non-01 group item.

    However, we keep the distinction between RENAMES and REDEFINES to better
    match said typical COBOL, and possibly allow more detailed error
    reporting. *)
and record_renamings = record_renaming with_loc list
and record_renaming =
  {
    renaming_name: Cobol_ptree.qualname with_loc;
    renaming_layout: renamed_item_layout;
    renaming_offset: Data_memory.offset;
    renaming_size: Data_memory.size;
    renaming_from: Cobol_ptree.qualname with_loc;
    renaming_thru: Cobol_ptree.qualname with_loc option;
  }
and renamed_item_layout =
  | Renamed_elementary of
      {
        usage: usage;
      }
  | Renamed_struct of
      {
        subfields: item_definitions;    (* CHECKME: items rather than fields? *)
      }

(* type data_const_record = *)
(*   { *)
(*     const_name: Cobol_ptree.name with_loc; *)
(*     const_descr: Cobol_ptree.constant_item_descr; *)
(*     const_layout: const_layout; *)
(*   } *)

type data_definition =
  | Data_field of
      {
        record: record;
        def: field_definition with_loc;
      }
  | Data_renaming of                                              (* not sure *)
      {
        record: record;
        def: record_renaming with_loc;
      }
  | Data_condition of
      {
        record: record;
        field: field_definition with_loc;
        def: condition_name with_loc;
      }
  | Table_index of
      {
        record: record;                    (* record where [table] is defined *)
        table: table_definition with_loc;          (* table whose index it is *)
        qualname: Cobol_ptree.qualname with_loc;   (* fully qualified name *)
      }

(* screen: "_ OCCURS n TIMES" only. Max 2 dimensions. *)
