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

open Data_types

open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

(* --- *)

class ['a] folder = object
  (* inherit ['a] Cobol_common.Visitor.Fold.folder *)
  inherit ['a] Cobol_ptree.Terms_visitor.folder
  method fold_record: (record, 'a) fold = default
  method fold_storage: (data_storage, 'a) fold = default
  method fold_picture: (picture, 'a) fold = default
  method fold_usage: (usage, 'a) fold = default
  method fold_fixed_span: (fixed_span, 'a) fold = default
  method fold_depending_span: (depending_span, 'a) fold = default
  method fold_dynamic_span: (dynamic_span, 'a) fold = default
  method fold_span: (span, 'a) fold = default
  method fold_item_definitions: (item_definitions, 'a) fold = default
  method fold_item_definition': (item_definition with_loc, 'a) fold = default
  (* method fold_item_definition: (item_definition, 'a) fold = default *)
  method fold_item_redefinitions: (item_redefinitions, 'a) fold = default
  (* method fold_field_definitions: (field_definitions, 'a) fold = default *)
  method fold_field_definition': (field_definition with_loc, 'a) fold = default
  method fold_field_definition: (field_definition, 'a) fold = default
  method fold_field_layout: (field_layout, 'a) fold = default
  method fold_table_definition': (table_definition with_loc, 'a) fold = default
  method fold_table_definition: (table_definition, 'a) fold = default
  method fold_table_range: (table_range, 'a) fold = default
  (* method fold_table_dimension: (table_dimension, 'a) fold = default *)
  method fold_condition_names: (condition_names, 'a) fold = default
  method fold_condition_name': (condition_name with_loc, 'a) fold = default
  method fold_condition_name: (condition_name, 'a) fold = default
  method fold_renamed_item_layout: (renamed_item_layout, 'a) fold = default
  method fold_record_renamings: (record_renamings, 'a) fold = default
  method fold_record_renaming': (record_renaming with_loc, 'a) fold = default
  method fold_record_renaming: (record_renaming, 'a) fold = default
  method fold_memory_offset: (Data_memory.offset, 'a) fold = default
  method fold_memory_size: (Data_memory.size, 'a) fold = default
end

(* --- *)

let fold_storage (v: _ #folder) = leaf v#fold_storage
let fold_memory_offset (v: _ #folder) = leaf v#fold_memory_offset
let fold_memory_size (v: _ #folder) = leaf v#fold_memory_size
let fold_picture (v: _ #folder) = leaf v#fold_picture      (* leaf (for now?) *)

let fold_usage (v: _ #folder) =
  handle v#fold_usage
    ~continue:begin function
      | Binary pic
      | Bit pic
      | Display pic
      | National pic
      | Packed_decimal pic ->
          fold_picture v pic
      | Function_pointer name
      | Object_reference Some NamedClass { class_or_interface_name = name; _ }
      | Pointer Some name
      | Program_pointer Some name ->
          Cobol_ptree.Terms_visitor.fold_name' v name
      | Binary_C_long _
      | Binary_char _
      | Binary_double _
      | Binary_long _
      | Binary_short _
      | Float_binary _
      | Float_decimal _
      | Float_extended
      | Float_long
      | Float_short
      | Index
      | Object_reference _
      | Procedure_pointer
      | Pointer _
      | Program_pointer _ ->
          Fun.id
    end

let fold_fixed_span (v: _ #folder) =
  handle v#fold_fixed_span
    ~continue:(fun { occurs_times } -> fold_int' v occurs_times)

let fold_depending_span (v: _ #folder) =
  handle v#fold_depending_span
    ~continue:begin fun { occurs_depending_min;
                          occurs_depending_max;
                          occurs_depending } x -> x
      >> fold_int' v occurs_depending_min
      >> fold_int' v occurs_depending_max
      >> Cobol_ptree.Terms_visitor.fold_qualname' v occurs_depending
    end

let fold_dynamic_span (v: _ #folder) =
  handle v#fold_dynamic_span
    ~continue:begin fun { occurs_dynamic_capacity;
                          occurs_dynamic_capacity_min;
                          occurs_dynamic_capacity_max;
                          occurs_dynamic_initialized } x -> x
      >> Cobol_ptree.Terms_visitor.fold_qualname'_opt v occurs_dynamic_capacity
      >> fold_int'_opt v occurs_dynamic_capacity_min
      >> fold_int'_opt v occurs_dynamic_capacity_max
      >> fold' ~fold:fold_bool v occurs_dynamic_initialized
    end

let fold_span (v: _ #folder) =
  handle v#fold_span
    ~continue:begin function
      | Fixed_span d -> fold_fixed_span v d
      | Depending_span d -> fold_depending_span v d
      | Dynamic_span d -> fold_dynamic_span v d
    end

let rec fold_item_definitions (v: _ #folder) =
  handle v#fold_item_definitions
    ~continue:(fold_nel v ~fold:fold_item_definition')

and fold_item_definition' (v: _ #folder) =
  handle v#fold_item_definition'
    ~continue:begin fun def x -> match ~&def with
      | Field f -> fold_field_definition' v (f &@<- def) x
      | Table t -> fold_table_definition' v (t &@<- def) x
    end

(* and fold_item_definition (v: _ #folder) = *)
(*   handle v#fold_item_definition *)
(*     ~continue:begin function *)
(*       | Field f -> fold_field_definition *)
(*     ~continue:begin fun { item_qualname; item_layout; item_offset; *)
(*                           item_size; item_redefinitions; item_redefines; *)
(*                           item_conditions; item_length = _ } x -> x *)
(*       >> Cobol_ptree.Terms_visitor.fold_qualname'_opt v item_qualname *)
(*       >> Cobol_ptree.Terms_visitor.fold_qualname'_opt v item_redefines *)
(*       >> fold_item_layout v item_layout *)
(*       >> fold_memory_offset v item_offset *)
(*       >> fold_memory_size v item_size *)
(*       >> fold_condition_names v item_conditions *)
(*       >> fold_item_redefinitions v item_redefinitions *)
(*     end *)

and fold_item_redefinitions (v: _ #folder) =
  handle v#fold_item_redefinitions
    ~continue:(fold_list v ~fold:fold_item_definition')

(* and fold_field_definitions (v: _ #folder) = *)
(*   handle v#fold_field_definitions *)
(*     ~continue:(fold_nel v ~fold:fold_field_definition') *)

and fold_field_definition' (v: _ #folder) =
  handle' v#fold_field_definition' v ~fold:fold_field_definition

and fold_field_definition (v: _ #folder) =
  handle v#fold_field_definition
    ~continue:begin fun { field_qualname; field_redefines;
                          field_leading_ranges;
                          field_offset; field_size; field_layout;
                          field_conditions; field_redefinitions;
                          field_length = _ } x -> x
      >> Cobol_ptree.Terms_visitor.fold_qualname'_opt v field_qualname
      >> Cobol_ptree.Terms_visitor.fold_qualname'_opt v field_redefines
      >> fold_list ~fold:fold_table_range v field_leading_ranges
      >> fold_field_layout v field_layout
      >> fold_memory_offset v field_offset
      >> fold_memory_size v field_size
      >> fold_condition_names v field_conditions
      >> fold_item_redefinitions v field_redefinitions
    end

and fold_field_layout (v: _ #folder) =
  handle v#fold_field_layout
    ~continue:begin fun l x -> match l with
      | Elementary_field { usage; init_value } -> x
          >> fold_usage v usage
          >> Cobol_ptree.Terms_visitor.fold_literal'_opt v init_value
      | Struct_field { subfields } -> x
          >> fold_item_definitions v subfields
    end

and fold_table_definition' (v: _ #folder) =
  handle' v#fold_table_definition' v ~fold:fold_table_definition

and fold_table_definition (v: _ #folder) =
  handle v#fold_table_definition
    ~continue:begin fun { table_field; table_offset; table_size;
                          table_range; table_init_values;
                          table_redefines; table_redefinitions } x -> x
      >> fold_field_definition' v table_field
      >> fold_memory_offset v table_offset
      >> fold_memory_size v table_size
      >> fold_table_range v table_range
      (* >> Cobol_ptree.Terms_visitor.fold_qualname'_opt v table_index *)
      >> fold_list v table_init_values
        ~fold:Cobol_ptree.Terms_visitor.fold_literal'
      >> Cobol_ptree.Terms_visitor.fold_qualname'_opt v table_redefines
      >> fold_item_redefinitions v table_redefinitions
    end

and fold_table_range (v: _ #folder) =
  handle v#fold_table_range
    ~continue:begin fun { range_span; range_indexes } x -> x
      >> fold_span v range_span
      >> fold_list v range_indexes
        ~fold:Cobol_ptree.Terms_visitor.fold_qualname'
    end

and fold_condition_names (v: _ #folder) =
  handle v#fold_condition_names
    ~continue:(fold_list v ~fold:fold_condition_name')

and fold_condition_name' (v: _ #folder) =
  handle' v#fold_condition_name' v ~fold:fold_condition_name

and fold_condition_name (v: _ #folder) =
  handle v#fold_condition_name
    ~continue:begin fun { condition_name_qualname;
                          condition_name_item = _ } x -> x
      >> Cobol_ptree.Terms_visitor.fold_qualname' v condition_name_qualname
      (* NB: we skip the item def for now as its representation is temporary *)
      (* >> Cobol_ptree.Data_sections_visitor.fold_condition_name_item v *)
      (*   condition_name_item *)
    end

let fold_renamed_item_layout (v: _ #folder) =
  handle v#fold_renamed_item_layout
    ~continue:begin function
      | Renamed_elementary { usage } ->
          fold_usage v usage
      | Renamed_struct { subfields } ->
          fold_item_definitions v subfields
    end

let fold_record_renaming (v: _ #folder) =
  handle v#fold_record_renaming
    ~continue:begin fun { renaming_name; renaming_layout;
                          renaming_offset; renaming_size;
                          renaming_from; renaming_thru } x -> x
      >> Cobol_ptree.Terms_visitor.fold_qualname' v renaming_name
      >> fold_renamed_item_layout v renaming_layout
      >> fold_memory_offset v renaming_offset
      >> fold_memory_size v renaming_size
      >> Cobol_ptree.Terms_visitor.fold_qualname' v renaming_from
      >> Cobol_ptree.Terms_visitor.fold_qualname'_opt v renaming_thru
    end

let fold_record_renaming' (v: _ #folder) =
  handle' v#fold_record_renaming' v ~fold:fold_record_renaming

let fold_record_renamings (v: _ #folder) =
  handle v#fold_record_renamings
    ~continue:(fold_list v ~fold:fold_record_renaming')

let fold_record (v: _ #folder) =
  handle v#fold_record
    ~continue:begin fun { record_name; record_storage;
                          record_item; record_renamings } x -> x
      >> fold_string v record_name
      >> fold_storage v record_storage
      >> fold_item_definition' v record_item
      >> fold_record_renamings v record_renamings
    end
