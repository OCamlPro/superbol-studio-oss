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

(* --- *)

class ['a] folder = object
  (* inherit ['a] Cobol_common.Visitor.Fold.folder *)
  inherit ['a] Cobol_ptree.Terms_visitor.folder
  method fold_record: (record, 'a) fold = default
  method fold_storage: (data_storage, 'a) fold = default
  method fold_picture: (picture, 'a) fold = default
  method fold_usage: (usage, 'a) fold = default
  method fold_item_definitions: (item_definitions, 'a) fold = default
  method fold_item_definition': (item_definition with_loc, 'a) fold = default
  method fold_item_definition: (item_definition, 'a) fold = default
  method fold_item_redefinitions: (item_redefinitions, 'a) fold = default
  method fold_renamed_item_layout: (renamed_item_layout, 'a) fold = default
  method fold_record_renamings: (record_renamings, 'a) fold = default
  method fold_record_renaming': (record_renaming with_loc, 'a) fold = default
  method fold_record_renaming: (record_renaming, 'a) fold = default
  method fold_item_layout: (item_layout, 'a) fold = default
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
      | Usage picture -> fold_picture v picture
    end

let rec fold_item_definitions (v: _ #folder) =
  handle v#fold_item_definitions
    ~continue:(fold_nel v ~fold:fold_item_definition')

and fold_item_definition' (v: _ #folder) =
  handle' v#fold_item_definition' v ~fold:fold_item_definition

and fold_item_definition (v: _ #folder) =
  handle v#fold_item_definition
    ~continue:begin fun { item_qualname; item_layout; item_offset;
                          item_size; item_redefinitions; item_redefines;
                          item_length = _; } x -> x
      >> Cobol_ptree.Terms_visitor.fold_qualname'_opt v item_qualname
      >> Cobol_ptree.Terms_visitor.fold_qualname'_opt v item_redefines
      >> fold_item_layout v item_layout
      >> fold_memory_offset v item_offset
      >> fold_memory_size v item_size
      >> fold_item_redefinitions v item_redefinitions
    end

and fold_item_redefinitions (v: _ #folder) =
  handle v#fold_item_redefinitions
    ~continue:(fold_list v ~fold:fold_item_definition')

and fold_item_layout (v: _ #folder) =
  handle v#fold_item_layout
    ~continue:begin fun l x -> match l with
      | Elementary_item { usage; value } -> x
          >> fold_usage v usage
          >> Cobol_ptree.Terms_visitor.fold_literal'_opt v value
      | Struct_item { fields } -> x
          >> fold_item_definitions v fields
      | Fixed_table { items; length; value } -> x
          >> fold_item_definitions v items
          >> fold_int' v length
          >> Cobol_ptree.Terms_visitor.fold_literal'_opt v value
      | Depending_table { items; min_occurs; max_occurs; depending; value } -> x
          >> fold_item_definitions v items
          >> fold_int' v min_occurs
          >> fold_int' v max_occurs
          >> Cobol_ptree.Terms_visitor.fold_qualname' v depending
          >> Cobol_ptree.Terms_visitor.fold_literal'_opt v value
      | Dynamic_table { items; capacity; min_capacity; max_capacity;
                        value; initialized } -> x
          >> fold_item_definitions v items
          >> Cobol_ptree.Terms_visitor.fold_qualname'_opt v capacity
          >> fold_option v ~fold:fold_int' min_capacity
          >> fold_option v ~fold:fold_int' max_capacity
          >> Cobol_ptree.Terms_visitor.fold_literal'_opt v value
          >> fold' v ~fold:fold_bool initialized
    end

let fold_renamed_item_layout (v: _ #folder) =
  handle v#fold_renamed_item_layout
    ~continue:begin function
      | Renamed_elementary { usage } ->
          fold_usage v usage
      | Renamed_struct { fields } ->
          fold_item_definitions v fields
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
