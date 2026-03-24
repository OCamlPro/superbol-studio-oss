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
open Cobol_common.Srcloc.INFIX

module Visitor = Cobol_common.Visitor

(* ignores redefs by default *)
let fold_definitions ?(fold_redefinitions = false) ~field ~table def acc =
  Data_visitor.fold_item_definition' object
    inherit [_] Data_visitor.folder
    method! fold_field_definition' def acc =
      Visitor.do_children (field def acc)
    method! fold_table_definition' def acc =
      Visitor.do_children (table def acc)
    method! fold_usage _ = Visitor.skip
    method! fold_item_redefinitions _ acc =
      if fold_redefinitions
      then Visitor.do_children acc
      else Visitor.skip_children acc
    method! fold_table_range _ = Visitor.skip
    method! fold_fixed_span _ = Visitor.skip
    method! fold_depending_span _ = Visitor.skip
    method! fold_dynamic_span _ = Visitor.skip
    method! fold_condition_names _ = Visitor.skip
    method! fold_memory_offset _ = Visitor.skip
    method! fold_memory_size _ = Visitor.skip
    method! fold_qualname' _ = Visitor.skip
  end def acc

let offset: item_definition -> Data_memory.offset = function
  | Field f -> f.field_offset
  | Table t -> t.table_offset

let size: item_definition -> Data_memory.size = function
  | Field f -> f.field_size
  | Table t -> t.table_size

let qualname = function
  | Field { field_qualname; _ } -> field_qualname
  | Table _ -> None

(** Note: may be a no-op *)
let pp_item_qualname ?(leading = Fmt.nop) ppf item =
  Fmt.(option (leading ++ Cobol_ptree.pp_qualname')) ppf (qualname item)

let def_loc = function
  | Data_field { def; _} -> ~@def
  | Data_renaming { def; _} -> ~@def
  | Data_condition { def; _} -> ~@def
  | Table_index { table; _ } -> ~@table

let def_qualname = function
  | Data_field { def; _ } -> begin
    match ~&def.field_qualname with
        | None -> None
        | Some qualname' -> Some ~&qualname' end
  | Data_renaming { def; _ } ->
      Some ~&(~&def.renaming_name)
  | Data_condition { def; _ } ->
      Some ~&(~&def.condition_name_qualname)
  | Table_index { qualname; _ } ->
      Some ~&qualname

let def_record: data_definition -> record = function
  | Data_field { record; _}
  | Data_renaming { record; _}
  | Data_condition { record; _}
  | Table_index { record; _ } -> record

let def_storage: data_definition -> data_storage = fun def ->
  (def_record def).record_storage

let def_size: data_definition -> Data_memory.size = function
  | Data_field { def; _} -> ~&def.field_size
  | Data_renaming { def; _} -> ~&def.renaming_size
  | Data_condition { field; _} -> ~&field.field_size
  | Table_index { table; _ } -> ~&table.table_size

let def_offset: data_definition -> Data_memory.offset = function
  | Data_field { def; _} -> ~&def.field_offset
  | Data_renaming { def; _} -> ~&def.renaming_offset
  | Data_condition { field; _} -> ~&field.field_offset
  | Table_index { table; _ } -> ~&table.table_offset
