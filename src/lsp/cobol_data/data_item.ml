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
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module Visitor = Cobol_common.Visitor

(* ignores redefs by default *)
let fold_definitions ?(fold_redefinitions = false) ~field ~table ?visit_item
    def acc =
  Data_visitor.fold_item_definition' object
    inherit [_] Data_visitor.folder
    method! fold_field_definition' def acc =
      Visitor.do_children (field def acc)
    method! fold_table_definition' def acc =
      Visitor.do_children (table def acc)
    method! fold_usage _ = Visitor.skip
    method! fold_item_definition' def acc =
      match visit_item with
      | None -> Visitor.do_children acc
      | Some f -> f def acc
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

let record_size: record -> Data_memory.size = fun r ->
  size ~&(r.record_item)

(* Same as [qualname], but a table takes the name of the field it contains. *)
let item_qualname: item_definition -> Cobol_ptree.qualname with_loc option =
  function
  | Table { table_field; _ } -> ~&table_field.field_qualname
  | item -> qualname item

let redefines: item_definition -> Cobol_ptree.qualname with_loc option =
  function
  | Field { field_redefines; _ } -> field_redefines
  | Table { table_redefines; _ } -> table_redefines

let redefinitions: item_definition -> item_redefinitions = function
  | Field { field_redefinitions; _ } -> field_redefinitions
  | Table { table_redefinitions; _ } -> table_redefinitions

(** Note: may be a no-op *)
let pp_item_qualname ?(leading = Fmt.nop) ppf item =
  Fmt.(option (leading ++ Cobol_ptree.pp_qualname')) ppf (qualname item)

let def_loc: data_definition -> srcloc = function
  | Data_field { def; _ } -> ~@def
  | Data_renaming { def; _ } -> ~@def
  | Data_condition { def; _ } -> ~@def
  | Table_index { table; _ } -> ~@table

let def_qualname = function
  | Data_field { def = { payload = { field_qualname = Some qn'; _ }; _ }; _ } ->
      Some ~&qn'
  | Data_field { def = { payload = { field_qualname = None; _ }; _ }; _ } ->
      None
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

(* Item of a definition. A field with an OCCURS clause gives its table, not the
   field it contains. *)
let def_item: data_definition -> item_definition option = function
  | Data_field { table_def = Some table; _ } -> Some (Table ~&table)
  | Data_field { def; _ } -> Some (Field ~&def)
  | Data_renaming _ | Data_condition _ | Table_index _ -> None

(* Item redefined by [def], if any. [main_def] is also set on the items below a
   redefined one, so we read it only on an item with a REDEFINES clause. *)
let def_redefined: data_definition -> item_definition option = fun def ->
  match def, def_item def with
  | Data_field { main_def = Some item; _ }, Some own
    when redefines own <> None ->
      Some ~&item
  | _ ->
      None

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

let def_has_issues: data_definition -> bool = function
  | Data_field { def; _ } -> ~&def.field_has_definition_issues
  | Data_renaming { def; _ } -> ~&def.renaming_has_definition_issues
  | Data_condition { field; _ } -> ~&field.field_has_definition_issues
  | Table_index { table; _ } -> ~&table.table_has_definition_issues

let def_leading_ranges: data_definition -> table_range list = function
  | Data_field { def; _} -> ~&def.field_leading_ranges
  | Data_renaming _ -> []
  | Data_condition { field; _} -> ~&field.field_leading_ranges
  | Table_index _ -> []
