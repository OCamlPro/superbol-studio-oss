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

open Types

(* ignores redefs by default *)
let fold_definitions ?(fold_redefinitions = false) ~field ~table def acc =
  Visitor.fold_item_definition' object
    inherit [_] Visitor.folder
    method! fold_field_definition' def acc =
      Cobol_common.Visitor.do_children (field def acc)
    method! fold_table_definition' def acc =
      Cobol_common.Visitor.do_children (table def acc)
    method! fold_usage _ = Cobol_common.Visitor.skip
    method! fold_item_redefinitions _ acc =
      if fold_redefinitions
      then Cobol_common.Visitor.do_children acc
      else Cobol_common.Visitor.skip_children acc
    method! fold_table_range _ = Cobol_common.Visitor.skip
    method! fold_fixed_span _ = Cobol_common.Visitor.skip
    method! fold_depending_span _ = Cobol_common.Visitor.skip
    method! fold_dynamic_span _ = Cobol_common.Visitor.skip
    method! fold_condition_names _ = Cobol_common.Visitor.skip
    method! fold_memory_offset _ = Cobol_common.Visitor.skip
    method! fold_memory_size _ = Cobol_common.Visitor.skip
    method! fold_qualname' _ = Cobol_common.Visitor.skip
  end def acc

let offset: item_definition -> Memory.offset = function
  | Field f -> f.field_offset
  | Table t -> t.table_offset

let size: item_definition -> Memory.size = function
  | Field f -> f.field_size
  | Table t -> t.table_size

let qualname = function
  | Field { field_qualname; _ } -> field_qualname
  | Table _ -> None

(** Note: may be a no-op *)
let pp_item_qualname ?(leading = Fmt.nop) ppf item =
  Fmt.(option (leading ++ Cobol_ptree.Types.pp_qualname')) ppf (qualname item)
