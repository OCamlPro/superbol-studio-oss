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

open PTree_types

open Cobol_common.Srcloc.INFIX
open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)
open Terms_visitor
open Data_descr_visitor

let todo    x = Cobol_common.Visitor.todo    __FILE__ x
let partial x = Cobol_common.Visitor.partial __FILE__ x

(* --- *)

class virtual ['a] folder = object
  inherit ['a] Data_descr_visitor.folder
  method fold_working_storage_section: (working_storage_section, 'a) fold = default
  method fold_linkage_section        : (linkage_section        , 'a) fold = default
  method fold_file_section           : (file_section           , 'a) fold = default
  method fold_communication_section  : (communication_section  , 'a) fold = default
  method fold_local_storage_section  : (local_storage_section  , 'a) fold = default
  method fold_report_section         : (report_section         , 'a) fold = default
  method fold_screen_section         : (screen_section         , 'a) fold = default
  method fold_data_level          : (data_level                  , 'a) fold = default
  method fold_data_level'         : (data_level with_loc         , 'a) fold = default
  method fold_data_name           : (data_name                   , 'a) fold = default
  method fold_data_name'          : (data_name with_loc          , 'a) fold = default
  method fold_constant_value      : (constant_value              , 'a) fold = default
  method fold_constant_value'     : (constant_value with_loc     , 'a) fold = default
  method fold_constant_item       : (constant_item               , 'a) fold = default
  method fold_constant_item'      : (constant_item with_loc      , 'a) fold = default
  method fold_rename_item         : (rename_item                 , 'a) fold = default
  method fold_rename_item'        : (rename_item with_loc        , 'a) fold = default
  method fold_condition_name_value: (condition_name_value        , 'a) fold = default
  method fold_condition_name_item : (condition_name_item         , 'a) fold = default
  method fold_condition_name_item': (condition_name_item with_loc, 'a) fold = default
  method fold_picture             : (picture                     , 'a) fold = default
  method fold_picture'            : (picture with_loc            , 'a) fold = default
  method fold_picture_clause      : (picture_clause              , 'a) fold = default
  method fold_picture_clause'     : (picture_clause with_loc     , 'a) fold = default
  method fold_data_clause         : (data_clause                 , 'a) fold = default
  method fold_data_clause'        : (data_clause with_loc        , 'a) fold = default
  method fold_data_item           : (data_item                   , 'a) fold = default
  method fold_data_item'          : (data_item with_loc          , 'a) fold = default
  method fold_comm_channel        : (comm_channel                , 'a) fold = default
  method fold_comm_clause         : (comm_clause                 , 'a) fold = default
  method fold_comm_clause'        : (comm_clause with_loc        , 'a) fold = default
  method fold_comm_direction      : (comm_direction              , 'a) fold = default
  method fold_communication_descr : (communication_descr         , 'a) fold = default
  method fold_communication_descr': (communication_descr with_loc, 'a) fold = default
  method fold_report_group_clause : (report_group_clause         , 'a) fold = default
  method fold_report_group_clause': (report_group_clause with_loc, 'a) fold = default
  method fold_report_group_item   : (report_group_item           , 'a) fold = default
  method fold_report_group_item'  : (report_group_item with_loc  , 'a) fold = default
  method fold_report_clause       : (report_clause               , 'a) fold = default
  method fold_report_clause'      : (report_clause with_loc      , 'a) fold = default
  method fold_report_descr        : (report_descr                , 'a) fold = default
  method fold_report_descr'       : (report_descr with_loc       , 'a) fold = default
  method fold_screen_clause       : (screen_clause               , 'a) fold = default
  method fold_screen_clause'      : (screen_clause with_loc      , 'a) fold = default
  method fold_screen_item         : (screen_item                 , 'a) fold = default
  method fold_screen_item'        : (screen_item with_loc        , 'a) fold = default
  method fold_data_occurs_clause  : (data_occurs_clause          , 'a) fold = default
  method fold_sort_spec           : (sort_spec                   , 'a) fold = default
  method fold_sort_direction      : (sort_direction              , 'a) fold = default
end

let todo    l f = todo    __MODULE__ l f
and partial l f = partial __MODULE__ l f

let fold_data_level (v: _ #folder) =
  leaf v#fold_data_level

let fold_data_level' (v: _ #folder) =
  handle' v#fold_data_level' ~fold:fold_data_level v

let fold_data_name (v: _ #folder) =
  handle v#fold_data_name
    ~continue:(function DataName n -> fold_name' v n | DataFiller -> Fun.id)

let fold_data_name' (v: _ #folder) =
  handle v#fold_data_name'
    ~continue:(fold' ~fold:fold_data_name v)

let fold_data_name'_opt (v: _ #folder) =
  fold_option ~fold:fold_data_name' v

let fold_locale_phrase_opt (v: _ #folder) =
  fold_option ~fold:fold_locale_phrase v

let fold_constant_value (v: _ #folder) =
  handle v#fold_constant_value
    ~continue:begin function
      | ConstExpr e -> fold_expr v e
      | ConstByteLength n | ConstLength n | ConstFrom n -> fold_name' v n
    end

let fold_constant_value' (v: _ #folder) =
  handle' v#fold_constant_value' ~fold:fold_constant_value v

let fold_constant_item (v: _ #folder) =
  handle v#fold_constant_item
    ~continue:begin fun { constant_level; constant_name;
                          constant_global; constant_value } x -> x
      >> fold_data_level' v constant_level
      >> fold_name' v constant_name
      >> fold_bool v constant_global
      >> fold_constant_value' v constant_value
    end

let fold_constant_item' (v: _ #folder) =
  handle' v#fold_constant_item' ~fold:fold_constant_item v

let fold_rename_item (v: _ #folder) =
  handle v#fold_rename_item
    ~continue:begin fun { rename_level; rename_to;
                          rename_renamed; rename_through } x -> x
      >> fold_data_level' v rename_level
      >> fold_name' v rename_to
      >> fold_qualname v rename_renamed
      >> fold_qualname_opt v rename_through
    end

let fold_rename_item' (v: _ #folder) =
  handle' v#fold_rename_item' ~fold:fold_rename_item v

let fold_condition_name_value (v: _ #folder) =
  handle v#fold_condition_name_value
    ~continue:begin fun { condition_name_value; condition_name_through } x -> x
      >> fold_literal v condition_name_value
      >> fold_literal_opt v condition_name_through
    end

let fold_condition_name_item (v: _ #folder) =
  handle v#fold_condition_name_item
    ~continue:begin fun { condition_name_level; (*is always 88*)
                          condition_name; condition_name_values;
                          condition_name_alphabet;
                          condition_name_when_false } x -> x
      >> fold_data_level' v condition_name_level (*or ignore*)
      >> fold_name' v condition_name
      >> fold_list ~fold:fold_condition_name_value v condition_name_values
      >> fold_name'_opt v condition_name_alphabet
      >> fold_literal_opt v condition_name_when_false
    end

let fold_condition_name_item' (v: _ #folder) =
  handle' v#fold_condition_name_item' ~fold:fold_condition_name_item v

let fold_picture (v: _ #folder) =
  leaf v#fold_picture

let fold_picture' (v: _ #folder) =
  handle' v#fold_picture' ~fold:fold_picture v

let fold_picture_clause (v: _ #folder) =
  handle v#fold_picture_clause
    ~continue:begin fun { picture; picture_locale; picture_depending } x -> x
      >> fold_picture v picture
      >> fold_locale_phrase_opt v picture_locale
      >> fold_qualname'_opt v picture_depending
    end

let fold_picture_clause' (v: _ #folder) =
  handle' v#fold_picture_clause' ~fold:fold_picture_clause v

let fold_sort_direction (v: _ #folder) =
  leaf v#fold_sort_direction

let fold_sort_spec (v: _ #folder) =
  handle v#fold_sort_spec
    ~continue:begin fun { sort_key_direction; sort_key_names } x -> x
      >> fold_sort_direction v sort_key_direction
      >> fold_list ~fold:fold_qualname v sort_key_names
    end

let fold_data_occurs_clause (v: _ #folder) =
  handle v#fold_data_occurs_clause
    ~continue:begin fun c x -> match c with
      | OccursFixed { times; key_is; indexed_by } -> x
          >> fold_integer v times
          >> fold_list ~fold:fold_sort_spec v key_is
          >> fold_name'_list v indexed_by
      | OccursDepending { from; to_; depending;
                          key_is; indexed_by } -> x
          >> fold_integer v from
          >> fold_integer v to_
          >> fold_qualname' v depending
          >> fold_list ~fold:fold_sort_spec v key_is
          >> fold_name'_list v indexed_by
      | OccursDynamic { capacity_in; from; to_;
                        initialized; key_is; indexed_by } -> x
          >> fold_name'_opt v capacity_in
          >> fold_integer_opt v from
          >> fold_integer_opt v to_
          >> fold_bool v initialized
          >> fold_list ~fold:fold_sort_spec v key_is
          >> fold_name'_list v indexed_by
    end

let fold_data_clause (v: _ #folder) =
  handle v#fold_data_clause
    ~continue:begin function
      | DataAligned
      | DataAnyLength
      | DataBased
      | DataBlankWhenZero
      | DataConstantRecord
      | DataGlobal
      | DataJustified -> Fun.id
      | DataOccurs c -> fold_data_occurs_clause v c
      | DataRedefines n
      | DataType n
      | DataSameAs n -> fold_name' v n
      | _ -> partial __LINE__ "fold_data_clause"
    end

let fold_data_clause' (v: _ #folder) =
  handle' v#fold_data_clause' ~fold:fold_data_clause v

let fold_data_clauses (v: _ #folder) =
  fold_list ~fold:fold_data_clause' v

let fold_data_item (v: _ #folder) =
  handle v#fold_data_item
    ~continue:begin fun { data_level; data_name; data_clauses } x -> x
      >> fold_data_level' v data_level
      >> fold_data_name'_opt v data_name
      >> fold_data_clauses v data_clauses
    end

let fold_data_item' (v: _ #folder) =
  handle' v#fold_data_item' ~fold:fold_data_item v

let fold_working_item_descr (v: _ #folder) : working_item_descr -> _ = function
  | Constant c -> fold_constant_item v c
  | Renames r -> fold_rename_item v r
  | CondName c -> fold_condition_name_item v c
  | Data e -> fold_data_item v e

let fold_working_item_descr' (v: _ #folder)
  : working_item_descr with_loc -> _ = fun d -> match ~&d with
  | Constant c -> fold_constant_item' v (c &@<- d)
  | Renames r -> fold_rename_item' v (r &@<- d)
  | CondName c -> fold_condition_name_item' v (c &@<- d)
  | Data e -> fold_data_item' v (e &@<- d)

let fold_working_storage_item_descr  = fold_working_item_descr
let fold_working_storage_item_descr' = fold_working_item_descr'
let fold_working_storage_section (v: _ #folder) =
  handle v#fold_working_storage_section
    ~continue:(fold_list ~fold:fold_working_storage_item_descr' v)

let fold_linkage_item_descr  = fold_working_item_descr
let fold_linkage_item_descr' = fold_working_item_descr'
let fold_linkage_section (v: _ #folder) =
  handle v#fold_linkage_section
    ~continue:(fold_list ~fold:fold_linkage_item_descr' v)

let fold_file_item_descr  = fold_working_item_descr
let fold_file_item_descr' = fold_working_item_descr'
let fold_file_section (v: _ #folder) =
  handle v#fold_file_section
    ~continue:(todo __LINE__ "fold_file_section")

let fold_comm_channel (v: _ #folder) =
  leaf v#fold_comm_channel

let fold_comm_clause (v: _ #folder) =
  handle v#fold_comm_clause
    ~continue:begin fun c x -> match c with
      | CommSymbolic (c, n) -> x
          >> fold_comm_channel v c
          >> fold_name' v n
      | CommDestinationTable (i, nl) -> x
          >> fold_integer v i
          >> fold_name'_list v nl
      | CommDestinationCount n
      | CommMessageCount n
      | CommMessageDate n
      | CommMessageTime n
      | CommTextLength n
      | CommStatusKey n
      | CommEndKey n
      | CommErrorKey n -> x
          >> fold_name' v n
    end

let fold_comm_clause' (v: _ #folder) =
  handle' v#fold_comm_clause' ~fold:fold_comm_clause v

let fold_comm_direction (v: _ #folder) =
  handle v#fold_comm_direction
    ~continue:begin fun d x -> match d with
      | CommOutput -> x
      | CommInput { initial; items } -> x
          >> fold_bool v initial
          >> fold_list ~fold:fold_data_name' v items
      | CommIO { initial; items } -> x
          >> fold_bool v initial
          >> fold_name'_list v items
    end

let fold_communication_item_descr  = fold_working_item_descr
let fold_communication_item_descr' = fold_working_item_descr'
let fold_communication_descr (v: _ #folder) =
  handle v#fold_communication_descr
    ~continue:begin fun { comm_name; comm_clauses;
                          comm_items; comm_direction } x -> x
      >> fold_name' v comm_name
      >> fold_list ~fold:fold_comm_clause' v comm_clauses
      >> fold_list ~fold:fold_communication_item_descr' v comm_items
      >> fold_comm_direction v comm_direction
    end

let fold_communication_descr' (v: _ #folder) =
  handle' v#fold_communication_descr' ~fold:fold_communication_descr v

let fold_communication_section (v: _ #folder) =
  handle v#fold_communication_section
    ~continue:(fold_list ~fold:fold_communication_descr' v)

let fold_local_storage_item_descr  = fold_working_item_descr
let fold_local_storage_item_descr' = fold_working_item_descr'
let fold_local_storage_section (v: _ #folder) =
  handle v#fold_local_storage_section
    ~continue:(fold_list ~fold:fold_local_storage_item_descr' v)

let fold_report_group_clause (v: _ #folder) =
  handle v#fold_report_group_clause
    ~continue:(todo __LINE__ "fold_report_group_clause")

let fold_report_group_clause' (v: _ #folder) =
  handle' v#fold_report_group_clause' ~fold:fold_report_group_clause v

let fold_report_group_item (v: _ #folder) =
  handle v#fold_report_group_item
    ~continue:begin fun { report_level; report_data_name;
                          report_group_clauses } x -> x
      >> fold_int v report_level
      >> fold_data_name'_opt v report_data_name
      >> fold_list ~fold:fold_report_group_clause' v report_group_clauses
    end

let fold_report_group_item' (v: _ #folder) =
  handle' v#fold_report_group_item' ~fold:fold_report_group_item v

let fold_report_item_descr (v: _ #folder) : report_item_descr -> _ = function
  | Constant c -> fold_constant_item v c
  | ReportGroup r -> fold_report_group_item v r

let fold_report_item_descr' (v: _ #folder)
  : report_item_descr with_loc -> _ = fun d -> match ~&d with
  | Constant c -> fold_constant_item' v (c &@<- d)
  | ReportGroup r -> fold_report_group_item' v (r &@<- d)

let fold_report_clause (v: _ #folder) =
  handle v#fold_report_clause
    ~continue:begin fun r x -> match r with
      | Global -> x
      | Code i -> fold_ident v i x
      | Control { final; controls } -> x
          >> fold_bool v final
          >> fold_name'_list v controls
      | PageLimit { lines; columns; heading; first_detail;
                    last_control_heading; last_detail; footing } -> x
          >> fold_integer_opt v lines
          >> fold_integer_opt v columns
          >> fold_integer_opt v heading
          >> fold_integer_opt v first_detail
          >> fold_integer_opt v last_control_heading
          >> fold_integer_opt v last_detail
          >> fold_integer_opt v footing
    end

let fold_report_clause' (v: _ #folder) =
  handle' v#fold_report_clause' ~fold:fold_report_clause v

let fold_report_descr (v: _ #folder) =
  handle v#fold_report_descr
    ~continue:begin fun { report_name; report_clauses; report_items } x -> x
      >> fold_name' v report_name
      >> fold_list ~fold:fold_report_clause' v report_clauses
      >> fold_list ~fold:fold_report_item_descr' v report_items
    end

let fold_report_descr' (v: _ #folder) =
  handle' v#fold_report_descr' ~fold:fold_report_descr v

let fold_report_section (v: _ #folder) =
  handle v#fold_report_section
    ~continue:(fold_list ~fold:fold_report_descr' v)

let fold_screen_clause (v: _ #folder) =
  handle v#fold_screen_clause
    ~continue:(todo __LINE__ "fold_screen_clause")

let fold_screen_clause' (v: _ #folder) =
  handle' v#fold_screen_clause' ~fold:fold_screen_clause v

let fold_screen_item (v: _ #folder) =
  handle v#fold_screen_item
    ~continue:begin fun { screen_level; screen_data_name; screen_clauses } x -> x
      >> fold_int v screen_level
      >> fold_data_name'_opt v screen_data_name
      >> fold_list ~fold:fold_screen_clause' v screen_clauses
    end

let fold_screen_item' (v: _ #folder) =
  handle' v#fold_screen_item' ~fold:fold_screen_item v

let fold_screen_item_descr (v: _ #folder) : screen_item_descr -> _ = function
  | Constant c -> fold_constant_item v c
  | Screen s -> fold_screen_item v s

let fold_screen_item_descr' (v: _ #folder)
  : screen_item_descr with_loc -> _ = fun d -> match ~&d with
  | Constant c -> fold_constant_item' v (c &@<- d)
  | Screen s -> fold_screen_item' v (s &@<- d)

let fold_screen_section (v: _ #folder) =
  handle v#fold_screen_section
    ~continue:(fold_list ~fold:fold_screen_item_descr' v)
