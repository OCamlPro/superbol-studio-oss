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

(* --- *)

class virtual ['a] folder = object
  inherit ['a] Data_descr_visitor.folder
  inherit! ['a] Operands_visitor.folder
  method fold_working_storage_section: (working_storage_section     , 'a) fold = default
  method fold_linkage_section        : (linkage_section             , 'a) fold = default
  method fold_file_section           : (file_section                , 'a) fold = default
  method fold_communication_section  : (communication_section       , 'a) fold = default
  method fold_local_storage_section  : (local_storage_section       , 'a) fold = default
  method fold_report_section         : (report_section              , 'a) fold = default
  method fold_screen_section         : (screen_section              , 'a) fold = default
  method fold_picture_string         : (picture_string              , 'a) fold = default
  method fold_picture_string'        : (picture_string with_loc     , 'a) fold = default
  method fold_picture_clause         : (picture_clause              , 'a) fold = default
  method fold_picture_clause'        : (picture_clause with_loc     , 'a) fold = default
  method fold_data_clause            : (data_clause                 , 'a) fold = default
  method fold_data_clause'           : (data_clause with_loc        , 'a) fold = default
  method fold_data_item              : (data_item                   , 'a) fold = default
  method fold_data_item'             : (data_item with_loc          , 'a) fold = default
  method fold_constant_item          : (constant_item               , 'a) fold = default
  method fold_constant_item'         : (constant_item with_loc      , 'a) fold = default
  method fold_rename_item            : (rename_item                 , 'a) fold = default
  method fold_rename_item'           : (rename_item with_loc        , 'a) fold = default
  method fold_condition_name_item    : (condition_name_item         , 'a) fold = default
  method fold_condition_name_item'   : (condition_name_item with_loc, 'a) fold = default
  method fold_communication_descr    : (communication_descr         , 'a) fold = default
  method fold_communication_descr'   : (communication_descr with_loc, 'a) fold = default
  method fold_report_group_clause    : (report_group_clause         , 'a) fold = default
  method fold_report_group_clause'   : (report_group_clause with_loc, 'a) fold = default
  method fold_report_group_item      : (report_group_item           , 'a) fold = default
  method fold_report_group_item'     : (report_group_item with_loc  , 'a) fold = default
  method fold_report_descr           : (report_descr                , 'a) fold = default
  method fold_report_descr'          : (report_descr with_loc       , 'a) fold = default
  method fold_screen_clause          : (screen_clause               , 'a) fold = default
  method fold_screen_clause'         : (screen_clause with_loc      , 'a) fold = default
  method fold_screen_item            : (screen_item                 , 'a) fold = default
  method fold_screen_item'           : (screen_item with_loc        , 'a) fold = default
  method fold_file_fd_clause         : (file_fd_clause              , 'a) fold = default
  method fold_file_fd_clause'        : (file_fd_clause with_loc     , 'a) fold = default
  method fold_file_sd_clause         : (file_sd_clause              , 'a) fold = default
  method fold_file_sd_clause'        : (file_sd_clause with_loc     , 'a) fold = default
  method fold_file_clauses           : (file_clauses                , 'a) fold = default
  method fold_file_descr             : (file_descr                  , 'a) fold = default
  method fold_file_descr'            : (file_descr with_loc         , 'a) fold = default
  method fold_exec_declarations      : (exec_declarations           , 'a) fold = default
  method fold_exec_declarations'     : (exec_declarations with_loc  , 'a) fold = default
end

let fold_picture_string (v: _ #folder) =
  leaf v#fold_picture_string

let fold_picture_string' (v: _ #folder) =
  handle' v#fold_picture_string' ~fold:fold_picture_string v

let fold_picture_clause (v: _ #folder) =
  handle v#fold_picture_clause
    ~continue:begin fun { picture_string; picture_locale;
                          picture_depending } x -> x
      >> fold_picture_string v picture_string
      >> fold_option ~fold:Data_descr_visitor.fold_locale_phrase v picture_locale
      >> fold_qualname'_opt v picture_depending
    end

let fold_picture_clause' (v: _ #folder) =
  handle' v#fold_picture_clause' ~fold:fold_picture_clause v

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
      | DataOccurs c -> Data_descr_visitor.fold_data_occurs_clause v c
      | DataValue c -> Data_descr_visitor.fold_data_value_clause v c
      | DataDynamicLength { structure_name; limit } -> fun x -> x
        >> fold_name'_opt v structure_name
        >> fold_integer_opt v limit
      | DataExternal c -> Data_descr_visitor.fold_external_clause v c
      | DataGroupUsage c -> Data_descr_visitor.fold_group_usage_clause v c
      | DataPicture c -> fold_picture_clause' v c
      | DataProperty c -> fold' ~fold:Data_descr_visitor.fold_property_clause v c
      | DataType n
      | DataRedefines n
      | DataSameAs n -> fold_name' v n
      | DataSelectWhen c -> Data_descr_visitor.fold_select_when_clause v c
      | DataSign c -> Data_descr_visitor.fold_sign_clause v c
      | DataSynchronized c -> Data_descr_visitor.fold_synchronized_clause v c
      | DataTypedef { strong } -> fold_bool v strong
      | DataUsage c -> Data_descr_visitor.fold_usage_clause v c
      | DataValidation c -> Data_descr_visitor.fold_validation_clause v c
    end

let fold_data_clause' (v: _ #folder) =
  handle' v#fold_data_clause' ~fold:fold_data_clause v

let fold_data_clauses (v: _ #folder) =
  fold_list ~fold:fold_data_clause' v

let fold_data_item (v: _ #folder) =
  handle v#fold_data_item
    ~continue:begin fun { data_level; data_name; data_clauses } x -> x
      >> Data_descr_visitor.fold_data_level' v data_level
      >> Data_descr_visitor.fold_data_name'_opt v data_name
      >> fold_data_clauses v data_clauses
    end

let fold_data_item' (v: _ #folder) =
  handle' v#fold_data_item' ~fold:fold_data_item v

let fold_constant_item (v: _ #folder) =
  handle v#fold_constant_item
    ~continue:begin fun { constant_level; constant_name;
                          constant_global; constant_value } x -> x
      >> Data_descr_visitor.fold_data_level' v constant_level
      >> fold_name' v constant_name
      >> fold_bool v constant_global
      >> Data_descr_visitor.fold_constant_value' v constant_value
    end

let fold_constant_item' (v: _ #folder) =
  handle' v#fold_constant_item' ~fold:fold_constant_item v

let fold_rename_item (v: _ #folder) =
  handle v#fold_rename_item
    ~continue:begin fun { rename_level; rename_to;
                          rename_from; rename_thru } x -> x
      >> Data_descr_visitor.fold_data_level' v rename_level
      >> fold_name' v rename_to
      >> fold_qualname' v rename_from
      >> fold_qualname'_opt v rename_thru
    end

let fold_rename_item' (v: _ #folder) =
  handle' v#fold_rename_item' ~fold:fold_rename_item v

let fold_condition_name_item (v: _ #folder) =
  handle v#fold_condition_name_item
    ~continue:begin fun { condition_name_level; (*is always 88*)
                          condition_name; condition_name_values;
                          condition_name_alphabet;
                          condition_name_when_false } x -> x
      >> Data_descr_visitor.fold_data_level' v condition_name_level (*or ignore*)
      >> fold_name' v condition_name
      >> fold_list v condition_name_values
        ~fold:Data_descr_visitor.fold_condition_name_value
      >> fold_name'_opt v condition_name_alphabet
      >> fold_literal_opt v condition_name_when_false
    end

let fold_condition_name_item' (v: _ #folder) =
  handle' v#fold_condition_name_item' ~fold:fold_condition_name_item v

let fold_exec_declarations (v: _ #folder) =
  leaf v#fold_exec_declarations

let fold_exec_declarations' (v: _ #folder) =
  handle' v#fold_exec_declarations' ~fold:fold_exec_declarations v

let fold_working_item_descr (v: _ #folder) : working_item_descr -> _ = function
  | Constant c -> fold_constant_item v c
  | Renames r -> fold_rename_item v r
  | CondName c -> fold_condition_name_item v c
  | Data e -> fold_data_item v e
  | Exec e -> fold_exec_declarations v e

let fold_working_item_descr' (v: _ #folder)
  : working_item_descr with_loc -> _ = fun d -> match ~&d with
  | Constant c -> fold_constant_item' v (c &@<- d)
  | Renames r -> fold_rename_item' v (r &@<- d)
  | CondName c -> fold_condition_name_item' v (c &@<- d)
  | Data e -> fold_data_item' v (e &@<- d)
  | Exec e -> fold_exec_declarations' v (e &@<- d)

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

let fold_file_fd_clause (v: _ #folder) =
  handle v#fold_file_fd_clause
    ~continue:begin fun c x -> match c with
      | FileGlobal ->
          x
      | FileFormat c -> x
          >> Data_descr_visitor.fold_format_clause v c
      | FileExternal c -> x
          >> Data_descr_visitor.fold_external_clause v c
      | FileBlockContains { from; to_; contents } -> x
          >> fold_name v from
          >> fold_name_opt v to_
          >> Data_descr_visitor.fold_file_block_contents v contents
      | FileRecord c -> x
          >> Data_descr_visitor.fold_record_clause v c
      | FileRecordingMode m -> x
          >> Data_descr_visitor.fold_recording_mode v m
      | FileLabel c -> x
          >> Data_descr_visitor.fold_label_clause v c
      | FileValueOf c -> x
          >> fold_list ~fold:Data_descr_visitor.fold_valueof_clause v c
      | FileData d -> x
          >> Data_descr_visitor.fold_file_data_clause v d
      | FileLinage l -> x
          >> Data_descr_visitor.fold_file_linage_clause v l
      | FileCodeSet a -> x
          >> Operands_visitor.fold_alphabet_specification v a
      | FileReport c -> x
          >> fold_name'_list v c
    end
let fold_file_fd_clause' (v: _ #folder) =
  handle' v#fold_file_fd_clause' ~fold:fold_file_fd_clause v

let fold_file_sd_clause (v: _ #folder) =
  handle v#fold_file_sd_clause
    ~continue:begin fun c x -> match c with
      | FileSDGlobal ->
          x
      | FileSDRecord c -> x
          >> Data_descr_visitor.fold_record_clause v c
      | FileSDData d -> x
          >> Data_descr_visitor.fold_file_data_clause v d
    end
let fold_file_sd_clause' (v: _ #folder) =
  handle' v#fold_file_sd_clause' ~fold:fold_file_sd_clause v

let fold_file_clauses (v: _ #folder) =
  handle v#fold_file_clauses
    ~continue:begin function
      | FileFD clauses ->
          fold_list ~fold:fold_file_fd_clause' v clauses
      | FileSD clauses ->
          fold_list ~fold:fold_file_sd_clause' v clauses
    end

let fold_file_descr (v: _ #folder) =
  handle v#fold_file_descr
    ~continue:begin fun { file_name; file_clauses; file_items } x -> x
      >> fold_name' v file_name
      >> fold_file_clauses v file_clauses
      >> fold_list ~fold:fold_file_item_descr' v file_items
    end
let fold_file_descr' (v: _ #folder) =
  handle' v#fold_file_descr' ~fold:fold_file_descr v

let fold_file_section (v: _ #folder) =
  handle v#fold_file_section
    ~continue:(fold_list ~fold:fold_file_descr' v)

let fold_communication_item_descr  = fold_working_item_descr
let fold_communication_item_descr' = fold_working_item_descr'
let fold_communication_descr (v: _ #folder) =
  handle v#fold_communication_descr
    ~continue:begin fun { comm_name; comm_clauses;
                          comm_items; comm_direction } x -> x
      >> fold_name' v comm_name
      >> fold_list ~fold:Data_descr_visitor.fold_comm_clause' v comm_clauses
      >> fold_list ~fold:fold_communication_item_descr' v comm_items
      >> Data_descr_visitor.fold_comm_direction v comm_direction
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
    ~continue:begin fun c x -> match c with
      | ReportType c -> x
          >> Data_descr_visitor.fold_report_type_clause v c
      | ReportNextGroup c -> x
          >> Data_descr_visitor.fold_next_group_clause v c
      | ReportLine l -> x
          >> fold_list ~fold:Data_descr_visitor.fold_line_position v l
      | ReportPicture p -> x
          >> fold' ~fold:fold_picture_clause v p
      | ReportUsage c -> x
          >> Data_descr_visitor.fold_report_screen_usage_clause v c
      | ReportSign s -> x
          >> Data_descr_visitor.fold_sign_clause v s
      | ReportJustified
      | ReportBlankWhenZero
      | ReportGroupIndicate ->
          x
      | ReportColumn { alignment; position } -> x
          >> Data_descr_visitor.fold_alignment v alignment
          >> fold_list ~fold:Data_descr_visitor.fold_column_position v position
      | ReportSource { source; rounding } -> x
          >> fold_list ~fold:fold_expr v source
          >> fold_rounding v rounding
      | ReportSum { sum_of; reset_on; rounding } -> x
          >> fold_list v sum_of
            ~fold:Data_descr_visitor.fold_sum_phrase
          >> fold_option v reset_on
            ~fold:Data_descr_visitor.fold_report_data_name_or_final
          >> fold_rounding v rounding
      | ReportValue l -> x
          >> fold_literal v l
      | ReportPresentWhen c -> x
          >> fold_condition v c
      | ReportOccurs { from; to_; depending; step } -> x
          >> fold_name v from
          >> fold_name_opt v to_
          >> fold_qualname'_opt v depending
          >> fold_name_opt v step
      | ReportVarying l -> x
          >> fold_list ~fold:Data_descr_visitor.fold_data_varying v l
    end

let fold_report_group_clause' (v: _ #folder) =
  handle' v#fold_report_group_clause' ~fold:fold_report_group_clause v

let fold_report_group_item (v: _ #folder) =
  handle v#fold_report_group_item
    ~continue:begin fun { report_level; report_data_name;
                          report_group_clauses } x -> x
      >> fold_int v report_level
      >> Data_descr_visitor.fold_data_name'_opt v report_data_name
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

let fold_report_descr (v: _ #folder) =
  handle v#fold_report_descr
    ~continue:begin fun { report_name; report_clauses; report_items } x -> x
      >> fold_name' v report_name
      >> fold_list ~fold:Data_descr_visitor.fold_report_clause' v report_clauses
      >> fold_list ~fold:fold_report_item_descr' v report_items
    end

let fold_report_descr' (v: _ #folder) =
  handle' v#fold_report_descr' ~fold:fold_report_descr v

let fold_report_section (v: _ #folder) =
  handle v#fold_report_section
    ~continue:(fold_list ~fold:fold_report_descr' v)

let fold_screen_clause (v: _ #folder) =
  handle v#fold_screen_clause
    ~continue:(todo __POS__ "fold_screen_clause")

let fold_screen_clause' (v: _ #folder) =
  handle' v#fold_screen_clause' ~fold:fold_screen_clause v

let fold_screen_item (v: _ #folder) =
  handle v#fold_screen_item
    ~continue:begin fun { screen_level; screen_data_name; screen_clauses } x -> x
      >> fold_int v screen_level
      >> Data_descr_visitor.fold_data_name'_opt v screen_data_name
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
