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

open Ast

module Misc_sections = struct
  type informational_paragraphs        = Ast.informational_paragraphs
  let pp_informational_paragraphs      = Ast.pp_informational_paragraphs
  let compare_informational_paragraphs = Ast.compare_informational_paragraphs
  type options_paragraph               = Ast.options_paragraph
  let pp_options_paragraph             = Ast.pp_options_paragraph
  let compare_options_paragraph        = Ast.compare_options_paragraph
  type environment_division            = Ast.environment_division
  let pp_environment_division          = Ast.pp_environment_division
  let compare_environment_division     = Ast.compare_environment_division
end

module Data_sections (Picture: Abstract.PICTURE) = struct
  include Picture

  type picture_clause =
    {
      picture: Picture.picture;
      picture_locale: locale_phrase option;
      picture_depending: qualname with_loc option;
    }
  [@@deriving ord]

  let pp_picture_clause ppf { picture; picture_locale; picture_depending } =
    Fmt.pf ppf "PIC %a%a%a"
      pp_picture picture
      Fmt.(option (sp ++ pp_locale_phrase)) picture_locale
      Fmt.(
        option (any "@ DEPENDING ON@ " ++ pp_with_loc pp_qualname)
      ) picture_depending

  (* TODO (can be delayed): Unify some of the types (probably like for terms) *)

  type data_clause =
    | DataAligned                                                 (* +COB2002 *)
    | DataAnyLength                                               (* +COB2002 *)
    | DataBased                                                   (* +COB2002 *)
    | DataBlankWhenZero
    | DataConstantRecord                                          (* +COB2002 *)
    | DataOccurs of data_occurs_clause
    | DataType of name with_loc                                   (* +COB2002 *)
    | DataValue of data_value_clause
    | DataDynamicLength of                                        (* +COB2002 *)
        {
          dynamic_length_structure_name: name with_loc option;
          limit_is: integer option;
        }
    | DataExternal of external_clause
    | DataGlobal
    | DataGroupUsage of group_usage_clause                        (* +COB2002 *)
    | DataJustified
    | DataPicture of picture_clause with_loc
    | DataProperty of property_clause with_loc                    (* +COB2002 *)
    | DataRedefines of name with_loc
    | DataSameAs of name with_loc                                 (* +COB2002 *)
    | DataSelectWhen of select_when_clause                        (* +COB2002 *)
    | DataSign of sign_clause
    | DataSynchronized of synchronized_clause
    | DataTypedef of                                              (* +COB2002 *)
        {
          strong: bool;
        }
    | DataUsage of usage_clause
    | DataValidation of validation_clause                         (* +COB2002 *)
  [@@deriving ord]

  let pp_data_clause ppf = function
    | DataAligned -> Fmt.pf ppf "ALIGNED"
    | DataAnyLength -> Fmt.pf ppf "ANY LENGTH"
    | DataBased -> Fmt.pf ppf "BASED"
    | DataBlankWhenZero -> Fmt.pf ppf "BLANK WHEN ZERO"
    | DataConstantRecord -> Fmt.pf ppf "CONSTANT RECORD"
    | DataOccurs doc -> pp_data_occurs_clause ppf doc
    | DataType n -> Fmt.pf ppf "TYPE %a" (pp_with_loc pp_name) n
    | DataValue dvc -> pp_data_value_clause ppf dvc
    | DataDynamicLength { dynamic_length_structure_name = sn; limit_is = l } ->
      Fmt.pf ppf "DYNAMIC%a%a"
        Fmt.(option (sp ++ pp_with_loc pp_name)) sn
        Fmt.(option (any "@ LIMIT IS@ " ++ pp_integer)) l
    | DataExternal ec -> pp_external_clause ppf ec
    | DataGlobal -> Fmt.pf ppf "GLOBAL"
    | DataGroupUsage guc -> pp_group_usage_clause ppf guc
    | DataJustified -> Fmt.pf ppf "JUSTIFIED"
    | DataPicture pc -> pp_with_loc pp_picture_clause ppf pc
    | DataProperty pc -> pp_with_loc pp_property_clause ppf pc
    | DataRedefines n -> Fmt.pf ppf "REDEFINES %a" pp_name' n
    | DataSameAs n -> Fmt.pf ppf "SAME AS %a" pp_name' n
    | DataSelectWhen swc -> pp_select_when_clause ppf swc
    | DataSign sc -> pp_sign_clause ppf sc
    | DataSynchronized sc -> pp_synchronized_clause ppf sc
    | DataTypedef { strong } ->
      Fmt.pf ppf "TYPEDEF";
      if strong then Fmt.pf ppf " STRONG"
    | DataUsage uc -> pp_usage_clause ppf uc
    | DataValidation vc -> pp_validation_clause ppf vc

  type report_group_clause =
    | ReportType of report_type_clause
    | ReportNextGroup of next_group_clause
    | ReportLine of line_position list
    | ReportPicture of picture_clause with_loc
    | ReportUsage of report_screen_usage_clause
    | ReportSign of sign_clause
    | ReportJustified
    | ReportColumn of
        {
          alignment: alignment;
          position: column_position list;                        (* non-empty *)
        }
    | ReportBlankWhenZero
    | ReportSource of
        {
          source: expression list;                               (* non-empty *)
          rounding: rounding;
        }
    | ReportSum of
        {
          sum_of: sum_phrase list;                               (* non-empty *)
          reset_on: report_data_name_or_final option;
          rounding: rounding;
        }
    | ReportValue of literal
    | ReportPresentWhen of condition                              (* +COB2002 *)
    | ReportGroupIndicate
    | ReportOccurs of                                             (* +COB2002 *)
       {
         from: integer;
         to_: integer option;
         depending: qualname with_loc option;
         step: integer option;
       }
    | ReportVarying of data_varying list                          (* +COB2002 *)
  [@@deriving ord]

  let pp_step_phrase ppf step =
    Fmt.pf ppf "STEP %a" pp_integer step

  let pp_report_group_clause ppf = function
    | ReportType rtc -> pp_report_type_clause ppf rtc
    | ReportNextGroup c -> pp_next_group_clause ppf c
    | ReportLine [ lp ] -> Fmt.pf ppf "LINE NUMBER %a" pp_line_position lp
    | ReportLine l ->
      Fmt.pf ppf "LINE NUMBERS %a"
        Fmt.(list ~sep:sp pp_line_position) l
    | ReportPicture pc -> pp_with_loc pp_picture_clause ppf pc
    | ReportUsage c -> pp_report_screen_usage_clause ppf c
    | ReportSign c -> pp_sign_clause ppf c
    | ReportJustified -> Fmt.pf ppf "JUSTIFIED"
    | ReportColumn { alignment; position } ->
      Fmt.pf ppf "COLUMN %a %a"
        pp_alignment alignment
        Fmt.(list ~sep:sp pp_column_position) position
    | ReportBlankWhenZero -> Fmt.pf ppf "BLANK WHEN ZERO"
    | ReportSource { source; rounding } ->
      Fmt.pf ppf "SOURCE %a %a"
        Fmt.(list ~sep:sp pp_expression) source
        pp_rounding rounding
    | ReportSum { sum_of; reset_on; rounding } ->
      Fmt.(list ~sep:nop (hbox pp_sum_phrase ++ sp)) ppf sum_of;
      Fmt.(option (any "RESET " ++ pp_report_data_name_or_final ++ sp))
        ppf reset_on;
      pp_rounding ppf rounding
    | ReportValue v -> Fmt.pf ppf "VALUE %a" pp_literal v
    | ReportPresentWhen c -> Fmt.pf ppf "PRESENT WHEN %a" pp_condition c
    | ReportGroupIndicate -> Fmt.pf ppf "GROUP"
    | ReportOccurs { from; to_; depending; step } ->
      Fmt.pf ppf "OCCURS %a%a%a%a"
        pp_integer from
        Fmt.(option (any "@ TO " ++ pp_integer)) to_
        Fmt.(option (sp ++ pp_depending_phrase)) depending
        Fmt.(option (sp ++ pp_step_phrase)) step
    | ReportVarying dvs -> pp_varying_clause ppf dvs

  type screen_clause =
    | ScreenAuto
    | ScreenBlank of blank_clause
    | ScreenBlankWhenZero
    | ScreenErase of erase_clause
    | ScreenFull
    | ScreenGlobal
    | ScreenJustified
    | ScreenPicture of picture_clause with_loc
    | ScreenUsage of report_screen_usage_clause
    | ScreenRequired
    | ScreenAttribute of screen_attribute_clause with_loc list   (* non-empty *)
    | ScreenColumn of screen_line_column_clause
    | ScreenLine of screen_line_column_clause
    | ScreenOccurs of integer
    | ScreenSecure
    | ScreenSign of sign_clause
    | ScreenSourceDestination of source_destination_clause with_loc list (* non-empty *)
  [@@deriving ord]

  let pp_screen_clause ppf = function
    | ScreenAuto -> Fmt.pf ppf "AUTO"
    | ScreenBlank bc -> pp_blank_clause ppf bc
    | ScreenBlankWhenZero -> Fmt.pf ppf "BLANK WHEN ZERO"
    | ScreenErase ec -> pp_erase_clause ppf ec
    | ScreenFull -> Fmt.pf ppf "FULL"
    | ScreenGlobal -> Fmt.pf ppf "GLOBAL"
    | ScreenJustified -> Fmt.pf ppf "JUSTIFIED"
    | ScreenPicture pc -> pp_with_loc pp_picture_clause ppf pc
    | ScreenUsage rsuc -> pp_report_screen_usage_clause ppf rsuc
    | ScreenRequired -> Fmt.pf ppf "REQUIRED"
    | ScreenAttribute sacs ->
      Fmt.(list ~sep:sp (pp_with_loc pp_screen_attribute_clause)) ppf sacs
    | ScreenColumn slcc ->
      Fmt.pf ppf "COLUMN %a" pp_screen_line_column_clause slcc
    | ScreenLine slcc ->
      Fmt.pf ppf "LINE %a" pp_screen_line_column_clause slcc
    | ScreenOccurs n -> Fmt.pf ppf "OCCURS %a TIMES" pp_integer n
    | ScreenSecure -> Fmt.pf ppf "SECURE"
    | ScreenSign sc -> pp_sign_clause ppf sc
    | ScreenSourceDestination sdcl ->
      Fmt.(list ~sep:sp (pp_with_loc pp_source_destination_clause)) ppf sdcl

  let pp_item pp_clause ppf lvl name clauses =
    Fmt.pf ppf "%a%a%a."
      pp_data_level lvl
      Fmt.(option (sp ++ pp_with_loc pp_data_name)) name
      Fmt.(list ~sep:nop (sp ++ pp_clause)) clauses

  type data_item =
    {
      data_level: data_level with_loc;
      data_name: data_name with_loc option;
      data_clauses: data_clause with_loc list;
    }
  [@@deriving ord]

  let pp_data_item ppf { data_level = lvl; data_name = nc; data_clauses = cls } =
    pp_item (pp_with_loc pp_data_clause) ppf lvl.payload nc cls

  type screen_item =
    {
      screen_level: int;
      screen_data_name: data_name with_loc option;
      screen_clauses: screen_clause with_loc list;
    }
  [@@deriving ord]

  let pp_screen_item ppf { screen_level; screen_data_name; screen_clauses } =
    pp_item
      (pp_with_loc pp_screen_clause) ppf
      screen_level screen_data_name screen_clauses

  type report_group_item =
    {
      report_level: int;
      report_data_name: data_name with_loc option;
      report_group_clauses: report_group_clause with_loc list;
    }
  [@@deriving ord]

  let pp_report_group_item ppf
    { report_level; report_data_name; report_group_clauses }
  =
    pp_item
      (pp_with_loc pp_report_group_clause) ppf
      report_level report_data_name report_group_clauses

  type data_ = [`data]
  type constant_ = [`const]
  type rename_ = [`rename]
  type condition_name_ = [`condition_name]
  type screen_ = [`screen]
  type report_group_ = [`report_group]
  type _ item_descr =
    | Constant: constant_item -> [>constant_] item_descr
    | Data: data_item -> [>data_] item_descr
    | Renames: rename_item -> [>rename_] item_descr
    | CondName: condition_name_item -> [>condition_name_] item_descr
    | Screen: screen_item -> [>screen_] item_descr
    | ReportGroup: (report_group_item) -> [>report_group_] item_descr

  let pp_item_descr (type k) : k item_descr Pretty.printer = fun ppf -> function
    | Constant c -> pp_constant_item ppf c
    | Data d -> pp_data_item ppf d
    | Renames r -> pp_rename_item ppf r
    | CondName c -> pp_condition_name_item ppf c
    | Screen s -> pp_screen_item ppf s
    | ReportGroup r -> pp_report_group_item ppf r

  let compare_item_descr (type a b) : a item_descr -> b item_descr -> int =
    fun a b ->
      match a, b with
      | Constant a, Constant b -> compare_constant_item a b
      | Constant _, _ -> -1
      | _, Constant _ -> 1
      | Data a, Data b -> compare_data_item a b
      | Data _, _ -> -1
      | _, Data _ -> 1
      | Renames a, Renames b -> compare_rename_item a b
      | Renames _, _ -> -1
      | _, Renames _ -> 1
      | CondName a, CondName b -> compare_condition_name_item a b
      | CondName _, _ -> -1
      | _, CondName _ -> 1
      | Screen a, Screen b -> compare_screen_item a b
      | Screen _, _ -> -1
      | _, Screen _ -> 1
      | ReportGroup a, ReportGroup b -> compare_report_group_item a b

  type data_item_descr    = data_ item_descr
  and constant_item_descr = constant_ item_descr
  and working_item_descr  = [constant_|data_|
                             rename_|condition_name_] item_descr
  and report_item_descr   = [constant_|report_group_] item_descr
  and screen_item_descr   = [constant_|screen_]       item_descr
  and any_item_descr      = [constant_|data_|rename_|condition_name_|
                             report_group_|screen_] item_descr

  let pp_data_item_descr     = pp_item_descr
  let compare_data_item_descr = compare_item_descr
  let pp_constant_item_descr = pp_item_descr
  let compare_constant_item_descr = compare_item_descr
  let pp_working_item_descr  = pp_item_descr
  let compare_working_item_descr = compare_item_descr
  let pp_report_item_descr   = pp_item_descr
  let compare_report_item_descr = compare_item_descr
  let pp_screen_item_descr   = pp_item_descr
  let compare_screen_item_descr = compare_item_descr
  let pp_any_item_descr      = pp_item_descr
  let compare_any_item_descr = compare_item_descr

  let item_descr_level (type k) : k item_descr -> data_level = function
    | Constant { constant_level = l ; _ } -> l.payload
    | Data { data_level = l; _ } -> l.payload
    | Renames { rename_level = l; _ } -> l.payload
    | CondName { condition_name_level = l; _ } -> l.payload
    | Screen { screen_level = l; _ } -> l
    | ReportGroup { report_level = l; _ } -> l

  type working_storage_item_descr = working_item_descr
  let compare_working_storage_item_descr = compare_item_descr
  type linkage_item_descr         = working_item_descr
  let compare_linkage_item_descr = compare_item_descr
  type file_item_descr            = working_item_descr
  let compare_file_item_descr = compare_item_descr
  type communication_item_descr   = working_item_descr
  let compare_communication_item_descr = compare_item_descr
  type local_storage_item_descr   = working_item_descr
  let compare_local_storage_item_descr = compare_item_descr

  type file_descr =
    {
      file_name: name with_loc;
      file_clauses: file_clauses;
      file_items: file_item_descr with_loc list;
    }

  and file_clauses =
    | FileFD of file_fd_clause with_loc list
    | FileSD of file_sd_clause with_loc list

  and file_fd_clause =
    | FileExternal of external_clause
    | FileGlobal
    | FileFormat of format_clause
    | FileBlockContains of
        {
          from: integer;
          to_: integer option;
          characters_or_records: file_block_contents;
        }
    | FileRecord of record_clause
    | FileLabel of label_clause
    | FileValueOf of value_of_clause list
    | FileData of file_data_clause
    | FileLinage of file_linage_clause
    | FileCodeSet of alphabet_specification
    | FileReport of name with_loc list

  and file_sd_clause =
    | FileSDRecord of record_clause
    | FileSDData of file_data_clause
    | FileSDGlobal
  [@@deriving ord]

  let pp_file_fd_clause ppf = function
    | FileExternal ec -> pp_external_clause ppf ec
    | FileGlobal -> Fmt.pf ppf "GLOBAL"
    | FileFormat fc -> pp_format_clause ppf fc
    | FileBlockContains { from; to_; characters_or_records } ->
      Fmt.pf ppf "BLOCK CONTAINS %a%a@ %a"
        pp_integer from
        Fmt.(option (any "@ TO " ++ pp_integer)) to_
        pp_file_block_contents characters_or_records
    | FileRecord rc -> pp_record_clause ppf rc
    | FileLabel lc -> pp_label_clause ppf lc
    | FileValueOf vcl ->
      Fmt.pf ppf "VALUE OF %a"
        Fmt.(list ~sep:sp pp_value_of_clause) vcl
    | FileData fdc -> pp_file_data_clause ppf fdc
    | FileLinage flc -> pp_file_linage_clause ppf flc
    | FileCodeSet als ->
      Fmt.pf ppf "CODE-SET %a" pp_alphabet_specification als
    | FileReport [ n ] ->
      Fmt.pf ppf "REPORT IS %a" (pp_with_loc pp_name) n
    | FileReport ns ->
      Fmt.pf ppf "REPORTS ARE %a"
        Fmt.(list ~sep:sp (pp_with_loc pp_name)) ns


  let pp_file_sd_clause ppf = function
    | FileSDRecord rc -> pp_record_clause ppf rc
    | FileSDData fdc -> pp_file_data_clause ppf fdc
    | FileSDGlobal -> Fmt.pf ppf "GLOBAL"

  let rec pp_close_boxes ppf = function
    | [] -> ()
    | _ :: boxes -> Fmt.pf ppf "@]"; pp_close_boxes ppf boxes

  let rec pp_close_boxes_until l ppf = function
    | [] -> Fmt.pf ppf "@,@[<v>"; [ l ]
    | (l' :: _) as boxes when l' = l -> Fmt.pf ppf "@,"; boxes
    | (l' :: _) as boxes when l' < l -> Fmt.pf ppf "@;<1 2>@[<v>"; l :: boxes
    | _ :: boxes -> Fmt.pf ppf "@]"; pp_close_boxes_until l ppf boxes

  let rec pp_item_descr_list boxes ppf = function
    | [] -> pp_close_boxes ppf boxes
    | id :: ids ->
      let idl = item_descr_level id.payload in
      let boxes = pp_close_boxes_until idl ppf boxes in
      Fmt.box (pp_with_loc pp_item_descr) ppf id;
      pp_item_descr_list boxes ppf ids

  let pp_file_descr ppf { file_name; file_clauses; file_items } =
    let pp_file_clauses pp_clause =
      Fmt.(list ~sep:sp (pp_with_loc pp_clause))
    in
    let de, is_empty, pp =
      match file_clauses with
      | FileFD file_clauses ->
        "FD", file_clauses == [],
        fun ppf ->
          Fmt.pf ppf "@ %a"
            Fmt.(pp_file_clauses (box pp_file_fd_clause)) file_clauses
      | FileSD file_clauses ->
        "SD", file_clauses == [],
        fun ppf ->
          Fmt.pf ppf "@ %a"
            (pp_file_clauses pp_file_sd_clause) file_clauses
    in
    let pp = if is_empty then fun _ -> () else pp in
    Fmt.pf ppf "@[<v 2>%s %a%t@].%a" de
      (pp_with_loc pp_name) file_name pp
      (pp_item_descr_list []) file_items


  type communication_descr =
    {
      comm_name: name with_loc;
      comm_clauses: comm_clause with_loc list;
      comm_items: communication_item_descr with_loc list;
      comm_direction: comm_direction;
    }

  and comm_direction =
    | CommInput of { initial: bool; items: data_name with_loc list }
    | CommOutput
    | CommIO of { initial: bool; items: name with_loc list }
  [@@deriving ord]

  let pp_comm_direction ppf = function
    | CommInput { initial; items = _ } ->
      if initial then Fmt.pf ppf "INITIAL ";
      Fmt.pf ppf "INPUT"
    | CommOutput -> Fmt.pf ppf "OUTPUT"
    | CommIO { initial; items = _ } ->
      if initial then Fmt.pf ppf "INITIAL ";
      Fmt.pf ppf "I-O"

  let pp_comm_direction_items ppf = function
    | CommInput { initial = _; items } ->
      Fmt.(list ~sep:nop (sp ++ pp_with_loc pp_data_name)) ppf items
    | CommOutput -> ()
    | CommIO { initial = _; items } ->
      Fmt.(list ~sep:nop (sp ++ pp_name')) ppf items

  let pp_communication_descr ppf
    { comm_name; comm_clauses; comm_items; comm_direction }
  =
    Fmt.pf ppf "@[<v 2>CD %a %a%a%a@].%a"
      (pp_with_loc pp_name) comm_name
      pp_comm_direction comm_direction
      Fmt.(list ~sep:nop (sp ++ pp_with_loc pp_comm_clause)) comm_clauses
      pp_comm_direction_items comm_direction
      (pp_item_descr_list []) comm_items


  type report_descr =
    {
      report_name: name with_loc;
      report_clauses: report_clause with_loc list;
      report_items: report_item_descr with_loc list;
    }
  [@@deriving ord]

  let pp_report_descr ppf { report_name; report_clauses; report_items } =
    Fmt.pf ppf "@[<v 2>RD %a%a@].%a"
      (pp_with_loc pp_name) report_name
      Fmt.(list ~sep:nop (sp ++ pp_with_loc pp_report_clause)) report_clauses
      (pp_item_descr_list []) report_items


  (* Actual sections *)

  type file_section = file_descr with_loc list [@@deriving ord]
  type working_storage_section = working_storage_item_descr with_loc list
    [@@deriving ord]
  type linkage_section = linkage_item_descr with_loc list [@@deriving ord]
  type communication_section = communication_descr with_loc list [@@deriving ord]
  type local_storage_section = local_storage_item_descr with_loc list
    [@@deriving ord]
  type report_section = report_descr with_loc list [@@deriving ord]
  type screen_section = screen_item_descr with_loc list [@@deriving ord]

  let pp_section k pp_item_descr =
    Fmt.(
      vbox (
      const string k ++ any " SECTION." ++
      list ~sep:nop (sp ++ pp_with_loc pp_item_descr))
    )

  let pp_item_section k ppf xs =
    Fmt.pf ppf "@[<v>";
    Fmt.(string ++ any " SECTION.") ppf k;
    pp_item_descr_list [] ppf xs;
    Fmt.pf ppf "@]"

  let pp_file_section : file_section Fmt.t = pp_section "FILE" pp_file_descr
  let pp_working_storage_section : working_storage_section Fmt.t =
    pp_item_section "WORKING-STORAGE"
  let pp_linkage_section : linkage_section Fmt.t =
    pp_item_section "LINKAGE"
  let pp_communication_section : communication_section Fmt.t =
    pp_section "COMMUNICATION" pp_communication_descr
  let pp_local_storage_section : local_storage_section Fmt.t =
    pp_item_section "LOCAL-STORAGE"
  let pp_report_section : report_section Fmt.t =
    pp_section "REPORT" pp_report_descr
  let pp_screen_section : screen_section Fmt.t =
    pp_item_section "SCREEN"

end

module Data_division (Data_sections: Abstract.DATA_SECTIONS) = struct
  include Data_sections

  type data_division =
    {
      file_section: file_section with_loc option;
      working_storage_section: working_storage_section with_loc option;
      linkage_section: linkage_section with_loc option;
      communication_section: communication_section with_loc option;
      local_storage_section: local_storage_section with_loc option;
      report_section: report_section with_loc option;
      screen_section: screen_section with_loc option;
    }
  [@@deriving ord]

  let pp_data_division ppf { file_section;
                             working_storage_section; linkage_section;
                             communication_section; local_storage_section;
                             report_section; screen_section } =
    let pp_section pp ppf section =
      Fmt.(option (sp ++ vbox (pp_with_loc pp))) ppf section
    in
    Fmt.pf ppf "@[<v 2>DATA DIVISION.";
    pp_section pp_file_section ppf file_section;
    pp_section pp_working_storage_section ppf working_storage_section;
    pp_section pp_linkage_section ppf linkage_section;
    pp_section pp_communication_section ppf communication_section;
    pp_section pp_local_storage_section ppf local_storage_section;
    pp_section pp_report_section ppf report_section;
    pp_section pp_screen_section ppf screen_section;
    Fmt.pf ppf "@]"


end

module Statements = struct
  type statement  = Ast.statement
  let pp_statement = Ast.pp_statement
  let compare_statement = Ast.compare_statement
  type statements = Ast.statements
  let pp_statements = Ast.pp_statements
  let pp_dump_statements = Ast.pp_dump_statements
  let compare_statements = Ast.compare_statements
end

module Proc_division (Statements: Abstract.STATEMENTS) = struct
  include Statements

  type procedure_division =
    {
      procedure_using_clauses: using_clause with_loc list;
      procedure_returning: ident with_loc option;
      procedure_raising_phrases: raising_phrase with_loc list;
      procedure_declaratives: declarative with_loc list;
      procedure_paragraphs: paragraph with_loc list;
    }

  and using_clause =
    | UsingByReference of using_by_reference list
    | UsingByValue of name with_loc list

  and using_by_reference =
    {
      using_by_reference: name with_loc;
      using_by_reference_optional: bool;
    }

  and raising_phrase =
    {
      raising: name with_loc;
      raising_factory: bool;
    }

  and declarative =
    {
      declarative_name: name with_loc;
      declarative_segment: integer option;
      declarative_use: declarative_use option;
      declarative_sentences: statements with_loc list;
    }

  and declarative_use =
    | UseAfterFileException of
        {
          global: bool;
          trigger: use_file_exception_on;
        }
    | UseBeforeReporting of
        {
          global: bool;
          report_group: ident;
        }
    | UseForDebugging of use_for_debugging_target list
    | UseAfterIOException of use_after_exception list
    | UseAfterExceptionObject of name with_loc

  and use_for_debugging_target =
    | UseForDebuggingProcedure of
        {
          all: bool;
          procedure: qualname;
        }
    | UseForDebuggingAllProcedures

  and use_file_exception_on =
    | UseFileExceptionOnNames of name with_loc list
    | UseFileExceptionOnOpenMode of open_mode

  and use_after_exception =
    {
      use_after_exception: name with_loc;
      use_after_exception_on_files: name with_loc list;
    }

  and paragraph =
    {
      paragraph_name: name with_loc option;
      paragraph_is_section: bool;
      paragraph_segment: integer option;
      paragraph_sentences: statements with_loc list;
    }
  [@@deriving ord]

  let pp_paragraph ppf {
      paragraph_name = pn;
      paragraph_is_section = pis;
      paragraph_segment = ps;
      paragraph_sentences = pss;
  } =
    (
      match pn with
      | Some n ->
        if pis then Fmt.pf ppf "@[<v 2>" else Fmt.pf ppf "@[<v>";
        pp_with_loc pp_name ppf n;
        if pis then Fmt.pf ppf " SECTION";
        Fmt.(option (sp ++ pp_integer)) ppf ps;
        Fmt.pf ppf ".@ "
      | None -> Fmt.pf ppf "@[<v>";
    );
    Fmt.(list ~sep:(any ".@,@ ") (pp_with_loc pp_statements)) ppf pss;
    if pss != [] then Fmt.pf ppf ".";
    Fmt.pf ppf "@]"

  let pp_use_after_exception ppf
    { use_after_exception = n; use_after_exception_on_files = af }
  =
    Fmt.pf ppf "%a%a"
      pp_name' n
      Fmt.(list ~sep:nop (any " FILE " ++ pp_name')) af

  let pp_use_file_exception_on ppf = function
    | UseFileExceptionOnNames ns -> Fmt.(list ~sep:sp pp_name') ppf ns
    | UseFileExceptionOnOpenMode om -> pp_open_mode ppf om

  let pp_use_for_debugging_target ppf = function
    | UseForDebuggingProcedure { all; procedure } ->
      if all then Fmt.pf ppf "ALL ";
      pp_qualname ppf procedure
    | UseForDebuggingAllProcedures ->
      Fmt.pf ppf "ALL PROCEDURES"

  let pp_declarative_use ppf = function
    | UseAfterFileException { global; trigger} ->
      Fmt.pf ppf "USE ";
      if global then Fmt.pf ppf "GLOBAL ";
      Fmt.pf ppf "AFTER EXCEPTION ON %a" pp_use_file_exception_on trigger;
    | UseBeforeReporting { global; report_group } ->
      Fmt.pf ppf "USE ";
      if global then Fmt.pf ppf "GLOBAL ";
      Fmt.pf ppf "BEFORE REPORTING %a" pp_ident report_group
    | UseForDebugging ufdts ->
      Fmt.pf ppf "USE FOR DEBUGGING ON@;<1 2>@[%a@]"
        Fmt.(list ~sep:sp pp_use_for_debugging_target) ufdts
    | UseAfterIOException uaes ->
      Fmt.pf ppf "USE AFTER EXCEPTION CONDITION@;<1 2>@[%a@]"
        Fmt.(list ~sep:sp pp_use_after_exception) uaes
    | UseAfterExceptionObject n ->
      Fmt.pf ppf "USE AFTER EXCEPTION OBJECT %a" pp_name' n

  let pp_declarative ppf {
    declarative_name = n; declarative_segment = s;
    declarative_use = u; declarative_sentences = ss }
  =
    pp_name' ppf n;
    let pp_seg = Fmt.(const (option (any " " ++ pp_integer)) s) in
    Fmt.(
      option (any " SECTION" ++ pp_seg ++ any ".@;<1 2>" ++ box pp_declarative_use)
    ) ppf u;
    Fmt.pf ppf ".";
    Fmt.(list ~sep:nop (sp ++ pp_with_loc pp_statements ++ any ".")) ppf ss

  let pp_declaratives ppf = function
    | [] -> ()
    | pd ->
      Fmt.pf ppf "DECLARATIVES.%a@ END DECLARATIVES."
        Fmt.(list ~sep:nop (sp ++ pp_with_loc pp_declarative)) pd

  let pp_using_by_reference ppf {
    using_by_reference = n; using_by_reference_optional = o }
  =
    if o then Fmt.pf ppf "OPTIONAL ";
    pp_name' ppf n

  let pp_using_clause ppf = function
    | UsingByReference ubrs ->
      Fmt.pf ppf "USING ";
      Fmt.(list ~sep:sp pp_using_by_reference) ppf ubrs
    | UsingByValue ns ->
      Fmt.pf ppf "USING BY VALUE %a" Fmt.(list ~sep:sp pp_name') ns

  let pp_raising_phrase ppf { raising; raising_factory } =
    if raising_factory then Fmt.pf ppf "FACTORY ";
    pp_name' ppf raising

  let pp_procedure_division ppf {
    procedure_using_clauses = puc;
    procedure_returning = pur;
    procedure_raising_phrases = prp;
    procedure_declaratives = pd;
    procedure_paragraphs = pp;
  } =
    Fmt.pf ppf "PROCEDURE DIVISION%a%a%a.%a@;<1 2>@[<hv>%a@]"
      Fmt.(list ~sep:nop (sp ++ pp_with_loc pp_using_clause)) puc
      Fmt.(option (any "@ RETURNING " ++ pp_with_loc pp_ident)) pur
      Fmt.(
        if prp == [] then nop else
        any "RAISING " ++ list ~sep:sp (pp_with_loc pp_raising_phrase)
      ) prp
      Fmt.(if pd != [] then sp ++ pp_declaratives else nop) pd
      Fmt.(list ~sep:sp (pp_with_loc pp_paragraph)) pp

end

module Compilation_group
    (Misc_sections: Abstract.MISC_SECTIONS)
    (Data_division: Abstract.DATA_DIVISION)
    (Proc_division: Abstract.PROC_DIVISION) =
struct

  include Misc_sections
  include Data_division
  include Proc_division

  type program_unit =
    {
      program_name: name with_loc;
      program_as: strlit option;
      program_level: program_level;
      program_options: options_paragraph with_loc option;
      program_env: environment_division with_loc option;
      program_data: data_division with_loc option;
      program_proc: procedure_division with_loc option;
      program_end_name: name with_loc option
    }

  and program_level =
    | ProgramDefinition of
        {       (* Note: more general than before (allows nested prototypes): *)
          kind: program_kind option;
          has_identification_division: bool;
          informational_paragraphs: informational_paragraphs
            [@compare fun _ _ -> 0]; (* ~COB85, -COB2002 *)
          nested_programs: program_unit with_loc list;
        }
    | ProgramPrototype

  and program_kind =
    | Common
    | Initial
    | Recursive
  [@@deriving ord]

  let pp_program_kind ppf = function
    | Common -> Fmt.pf ppf "COMMON"
    | Initial -> Fmt.pf ppf "INITIAL"
    | Recursive -> Fmt.pf ppf "RECURSIVE"

  let rec pp_program_unit ppf {
    program_name;
    program_as;
    program_level;
    program_options;
    program_env;
    program_data;
    program_proc;
    program_end_name;
  } =
    let has_identification_division =
      match program_level with
      | ProgramDefinition { has_identification_division = true; _ } -> true
      | _ -> false
    in
    let nested_programs =
      match program_level with
      | ProgramDefinition { nested_programs; _ } -> nested_programs
      | ProgramPrototype -> []
    in
    let _informational_paragraphs =
      match program_level with
      | ProgramDefinition { informational_paragraphs = ip; _ } -> Some ip
      | ProgramPrototype -> None
    in
    if has_identification_division then
      Fmt.pf ppf "@[IDENTIFICATION@ DIVISION@].@ ";
    Fmt.pf ppf "@[PROGRAM-ID.@ %a" (pp_with_loc pp_name) program_name;
    Fmt.(option (any "@ AS " ++ pp_strlit)) ppf program_as;
    (
      match program_level with
      | ProgramDefinition { kind; _ } ->
        Fmt.(option (sp ++ pp_program_kind)) ppf kind
      | ProgramPrototype -> Fmt.pf ppf "@ PROTOTYPE"
    );
    Fmt.pf ppf ".@]";
    Fmt.(option (sp ++ pp_with_loc pp_options_paragraph)) ppf program_options;
    Fmt.(option (sp ++ pp_with_loc pp_environment_division)) ppf program_env;
    Fmt.(option (sp ++ pp_with_loc pp_data_division)) ppf program_data;
    Fmt.(option (sp ++ pp_with_loc pp_procedure_division)) ppf program_proc;
    Fmt.(list ~sep:sp (pp_with_loc pp_program_unit)) ppf nested_programs;
    Fmt.(option (any "@ @[END PROGRAM@ " ++ pp_with_loc pp_name ++ any ".@]"))
      ppf program_end_name


  type function_unit =
    {
      function_name: name with_loc;
      function_as: strlit option;
      function_is_proto: bool;
      function_options: options_paragraph with_loc option;
      function_env: environment_division with_loc option;
      function_data: data_division with_loc option;
      function_proc: procedure_division option;
      function_end_name: name with_loc;
    }
  [@@deriving ord]

  let pp_id_paragraph
    ?(end_ = false) ?name
    popen pclose ppf pp_header header sections
  =
    Fmt.pf ppf "@[%s.%a@]" popen pp_header header;
    List.iter (Fmt.(option (sp ++ fun ppf fp -> fp ppf ())) ppf) sections;
    if end_ && Option.is_none name then
      Fmt.pf ppf "@ @[END %s.@]" pclose
    else
      Fmt.(option (any "@ @[END " ++ const string pclose ++ any " " ++ pp_name' ++ any ".@]"))
        ppf name

  let pp_function_id_paragraph ppf (n, fas, is_proto) =
    Fmt.pf ppf "%a%a%a"
      pp_name' n
      Fmt.(option (any "@ AS " ++ pp_strlit)) fas
      (if is_proto then Fmt.any "@ PROTOTYPE" else Fmt.nop) ()

  let pp_function_unit ppf
    { function_name = n; function_as = fas; function_is_proto = is_proto;
      function_options = opts; function_env = env; function_data = data;
      function_proc = proc; function_end_name = en }
  =
    pp_id_paragraph ~name:en "FUNCTION-ID" "FUNCTION" ppf
      Fmt.(sp ++ pp_function_id_paragraph ++ any ".") (n, fas, is_proto)
      Fmt.[
        Option.map (const (pp_with_loc pp_options_paragraph)) opts;
        Option.map (const (pp_with_loc pp_environment_division)) env;
        Option.map (const (pp_with_loc pp_data_division)) data;
        Option.map (const pp_procedure_division) proc;
      ]

  type method_definition =
    {
      method_name: name with_loc;
      method_kind: method_kind;
      method_override: bool;
      method_final: bool;
      method_options: options_paragraph with_loc option;
      method_env: environment_division with_loc option;
      method_data: data_division with_loc option;
      method_proc: procedure_division option;
      method_end_name: name with_loc;
    }

  and method_kind =
    | NamedMethod of { as_: strlit option }
    | PropertyMethod of { kind: property_kind }
  [@@deriving ord]

  let pp_method_kind ppf = function
    | NamedMethod { as_ } -> Fmt.(option (any "@ AS " ++ pp_strlit)) ppf as_
    | PropertyMethod { kind } -> pp_property_kind ppf kind

  let pp_method_id_paragraph ppf (n, k, o, f) =
    Fmt.pf ppf "%a %a" pp_name' n pp_method_kind k;
    if o then Fmt.pf ppf " OVERRIDE";
    if f then Fmt.pf ppf " FINAL"

  let pp_method_definition ppf {
    method_name = n; method_kind = k; method_override = o; method_final = f;
    method_options = opts; method_env = env; method_data = data;
    method_proc = proc; method_end_name = en
  } =
    pp_id_paragraph ~name:en "METHOD-ID" "METHOD" ppf
      Fmt.(sp ++ pp_method_id_paragraph ++ any ".") (n, k, o, f)
      Fmt.[
        Option.map (const (pp_with_loc pp_options_paragraph)) opts;
        Option.map (const (pp_with_loc pp_environment_division)) env;
        Option.map (const (pp_with_loc pp_data_division)) data;
        Option.map (const pp_procedure_division) proc;
      ]

  type factory_definition = (* Note: could be merged with instance_definition *)
    {
      factory_implements: name with_loc list;
      factory_options: options_paragraph with_loc option;
      factory_env: environment_division with_loc option;
      factory_data: data_division with_loc option;
      factory_methods: method_definition with_loc list option;
    }
  [@@deriving ord]

  let pp_object_procedure_division =
    Fmt.(
        any "PROCEDURE DIVISION." ++
        (list ~sep:nop (sp ++ pp_with_loc pp_method_definition))
    )

  let pp_implements ppf = function
    | [] -> ()
    | names ->
      Fmt.(list ~sep:sp (any "IMPLEMENTS " ++ pp_name')) ppf names

  let pp_factory_definition ppf
    { factory_implements = impl; factory_options = opts; factory_env = env;
      factory_data = data; factory_methods = meths }
  =
    pp_id_paragraph ~end_:true "FACTORY" "FACTORY" ppf
      pp_implements impl
      Fmt.[
        Option.map (const (pp_with_loc pp_options_paragraph)) opts;
        Option.map (const (pp_with_loc pp_environment_division)) env;
        Option.map (const (pp_with_loc pp_data_division)) data;
        Option.map (const pp_object_procedure_division) meths;
      ]

  type instance_definition =
    {
      instance_implements: name with_loc list;
      instance_options: options_paragraph with_loc option;
      instance_env: environment_division with_loc option;
      instance_data: data_division with_loc option;
      instance_methods: method_definition with_loc list option;
    }
  [@@deriving ord]

  let pp_instance_definition ppf
    { instance_implements = impl; instance_options = opts; instance_env = env;
      instance_data = data; instance_methods = meths }
  =
    pp_id_paragraph ~end_:true "OBJECT" "OBJECT" ppf
      pp_implements impl
      Fmt.[
        Option.map (const (pp_with_loc pp_options_paragraph)) opts;
        Option.map (const (pp_with_loc pp_environment_division)) env;
        Option.map (const (pp_with_loc pp_data_division)) data;
        Option.map (const pp_object_procedure_division) meths;
      ]


  type class_definition =
    {
      class_name: name with_loc;
      class_as: strlit option;
      class_final: bool;
      class_inherits: name with_loc list;
      class_usings: name with_loc list;
      class_options: options_paragraph with_loc option;
      class_env: environment_division with_loc option;
      class_factory: factory_definition option;
      class_instance: instance_definition option;
      class_end_name: name with_loc;
    }
  [@@deriving ord]

  let pp_class_id_paragraph ppf (cn, cas, f, inh, us) =
    pp_name' ppf cn;
    Fmt.(option (any "@ AS " ++ pp_strlit)) ppf cas;
    if f then Fmt.pf ppf " FINAL";
    if inh != [] then
      Fmt.pf ppf "@ INHERITS %a" Fmt.(list ~sep:sp pp_name') inh;
    if us != [] then
      Fmt.pf ppf "@ USING %a" Fmt.(list ~sep:sp pp_name') inh;
    Fmt.pf ppf "."

  let pp_class_definition ppf
    { class_name = cn; class_as = cas; class_final = f; class_inherits = inh;
      class_usings = us; class_options = opts; class_env = env;
      class_factory = fac; class_instance = inst; class_end_name = en }
  =
    pp_id_paragraph ~name:en "CLASS-ID" "CLASS" ppf
      Fmt.(sp ++ pp_class_id_paragraph) (cn, cas, f, inh, us)
      Fmt.[
        Option.map (const (pp_with_loc pp_options_paragraph)) opts;
        Option.map (const (pp_with_loc pp_environment_division)) env;
        Option.map (const pp_factory_definition) fac;
        Option.map (const pp_instance_definition) inst;
      ]


  type interface_definition =
    {
      interface_name: name with_loc;
      interface_as: strlit option;
      interface_inherits: name with_loc list;
      interface_usings: name with_loc list;
      interface_options: options_paragraph with_loc option;
      interface_env: environment_division with_loc option;
      interface_methods: method_definition with_loc list option;
      interface_end_name: name with_loc;
    }
  [@@deriving ord]

  let pp_interface_id_paragraph ppf (n, a, inh, us) =
    Fmt.pf ppf "%a%a%a%a"
      pp_name' n
      Fmt.(option (any "@ AS " ++ pp_strlit)) a
      Fmt.(
        if inh == [] then nop else
        Fmt.(any "@ INHERITS " ++ list ~sep:sp pp_name')
      ) inh
      Fmt.(
        if us == [] then nop else
        Fmt.(any "@ USING " ++ list ~sep:sp pp_name')
      ) us

  let pp_interface_definition ppf
    { interface_name = n; interface_as = a; interface_inherits = inh;
      interface_usings = us; interface_options = opts; interface_env = env;
      interface_methods = meths; interface_end_name = en }
  =
    pp_id_paragraph ~name:en "INTERFACE-ID" "INTERFACE" ppf
      Fmt.(sp ++ pp_interface_id_paragraph ++ any ".") (n, a, inh, us)
      Fmt.[
        Option.map (const (pp_with_loc pp_options_paragraph)) opts;
        Option.map (const (pp_with_loc pp_environment_division)) env;
        Option.map (const pp_object_procedure_division) meths;
      ]

  type compilation_unit =
    | Program of program_unit
    | Function of function_unit
    | ClassDefinition of class_definition
    | InterfaceDefinition of interface_definition
  [@@deriving ord]

  let pp_compilation_unit ppf = function
    | Program pu -> pp_program_unit ppf pu
    | Function fu -> pp_function_unit ppf fu
    | ClassDefinition cd -> pp_class_definition ppf cd
    | InterfaceDefinition id -> pp_interface_definition ppf id

  type compilation_group =
    compilation_unit with_loc list
  [@@deriving ord]

  let pp_compilation_group =
    Fmt.(list ~sep:sp (Fmt.vbox @@ pp_with_loc pp_compilation_unit))

  let show_compilation_group = Fmt.to_to_string pp_compilation_group

end
