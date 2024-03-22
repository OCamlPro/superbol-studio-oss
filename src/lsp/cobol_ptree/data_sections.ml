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

open Common
open Numericals
open Terms
open Data_descr

type picture_string = string with_loc

let pp_picture_string fmt { payload; _ } = Pretty.string fmt payload
let show_picture_string = Pretty.to_string "%a" pp_picture_string
let compare_picture_string = Cobol_common.Srcloc.compare_with_loc String.compare

type picture_clause =
  {
    picture_string: picture_string;
    picture_locale: locale_phrase option;
    picture_depending: qualname with_loc option;
  }
[@@deriving ord]

let pp_picture_clause ppf { picture_string; picture_locale;
                            picture_depending } =
  Fmt.pf ppf "PIC %a%a%a"
    pp_picture_string picture_string
    Fmt.(option (sp ++ pp_locale_phrase)) picture_locale
    Fmt.(option (any "@ DEPENDING ON@ " ++
                 pp_with_loc pp_qualname)) picture_depending

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
        structure_name: name with_loc option;
        limit: integer option;
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
  | DataDynamicLength { structure_name = sn; limit = l } ->
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

let pp_data_item ppf { data_level = lvl; data_name = nc;
                       data_clauses = cls } =
  pp_item (pp_with_loc pp_data_clause) ppf lvl.payload nc cls

type constant_item =
  {
    constant_level: data_level with_loc;      (* is a constant *)     (* TODO: check \in {"1", "01"} *)
    constant_name: name with_loc;
    constant_global: bool;
    constant_value: constant_value with_loc;
  }
[@@deriving ord]

let pp_constant_item ppf {
  constant_level; constant_name; constant_global; constant_value
} =
  Fmt.pf ppf "%a%a@ CONSTANT@ %a@ %a."
    (pp_with_loc pp_data_level) constant_level
    (pp_with_loc pp_name) constant_name
    Fmt.(if constant_global then any "@ IS GLOBAL" else nop) ()
    (pp_with_loc pp_constant_value) constant_value

type rename_item =
  {
    rename_level: data_level with_loc;
    rename_to: name with_loc;
    rename_from: qualname with_loc;
    rename_thru: qualname with_loc option;
  }
[@@deriving ord]

let pp_rename_item ppf { rename_level = rl; rename_to = rto;
                         rename_from = from; rename_thru = thru } =
  Fmt.pf ppf "%a %a@;<1 2>RENAMES %a%a."
    (pp_with_loc pp_data_level) rl
    pp_name' rto
    pp_qualname' from
    Fmt.(option (any "@;<1 2>THROUGH " ++ pp_qualname')) thru

type condition_name_item =
  {
    condition_name_level: data_level with_loc; (* is always 88 *)
    condition_name: name with_loc;
    condition_name_values: condition_name_value list;
    condition_name_alphabet: name with_loc option;
    condition_name_when_false: literal option;
  }
[@@deriving ord]

let pp_condition_name_item ppf
  { condition_name_level = cnl; condition_name = cn;
    condition_name_values = cnvl; condition_name_alphabet = cna;
    condition_name_when_false = cnwf }
=
  Fmt.pf ppf "%a %a@ VALUE %a%a%a."
    (pp_with_loc pp_data_level) cnl
    pp_name' cn
    Fmt.(list ~sep:comma pp_condition_name_value) cnvl
    Fmt.(option (any "@ IN " ++ pp_name')) cna
    Fmt.(option (any "@ WHEN FALSE " ++ pp_literal)) cnwf

type screen_item =
  {
    screen_level: int;
    screen_data_name: data_name with_loc option;
    screen_clauses: screen_clause with_loc list;
  }
[@@deriving ord]

let pp_screen_item ppf { screen_level; screen_data_name; screen_clauses } =
  pp_item (pp_with_loc pp_screen_clause) ppf
    screen_level screen_data_name screen_clauses

type report_group_item =
  {
    report_level: int;
    report_data_name: data_name with_loc option;
    report_group_clauses: report_group_clause with_loc list;
  }
[@@deriving ord]

let pp_report_group_item ppf { report_level; report_data_name;
                               report_group_clauses } =
  pp_item (pp_with_loc pp_report_group_clause) ppf
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
  | ReportGroup: report_group_item -> [>report_group_] item_descr

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
let pp_constant_item_descr = pp_item_descr
let pp_working_item_descr  = pp_item_descr
let pp_report_item_descr   = pp_item_descr
let pp_screen_item_descr   = pp_item_descr
let pp_any_item_descr      = pp_item_descr

let compare_data_item_descr = compare_item_descr
let compare_constant_item_descr = compare_item_descr
let compare_working_item_descr = compare_item_descr
let compare_report_item_descr = compare_item_descr
let compare_screen_item_descr = compare_item_descr
let compare_any_item_descr = compare_item_descr

let item_descr_level (type k) : k item_descr -> data_level = function
  | Constant { constant_level = l ; _ } -> l.payload
  | Data { data_level = l; _ } -> l.payload
  | Renames { rename_level = l; _ } -> l.payload
  | CondName { condition_name_level = l; _ } -> l.payload
  | Screen { screen_level = l; _ } -> l
  | ReportGroup { report_level = l; _ } -> l

type working_storage_item_descr = working_item_descr
type linkage_item_descr         = working_item_descr
type file_item_descr            = working_item_descr
type communication_item_descr   = working_item_descr
type local_storage_item_descr   = working_item_descr

let compare_working_storage_item_descr = compare_item_descr
let compare_linkage_item_descr = compare_item_descr
let compare_file_item_descr = compare_item_descr
let compare_communication_item_descr = compare_item_descr
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
        contents: file_block_contents;
      }
  | FileRecord of record_clause
  | FileRecordingMode of recording_mode_clause
  | FileLabel of label_clause
  | FileValueOf of valueof_clause list
  | FileData of file_data_clause
  | FileLinage of file_linage_clause
  | FileCodeSet of Operands.alphabet_specification
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
  | FileBlockContains { from; to_; contents } ->
      Fmt.pf ppf "BLOCK CONTAINS %a%a@ %a"
        pp_integer from
        Fmt.(option (any "@ TO " ++ pp_integer)) to_
        pp_file_block_contents contents
  | FileRecord rc -> pp_record_clause ppf rc
  | FileRecordingMode md -> pp_recording_mode_clause ppf md
  | FileLabel lc -> pp_label_clause ppf lc
  | FileValueOf vcl ->
      Fmt.pf ppf "VALUE OF %a"
        Fmt.(list ~sep:sp pp_valueof_clause) vcl
  | FileData fdc -> pp_file_data_clause ppf fdc
  | FileLinage flc -> pp_file_linage_clause ppf flc
  | FileCodeSet als ->
      Fmt.pf ppf "CODE-SET %a" Operands.pp_alphabet_specification als
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
  | [] ->
      pp_close_boxes ppf boxes
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
[@@deriving ord]

let pp_comm_direction_items ppf = function
  | CommInput { initial = _; items } ->
      Fmt.(list ~sep:nop (sp ++ pp_with_loc pp_data_name)) ppf items
  | CommOutput -> ()
  | CommIO { initial = _; items } ->
      Fmt.(list ~sep:nop (sp ++ pp_name')) ppf items

let pp_communication_descr ppf { comm_name; comm_clauses; comm_items;
                                 comm_direction } =
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
  Fmt.(vbox (const string k ++ any " SECTION." ++
             list ~sep:nop (sp ++ pp_with_loc pp_item_descr)))

let pp_item_section k ppf xs =
  Fmt.pf ppf "@[<v>";
  Fmt.(string ++ any " SECTION.") ppf k;
  pp_item_descr_list [] ppf xs;
  Fmt.pf ppf "@]"

let pp_file_section : file_section Fmt.t =
  pp_section "FILE" pp_file_descr
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
