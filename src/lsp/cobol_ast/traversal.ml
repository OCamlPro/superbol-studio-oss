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

(* NB: broken, but possibly obsolete. *)

(*
open Cobol_common.Srcloc.INFIX
open Cobol_common.Basics
open Ast

(** This module implements the functors let module types to make mapfolder from one
    implementation of {!Cobol_ptree.Make} to another. *)

module Make_data_sections_traversal
    (Data_sec_in: DATA_SECTIONS_AST)
    (Data_sec_out: DATA_SECTIONS_AST)
    (Default_map: sig
       val picture: Data_sec_in.Pic_repr.picture -> Data_sec_out.Pic_repr.picture
     end) =
struct
  (** This functor makes a mapfolder from {!In} to {!Out}, using {!Annot_map} to map the different
      representations from {!In} to {!Out}. *)
  open Data_sec_in
  open Data_sec_out

  (** This type is used to specify the types of the mapfold functions, ['a] is the type of
        the accumulator, ['b] is the type of the AST node that is given as input and
        ['c] is the type of the AST node that is given as output. *)
  type ('a, 'b, 'c) fm_fun = 'a fm_funs -> 'a -> 'b -> 'c * 'a

  (** This type is used to specify functions where the returns ast type does not change from the
       one that was given as input *)
  and ('a, 'b) fm_id_fun = ('a, 'b, 'b) fm_fun

  (** This record type is used to implement the mapfolder functions. *)
  and 'a fm_funs = {
    environment_division: ('a, environment_division with_loc, environment_division with_loc) fm_fun;
    configuration_section: ('a, configuration_section) fm_id_fun;
    input_output_section: ('a, input_output_section) fm_id_fun;
    special_names_paragraph: ('a, special_names_paragraph) fm_id_fun;
    currency_sign: ('a, currency_sign with_loc) fm_id_fun;
    decimal_point: ('a, decimal_point with_loc) fm_id_fun;
    condition_name_entry: ('a, condition_name_entry with_loc) fm_id_fun;
    rename_entry: ('a, rename_entry with_loc) fm_id_fun;
    qualname: ('a, qualname) fm_id_fun;
    data_name: ('a, entry_name option with_loc) fm_id_fun;
    level: ('a, integer) fm_id_fun;
    redefines_clause: ('a, redefines_clause with_loc) fm_id_fun;
    picture_clause: ('a, Data_sec_in.picture_clause with_loc, Data_sec_out.picture_clause with_loc) fm_fun;
    data_description_clause: ('a, Data_sec_in.data_description_clause with_loc, Data_sec_out.data_description_clause with_loc) fm_fun;
    screen_description_clause: ('a, Data_sec_in.screen_description_clause with_loc, Data_sec_out.screen_description_clause with_loc) fm_fun;
    report_group_description_clause: ('a, Data_sec_in.report_group_description_clause with_loc, Data_sec_out.report_group_description_clause with_loc) fm_fun;
    data_description_entry: ('a, Data_sec_in.data_description_entry with_loc, Data_sec_out.data_description_entry with_loc) fm_fun;
    report_group_description_entry: ('a, Data_sec_in.report_group_description_entry, Data_sec_out.report_group_description_entry) fm_fun;
    screen_description_entry: ('a, Data_sec_in.screen_description_entry, Data_sec_out.screen_description_entry) fm_fun;
    constant_or_data_description_entry: ('a, Data_sec_in.constant_or_data_description_entry with_loc, Data_sec_out.constant_or_data_description_entry with_loc) fm_fun;
    file_description_entry: ('a, Data_sec_in.file_description_entry, Data_sec_out.file_description_entry) fm_fun;
    sort_merge_description_entry: ('a, Data_sec_in.sort_merge_description_entry, Data_sec_out.sort_merge_description_entry) fm_fun;
    input_communication_entry: ('a, Data_sec_in.input_communication_entry, Data_sec_out.input_communication_entry) fm_fun;
    io_communication_entry: ('a, Data_sec_in.io_communication_entry, Data_sec_out.io_communication_entry) fm_fun;
    output_communication_entry: ('a, Data_sec_in.output_communication_entry, Data_sec_out.output_communication_entry) fm_fun;
    communication_description_entry: ('a, Data_sec_in.communication_description_entry with_loc, Data_sec_out.communication_description_entry with_loc) fm_fun;
    constant_or_report_group_description_entry: ('a, Data_sec_in.constant_or_report_group_description_entry with_loc, Data_sec_out.constant_or_report_group_description_entry with_loc) fm_fun;
    report_description_entry: ('a, Data_sec_in.report_description_entry with_loc, Data_sec_out.report_description_entry with_loc) fm_fun;
    constant_or_screen_description_entry: ('a, Data_sec_in.constant_or_screen_description_entry with_loc, Data_sec_out.constant_or_screen_description_entry with_loc) fm_fun;
    file_or_sort_merge_description_entry: ('a, Data_sec_in.file_or_sort_merge_description_entry with_loc, Data_sec_out.file_or_sort_merge_description_entry with_loc) fm_fun;
  }

  let fm_environment_division funs acc v =
    funs.environment_division funs acc v

  let fm_configuration_section funs acc v =
    funs.configuration_section funs acc v

  let fm_input_output_section funs acc v =
    funs.input_output_section funs acc v

  let fm_special_names_paragraph funs acc v =
    funs.special_names_paragraph funs acc v

  let fm_currency_sign funs acc v =
    funs.currency_sign funs acc v

  let fm_decimal_point funs acc v =
    funs.decimal_point funs acc v

  let fm_condition_name_entry funs acc v =
    funs.condition_name_entry funs acc v

  let fm_rename_entry funs acc v =
    funs.rename_entry funs acc v

  let fm_qualname funs acc v =
    funs.qualname funs acc v

  let fm_data_name funs acc v =
    funs.data_name funs acc v

  let fm_level funs acc v =
    funs.level funs acc v

  let fm_redefines_clause funs acc v =
    funs.redefines_clause funs acc v

  let fm_picture_clause funs acc v =
    funs.picture_clause funs acc v

  let fm_data_description_clause funs acc v =
    funs.data_description_clause funs acc v

  let fm_screen_description_clause funs acc v =
    funs.screen_description_clause funs acc v

  let fm_report_group_description_clause funs acc v =
    funs.report_group_description_clause funs acc v

  let fm_data_description_entry funs acc v =
    funs.data_description_entry funs acc v

  let fm_report_group_description_entry funs acc v =
    funs.report_group_description_entry funs acc v

  let fm_screen_description_entry funs acc v =
    funs.screen_description_entry funs acc v

  let fm_constant_or_data_description_entry funs acc v =
    funs.constant_or_data_description_entry funs acc v

  let fm_file_description_entry funs acc v =
    funs.file_description_entry funs acc v

  let fm_sort_merge_description_entry funs acc v =
    funs.sort_merge_description_entry funs acc v

  let fm_input_communication_entry funs acc v =
    funs.input_communication_entry funs acc v

  let fm_io_communication_entry funs acc v =
    funs.io_communication_entry funs acc v

  let fm_output_communication_entry funs acc v =
    funs.output_communication_entry funs acc v

  let fm_communication_description_entry funs acc v =
    funs.communication_description_entry funs acc v

  let fm_constant_or_report_group_description_entry funs acc v =
    funs.constant_or_report_group_description_entry funs acc v

  let fm_report_description_entry funs acc v =
    funs.report_description_entry funs acc v

  let fm_constant_or_screen_description_entry funs acc v =
    funs.constant_or_screen_description_entry funs acc v

  let fm_file_or_sort_merge_description_entry funs acc v =
    funs.file_or_sort_merge_description_entry funs acc v

  let nop _ x y =
    y, x

  let environment_division funs acc (ed: environment_division with_loc) =
    let configuration_section, acc =
      (~&ed.configuration_section, acc) >>= fm_configuration_section funs
    in
    let input_output_section, acc =
      (~&ed.input_output_section, acc) >>= fm_input_output_section funs
    in
    {
      configuration_section;
      input_output_section;
    } &@<- ed, acc

  let configuration_section funs acc (cs: configuration_section) =
    let special_names_paragraph, acc =
      (cs.special_names_paragraph, acc) >>= fm_special_names_paragraph funs
    in
    {cs with
     special_names_paragraph;
    }, acc

  let input_output_section = nop

  let special_names_paragraph funs acc (snp: special_names_paragraph) =
    LIST.foldmap (snp, acc) ~f:(fun acc (sncl_loc) ->
        match ~&sncl_loc with
        | CurrencySign cs ->
            let cs, acc = fm_currency_sign funs acc (cs &@<- sncl_loc) in
            (CurrencySign ~&cs &@<- cs), acc
        | DecimalPoint dp ->
            let dp, acc = fm_decimal_point funs acc (dp &@<- sncl_loc) in
            (DecimalPoint ~&dp &@<- dp), acc
        | _ ->
            sncl_loc, acc)

  let qualname = nop

  let data_name = nop

  let level = nop

  let decimal_point = nop

  let currency_sign = nop

  let condition_name_entry = nop

  let rename_entry funs acc re_loc =
    let re = ~&re_loc in
    let renamed_item, acc = fm_qualname funs acc (re.renamed_item) in
    let through_opt, acc = (re.through_opt, acc) >>= fm_qualname funs in
    {re with
     renamed_item;
     through_opt
    } &@<- re_loc, acc

  let redefines_clause = nop

  let picture_clause _funs acc (pc: Data_sec_in.picture_clause with_loc) =
    let picture = Default_map.picture (~&pc).picture in
    {
      picture;
      locale_opt = ~&pc.locale_opt;
    } &@<- pc, acc

  let data_description_clause funs acc ddc =
    match ~&ddc with
    | Data_sec_in.Aligned ->
        Aligned &@<- ddc, acc
    | AnyLength ->
        AnyLength &@<- ddc, acc
    | Based ->
        Based &@<- ddc, acc
    | BlankWhenZero ->
        (BlankWhenZero: data_description_clause) &@<- ddc, acc
    | ConstantRecord ->
        ConstantRecord &@<- ddc, acc
    | DataOccurs do_ ->
        DataOccurs do_ &@<- ddc, acc
    | DataType dt ->
        DataType dt &@<- ddc, acc
    | DataValue dv ->
        DataValue dv &@<- ddc, acc
    | DynamicLength dl ->
        DynamicLength dl &@<- ddc, acc
    | External e ->
        External e &@<- ddc, acc
    | Global ->
        (Global: data_description_clause) &@<- ddc, acc
    | GroupUsage gu ->
        GroupUsage gu &@<- ddc, acc
    | Justified ->
        (Justified: data_description_clause) &@<- ddc, acc
    | Picture p ->
        let picture_clause, acc = fm_picture_clause funs acc (p &@<- ddc) in
        (Picture ~&picture_clause: data_description_clause) &@<- picture_clause, acc
    | Property p ->
        Property p &@<- ddc, acc
    | Redefines r ->
        let redefines_clause, acc = fm_redefines_clause funs acc (r &@<- ddc) in
        Redefines ~&redefines_clause &@<- ddc, acc
    | SameAs sa ->
        SameAs sa &@<- ddc, acc
    | SelectWhen sw ->
        SelectWhen sw &@<- ddc, acc
    | Sign s ->
        (Sign s: data_description_clause) &@<- ddc, acc
    | Synchronized s ->
        Synchronized s &@<- ddc, acc
    | Typedef t ->
        Typedef t &@<- ddc, acc
    | Usage u ->
        Usage u &@<- ddc, acc
    | Validation v ->
        Validation v &@<- ddc, acc

  let report_group_description_clause funs acc rgde =
    match ~&rgde with
    | Data_sec_in.ReportType rt ->
        ReportType rt &@<- rgde, acc
    | NextGroup ng ->
        NextGroup ng &@<- rgde, acc
    | ReportLine rl ->
        ReportLine rl &@<- rgde, acc
    | Picture p ->
        let picture_clause, acc = fm_picture_clause funs acc (p &@<- rgde) in
        (Picture ~&picture_clause: report_group_description_clause) &@<- rgde, acc
    | ReportScreenUsage rsu ->
        (ReportScreenUsage rsu: report_group_description_clause) &@<- rgde, acc
    | Sign s ->
        (Sign s: report_group_description_clause) &@<- rgde, acc
    | Justified ->
        (Justified: report_group_description_clause) &@<- rgde, acc
    | ReportColumn rc ->
        ReportColumn rc &@<- rgde, acc
    | BlankWhenZero ->
        (BlankWhenZero: report_group_description_clause) &@<- rgde, acc
    | Source s ->
        Source s &@<- rgde, acc
    | Sum s ->
        Sum s &@<- rgde, acc
    | ReportValue rv ->
        ReportValue rv &@<- rgde, acc
    | PresentWhen pw ->
        PresentWhen pw &@<- rgde, acc
    | GroupIndicate ->
        GroupIndicate &@<- rgde, acc
    | ReportOccurs ro ->
        ReportOccurs ro &@<- rgde, acc
    | Varying v ->
        Varying v &@<- rgde, acc

  let screen_description_clause funs acc sdc =
    match ~&sdc with
    | Data_sec_in.Auto ->
        Auto &@<- sdc, acc
    | Blank b ->
        Blank b &@<- sdc, acc
    | BlankWhenZero ->
        BlankWhenZero &@<- sdc, acc
    | Erase e ->
        Erase e &@<- sdc, acc
    | Full ->
        (Full: screen_description_clause) &@<- sdc, acc
    | Global ->
        Global &@<- sdc, acc
    | Justified ->
        Justified &@<- sdc, acc
    | Picture p ->
        let picture_clause, acc = fm_picture_clause funs acc (p &@<- sdc) in
        Picture ~&picture_clause &@<- sdc, acc
    | ReportScreenUsage rsu ->
        ReportScreenUsage rsu &@<- sdc, acc
    | Required ->
        Required &@<- sdc, acc
    | ScreenAttribute ra ->
        ScreenAttribute ra &@<- sdc, acc
    | ScreenColumn rc ->
        ScreenColumn rc &@<- sdc, acc
    | ScreenLine sl ->
        ScreenLine sl &@<- sdc, acc
    | ScreenOccurs so ->
        ScreenOccurs so &@<- sdc, acc
    | Secure ->
        Secure &@<- sdc, acc
    | Sign s ->
        Sign s &@<- sdc, acc
    | SourceDestination sd ->
        SourceDestination sd &@<- sdc, acc

  let data_description_entry funs acc (dde: Data_sec_in.data_description_entry with_loc) =
    let level, acc = fm_level funs acc ~&dde.level in
    let data_name, acc = fm_data_name funs acc ~&dde.data_name in
    let data_description_clauses, acc =
      LIST.foldmap (~&dde.data_description_clauses, acc) ~f:(fm_data_description_clause funs)
    in
    {
      level;
      data_name;
      data_description_clauses;
    } &@<- dde, acc

  let report_group_description_entry funs acc (rgde: Data_sec_in.report_group_description_entry) =
    let report_group_description_clauses, acc =
      LIST.foldmap (rgde.report_group_description_clauses, acc)
        ~f:(fm_report_group_description_clause funs)
    in
    {
      level = rgde.level;
      data_name = rgde.data_name;
      report_group_description_clauses
    }, acc

  let screen_description_entry funs acc (sde: Data_sec_in.screen_description_entry) =
    let screen_description_clauses, acc =
      LIST.foldmap (sde.screen_description_clauses, acc)
        ~f:(fm_screen_description_clause funs)
    in
    {
      level = sde.level;
      data_name = sde.data_name;
      screen_description_clauses;
    }, acc

  let constant_or_data_description_entry funs acc cdde =
    match ~&cdde with
    | (Data_sec_in.Constant ce: Data_sec_in.constant_or_data_description_entry) ->
        (Constant ce: constant_or_data_description_entry) &@<- cdde, acc
    | Data dde ->
        let dde, acc = fm_data_description_entry funs acc (dde &@<- cdde) in
        (Data ~&dde: constant_or_data_description_entry) &@<- cdde, acc
    | Renames re ->
        let re, acc = fm_rename_entry funs acc (re &@<- cdde) in
        (Renames ~&re: constant_or_data_description_entry) &@<- cdde, acc
    | CondName cde ->
        let cde, acc = fm_condition_name_entry funs acc (cde &@<- cdde) in
        (CondName ~&cde: constant_or_data_description_entry) &@<- cdde, acc

  let file_description_entry funs acc (fde: Data_sec_in.file_description_entry) =
    let constant_or_data_descriptions, acc =
      LIST.foldmap (fde.constant_or_data_descriptions, acc)
        ~f:(fm_constant_or_data_description_entry funs)
    in
    {
      file_name = fde.file_name;
      file_descriptions = fde.file_descriptions;
      constant_or_data_descriptions;
    }, acc

  let sort_merge_description_entry funs acc (smde: Data_sec_in.sort_merge_description_entry) =
    let constant_or_data_descriptions, acc =
      LIST.foldmap (smde.constant_or_data_descriptions, acc)
        ~f:(fm_constant_or_data_description_entry funs)
    in
    {
      file_name = smde.file_name;
      sort_merge_file_descriptions = smde.sort_merge_file_descriptions;
      constant_or_data_descriptions;
    }, acc

  let input_communication_entry funs acc (ice: Data_sec_in.input_communication_entry) =
    let constant_or_data_descriptions, acc =
      LIST.foldmap (ice.constant_or_data_descriptions, acc)
        ~f:(fm_constant_or_data_description_entry funs)
    in
    ({
      cd_name = ice.cd_name;
      initial = ice.initial;
      communication_descriptions = ice.communication_descriptions;
      data_items = ice.data_items;
      constant_or_data_descriptions;
    }: input_communication_entry), acc

  let io_communication_entry funs acc (ice: Data_sec_in.io_communication_entry) =
    let constant_or_data_descriptions, acc =
      LIST.foldmap (ice.constant_or_data_descriptions, acc)
        ~f:(fm_constant_or_data_description_entry funs)
    in
    {
      cd_name = ice.cd_name;
      initial = ice.initial;
      communication_descriptions = ice.communication_descriptions;
      data_items = ice.data_items;
      constant_or_data_descriptions;
    }, acc

  let output_communication_entry funs acc (oce: Data_sec_in.output_communication_entry) =
    let constant_or_data_descriptions, acc =
      LIST.foldmap (oce.constant_or_data_descriptions, acc)
        ~f:(fm_constant_or_data_description_entry funs)
    in
    {
      cd_name = oce.cd_name;
      communication_descriptions = oce.communication_descriptions;
      constant_or_data_descriptions;
    }, acc

  let communication_description_entry funs acc cde =
    match ~&cde with
    | Data_sec_in.(Input i) ->
        let i, acc = fm_input_communication_entry funs acc i in
        Input i &@<- cde, acc
    | Output o ->
        let o, acc = fm_output_communication_entry funs acc o in
        Output o &@<- cde, acc
    | IO io ->
        let io, acc = fm_io_communication_entry funs acc io in
        IO io &@<- cde, acc

  let constant_or_report_group_description_entry funs acc crgde =
    match ~&crgde with
    | (Data_sec_in.Constant ce: Data_sec_in.constant_or_report_group_description_entry)  ->
        (Constant ce: constant_or_report_group_description_entry) &@<- crgde, acc
    | ReportGroup rg ->
        let rg, acc = fm_report_group_description_entry funs acc rg in
        ReportGroup rg &@<- crgde, acc

  let report_description_entry funs acc (rde: Data_sec_in.report_description_entry with_loc) =
    let constant_or_report_group_descriptions, acc =
      LIST.foldmap (~&rde.constant_or_report_group_descriptions, acc)
        ~f:(fm_constant_or_report_group_description_entry funs)
    in
    {
      report_name = ~&rde.report_name;
      report_descriptions = ~&rde.report_descriptions;
      constant_or_report_group_descriptions;
    } &@<- rde, acc

  let constant_or_screen_description_entry funs acc csde =
    match ~&csde with
    | Data_sec_in.(Constant ce) ->
        Constant ce &@<- csde, acc
    | Screen sde ->
        let sde, acc = fm_screen_description_entry funs acc sde in
        Screen sde &@<- csde, acc

  let file_or_sort_merge_description_entry funs acc fsmde =
    match ~&fsmde with
    | Data_sec_in.(File fde) ->
        let fde, acc = fm_file_description_entry funs acc fde in
        File fde &@<- fsmde, acc
    | SortMergeFile smf ->
        let smf, acc =  fm_sort_merge_description_entry funs acc smf in
        SortMergeFile smf &@<- fsmde, acc
  (** This record contains the functions with the default behaviour of the mapfolder (do nothing) except
      for the picture where it applies the transformation provided by {!Annot_map.picture_repr}. *)
  let fm_default: 'a fm_funs = {
    environment_division;
    configuration_section;
    input_output_section;
    special_names_paragraph;
    currency_sign;
    decimal_point;
    condition_name_entry;
    rename_entry;
    qualname;
    data_name;
    level;
    redefines_clause;
    picture_clause;
    data_description_clause;
    screen_description_clause;
    report_group_description_clause;
    data_description_entry;
    report_group_description_entry;
    screen_description_entry;
    constant_or_data_description_entry;
    file_description_entry;
    sort_merge_description_entry;
    input_communication_entry;
    io_communication_entry;
    output_communication_entry;
    communication_description_entry;
    constant_or_report_group_description_entry;
    report_description_entry;
    constant_or_screen_description_entry;
    file_or_sort_merge_description_entry;
  }

end

module Make_data_div_traversal
    (Data_div_in: DATA_DIVISION_AST)
    (Data_div_out: DATA_DIVISION_AST)
    (Default_map: sig
       val file_section: Data_div_in.file_section -> Data_div_out.file_section
       val working_storage_section: Data_div_in.working_storage_section -> Data_div_out.working_storage_section
       val linkage_section: Data_div_in.linkage_section -> Data_div_out.linkage_section
       val communication_section: Data_div_in.communication_section -> Data_div_out.communication_section
       val local_storage_section: Data_div_in.local_storage_section -> Data_div_out.local_storage_section
       val report_section: Data_div_in.report_section -> Data_div_out.report_section
       val screen_section: Data_div_in.screen_section -> Data_div_out.screen_section
     end)
    =
struct
  (** This functor makes a mapfolder from {!In} to {!Out}, using {!Annot_map} to map the different
      representations from {!In} to {!Out}. *)
  module Data_div_in = Data_div_in
  module Data_div_out = Data_div_out

  open Data_div_in
  open Data_div_out


  (** This type is used to specify the types of the mapfold functions, ['a] is the type of
        the accumulator, ['b] is the type of the AST node that is given as input and
        ['c] is the type of the AST node that is given as output. *)
  type ('a, 'b, 'c) fm_fun = 'a fm_funs -> 'a -> 'b -> 'c * 'a

  (** This type is used to specify functions where the returns ast type does not change from the
       one that was given as input *)
  and ('a, 'b) fm_id_fun = ('a, 'b, 'b) fm_fun

  (** This record type is used to implement the mapfolder functions. *)
  and 'a fm_funs = {
    data_division: ('a,Data_div_in.data_division with_loc, Data_div_out.data_division with_loc) fm_fun;
    file_section: ('a, Data_div_in.file_section, Data_div_out.file_section) fm_fun;
    working_storage_section: ('a, Data_div_in.working_storage_section, Data_div_out.working_storage_section) fm_fun;
    linkage_section: ('a, Data_div_in.linkage_section, Data_div_out.linkage_section) fm_fun;
    communication_section: ('a, Data_div_in.communication_section, Data_div_out.communication_section) fm_fun;
    local_storage_section: ('a, Data_div_in.local_storage_section, Data_div_out.local_storage_section) fm_fun;
    report_section: ('a, Data_div_in.report_section, Data_div_out.report_section) fm_fun;
    screen_section: ('a, Data_div_in.screen_section, Data_div_out.screen_section) fm_fun;
  }

  let fm_data_division funs acc v =
    funs.data_division funs acc v

  let fm_file_section funs acc v =
    funs.file_section funs acc v

  let fm_working_storage_section funs acc v =
    funs.working_storage_section funs acc v

  let fm_linkage_section funs acc v =
    funs.linkage_section funs acc v

  let fm_communication_section funs acc v =
    funs.communication_section funs acc v

  let fm_local_storage_section funs acc v =
    funs.local_storage_section funs acc v

  let fm_report_section funs acc v =
    funs.report_section funs acc v

  let fm_screen_section funs acc v =
    funs.screen_section funs acc v

  let nop _ x y =
    y, x

  let qualname = nop

  let data_name = nop

  let level = nop

  let file_section _funs acc fs =
    Default_map.file_section fs, acc

  let working_storage_section _funs acc wss =
    Default_map.working_storage_section wss, acc

  let linkage_section _funs acc ls =
    Default_map.linkage_section ls, acc

  let communication_section _funs acc cs =
    Default_map.communication_section cs, acc

  let local_storage_section _funs acc lss =
    Default_map.local_storage_section lss, acc

  let report_section _funs acc rs =
    Default_map.report_section rs, acc

  let screen_section _funs acc ss =
    Default_map.screen_section ss, acc

  let data_division funs acc (dd:Data_div_in.data_division with_loc) =
    let file_section, acc =
      (~&dd.file_section, acc) >>= fm_file_section funs
    in
    let working_storage_section, acc =
      (~&dd.working_storage_section, acc) >>= fm_working_storage_section funs
    in
    let linkage_section, acc =
      (~&dd.linkage_section, acc) >>= fm_linkage_section funs
    in
    let communication_section, acc =
      (~&dd.communication_section, acc) >>= fm_communication_section funs
    in
    let local_storage_section, acc =
      (~&dd.local_storage_section, acc) >>= fm_local_storage_section funs
    in
    let report_section, acc =
      (~&dd.report_section, acc) >>= fm_report_section funs
    in
    let screen_section, acc =
      (~&dd.screen_section, acc) >>= fm_screen_section funs
    in
    {
      file_section;
      working_storage_section;
      linkage_section;
      communication_section;
      local_storage_section;
      report_section;
      screen_section;
    } &@<- dd, acc

  (** This record contains the functions with the default behaviour of the mapfolder (do nothing) except
      for the picture where it applies the transformation provided by {!Annot_map.picture_repr}. *)
  let fm_default: 'a fm_funs = {
    data_division;
    file_section;
    working_storage_section;
    linkage_section;
    communication_section;
    local_storage_section;
    report_section;
    screen_section;
  }

end

module Make
    (AstIn: Ast.S)
    (AstOut: Ast.S)
    (Default_map: sig
       val picture: AstIn.Data_section.Pic_repr.picture -> AstOut.Data_section.Pic_repr.picture
       val file_section: AstIn.Data_div.file_section -> AstOut.Data_div.file_section
       val working_storage_section: AstIn.Data_div.working_storage_section -> AstOut.Data_div.working_storage_section
       val linkage_section: AstIn.Data_div.linkage_section -> AstOut.Data_div.linkage_section
       val communication_section: AstIn.Data_div.communication_section -> AstOut.Data_div.communication_section
       val local_storage_section: AstIn.Data_div.local_storage_section -> AstOut.Data_div.local_storage_section
       val report_section: AstIn.Data_div.report_section -> AstOut.Data_div.report_section
       val screen_section: AstIn.Data_div.screen_section -> AstOut.Data_div.screen_section
     end)
    =
struct
  (** This functor makes a mapfolder from {!In} to {!Out}, using {!Annot_map} to map the different
      representations from {!In} to {!Out}. *)
  module Data_sec_traversal = Make_data_sections_traversal (AstIn.Data_section) (AstOut.Data_section) (struct
      let picture = Default_map.picture end)
  module Data_div_traversal = Make_data_div_traversal (AstIn.Data_div) (AstOut.Data_div) (struct
        let file_section = Default_map.file_section
        let working_storage_section = Default_map.working_storage_section
        let linkage_section = Default_map.linkage_section
        let communication_section = Default_map.communication_section
        let local_storage_section = Default_map.local_storage_section
        let report_section = Default_map.report_section
        let screen_section = Default_map.screen_section
    end)

  open AstOut
  (** This type is used to specify the types of the mapfold functions, ['a] is the type of
        the accumulator, ['b] is the type of the AST node that is given as input and
        ['c] is the type of the AST node that is given as output. *)
  type ('a, 'b, 'c) fm_fun = 'a fm_funs -> 'a -> 'b -> 'c * 'a

  (** This type is used to specify functions where the returns ast type does not change from the
       one that was given as input *)
  and ('a, 'b) fm_id_fun = ('a, 'b, 'b) fm_fun

  (** This record type is used to implement the mapfolder functions. *)
  and 'a fm_funs = {
    compilation_group: ('a, AstIn.compilation_group, AstOut.compilation_group) fm_fun;
    compilation_unit: ('a, AstIn.compilation_unit with_loc, AstOut.compilation_unit with_loc) fm_fun;
    program_definition: ('a, AstIn.program_definition, AstOut.program_definition) fm_fun;
    program_prototype: ('a, AstIn.program_prototype, AstOut.program_prototype) fm_fun;
    function_definition: ('a, AstIn.function_definition, AstOut.function_definition) fm_fun;
    function_prototype: ('a, AstIn.function_prototype, AstOut.function_prototype) fm_fun;
    data_sec_traversal_funs: 'a Data_sec_traversal.fm_funs;
    data_div_traversal_funs: 'a Data_div_traversal.fm_funs;
  }

  let fm_compilation_group funs acc cg =
    funs.compilation_group funs acc cg

  let fm_compilation_unit funs acc cu =
    funs.compilation_unit funs acc cu

  let fm_program_definition funs acc pd =
    funs.program_definition funs acc pd

  let fm_program_prototype funs acc pp =
    funs.program_prototype funs acc pp

  let fm_function_definition funs acc fd =
    funs.function_definition funs acc fd

  let fm_function_prototype funs acc fp =
    funs.function_prototype funs acc fp

  let nop _ x y =
    y, x

  let function_prototype funs acc (fp: AstIn.function_prototype) =
    let environment_division, acc =
      (fp.environment_division, acc)
      >>= (Data_sec_traversal.fm_environment_division funs.data_sec_traversal_funs)
    in
    let data_division, acc =
      (fp.data_division, acc)
      >>= Data_div_traversal.fm_data_division funs.data_div_traversal_funs
    in
    {
      function_prototype_id_paragraph = fp.function_prototype_id_paragraph;
      options_paragraph = fp.options_paragraph;
      environment_division;
      data_division;
      procedure_division = fp.procedure_division;
      end_function = fp.end_function;
    }, acc

  let function_definition funs acc (fd: AstIn.function_definition) =
    let environment_division, acc =
      (fd.environment_division, acc)
      >>= (Data_sec_traversal.fm_environment_division funs.data_sec_traversal_funs)
    in
    let data_division, acc =
      (fd.data_division, acc)
      >>= Data_div_traversal.fm_data_division funs.data_div_traversal_funs
    in
    {
      function_id_paragraph = fd.function_id_paragraph;
      options_paragraph = fd.options_paragraph;
      environment_division;
      data_division;
      procedure_division = fd.procedure_division;
      end_function = fd.end_function;
    }, acc

  let program_prototype funs acc (pp: AstIn.program_prototype) =
    let environment_division, acc =
      (pp.environment_division, acc)
      >>= (Data_sec_traversal.fm_environment_division funs.data_sec_traversal_funs)
    in
    let data_division, acc =
      (pp.data_division, acc)
      >>= Data_div_traversal.fm_data_division funs.data_div_traversal_funs
    in
    {
      program_prototype_id_paragraph = pp.program_prototype_id_paragraph;
      options_paragraph = pp.options_paragraph;
      environment_division;
      data_division;
      procedure_division = pp.procedure_division;
      end_program = pp.end_program;
    }, acc

  let program_definition funs acc (pd: AstIn.program_definition) =
    let environment_division, acc =
      (pd.environment_division, acc)
      >>= (Data_sec_traversal.fm_environment_division funs.data_sec_traversal_funs)
    in
    let data_division, acc =
      (pd.data_division, acc)
      >>= Data_div_traversal.fm_data_division funs.data_div_traversal_funs
    in
    let nested_programs, acc =
      LIST.foldmap (pd.nested_programs, acc) ~f:(fun acc np_loc ->
          let np, acc = fm_program_definition funs acc ~&np_loc in
          np &@<- np_loc, acc)
    in
    {
      has_identification_division = pd.has_identification_division;
      program_id_paragraph = pd.program_id_paragraph;
      informational_paragraphs = pd.informational_paragraphs;
      options_paragraph = pd.options_paragraph;
      environment_division;
      data_division;
      procedure_division = pd.procedure_division;
      nested_programs;
      end_program = pd.end_program;
    }, acc

  let compilation_unit funs acc cu =
    match ~&cu with
    | AstIn.ProgramDefinition (pd) ->
        let pd, acc = fm_program_definition funs acc pd in
        (ProgramDefinition pd) &@<- cu, acc
    | ProgramPrototype (pp) ->
        let pp, acc = fm_program_prototype funs acc pp in
        (ProgramPrototype pp) &@<- cu, acc
    | FunctionDefinition (fd) ->
        let fd, acc = fm_function_definition funs acc fd in
        (FunctionDefinition fd) &@<- cu, acc
    | FunctionPrototype (fp) ->
        let fp, acc = fm_function_prototype funs acc fp in
        (FunctionPrototype fp) &@<- cu, acc
    | _ -> failwith "Not implemented yet"

  let compilation_group funs acc (cg: AstIn.compilation_group) =
    LIST.foldmap (cg, acc) ~f:(fm_compilation_unit funs)

  (** This record contains the functions with the default behaviour of the mapfolder (do nothing) except
      for the picture where it applies the transformation provided by {!Annot_map.picture_repr}. *)
  let fm_default: 'a fm_funs = {
    compilation_group;
    compilation_unit;
    program_definition;
    program_prototype;
    function_definition;
    function_prototype;
    data_sec_traversal_funs = Data_sec_traversal.fm_default;
    data_div_traversal_funs = Data_div_traversal.fm_default;
  }


end

module Id_default_mapper = struct
       let picture = Fun.id
       let file_section = Fun.id
       let working_storage_section = Fun.id
       let linkage_section = Fun.id
       let communication_section = Fun.id
       let local_storage_section = Fun.id
       let report_section = Fun.id
       let screen_section = Fun.id
end

module MakeId (Ast: Ast.S) = Make (Ast) (Ast) (Id_default_mapper)
*)
