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

[@@@ocaml.warning "-37-32"]

open EzCompat                                                    (* StringSet *)
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module DIAGS = Cobol_common.Diagnostics
module Visitor = Cobol_common.Visitor
module NEL = Cobol_data.Types.NEL

let name = Cobol_unit.Qual.name
let qual = Cobol_unit.Qual.qual
let name_of = Cobol_unit.Qual.name_of
let qual_of = Cobol_unit.Qual.qual_of
let requal = Cobol_unit.Qual.requal

let pp_data_name'_opt
  : Cobol_ptree.data_name with_loc option Pretty.printer
  = fun ppf -> function
    | Some { payload = DataName n; _ } ->
        Pretty.print ppf "item '%s'" ~&n
    | Some { payload = DataFiller; _ } | None ->
        Pretty.print ppf "FILLER item"

let pp_one_of pp_e ppf = function                   (* assumes non-empty list *)
  | [s] -> pp_e ppf s
  | lst -> Fmt.(hbox @@ any "one of: " ++ list ~sep:comma pp_e) ppf lst

(* --- *)

type error =
  | Item_not_allowed_in_section of
      {
        level: int with_loc;
        section: Cobol_data.Types.data_storage;
      }
  | Invalid_level_number of
      {
        level: int with_loc;
      }
  | Unexpected_level_number of
      {
        level: int with_loc;
        expected: int list;
      }
  | Misplaced_redefinition of
      {
        loc: srcloc;
        expl: misplacement_explanation;
      }
  | Unexpected_redefinition_level of
      {
        redef_loc: srcloc;
        redef_name: Cobol_ptree.data_name with_loc option;
        redef_level: int with_loc;
        expected_level: int;
        expected_redefined_loc: srcloc;
      }
  | Unexpected_redefinition_name of
      {
        redef_loc: srcloc;
        redef_name: Cobol_ptree.data_name with_loc option;
        redef_redefines: Cobol_ptree.name with_loc;
        expected_name: string;
        alias_used: string option;
      }
  | Redefinition_of_ODO_item of
      {
        redef_loc: srcloc;
        redef_name: Cobol_ptree.data_name with_loc option;
        redef_redefines: Cobol_ptree.name with_loc;
        odo_item: (* (_, [>`variable_length])  *)Cobol_data.Types.item_definition;
      }
  | Redefinition_of_table_item of
      {
        redef_loc: srcloc;
        redef_name: Cobol_ptree.data_name with_loc option;
        redef_redefines: Cobol_ptree.name with_loc;
        table_item: (* ([>`table], _)  *)Cobol_data.Types.item_definition;
      }
  | Misplaced_renaming of
      {
        loc: srcloc;
        expl: misplacement_explanation;
      }
  | Missing_picture_clause_for_elementary_item of
      {
        item_name: Cobol_ptree.data_name with_loc option;
        item_loc: srcloc;
      }
  | Unexpected_picture_clause_for_group_item of
      {
        picture: Cobol_ptree.picture_clause with_loc;
        item_name: Cobol_ptree.data_name with_loc option;
        item_loc: srcloc;
      }
  | Unexpected_table_value_clause of
      {
        item_name: Cobol_ptree.data_name with_loc option;
        value_loc: srcloc;
      }
  | Occurs_in_rename_operand of
      {
        operand: Cobol_ptree.qualname with_loc;
        item: Cobol_data.Types.item_definition;
      }
  | Invalid_renaming_of_variable_length_range of
      {
        loc: srcloc;
        depending_vars: Cobol_ptree.qualname NEL.t;
      }
  | Duplicate_clause of
      {
        first_loc: srcloc;
        second_loc: srcloc;
        clause_name: string;
      }
  | Picture_error of
      {
        picture_loc: srcloc;
        error: Cobol_data.Picture.TYPES.error with_loc;
      }
  | Item_not_found of
      {
        qualname: Cobol_ptree.qualname with_loc;
      }
  | Pending_feature of
      {
        name: string;
        loc: srcloc;
      }

and misplacement_explanation =
  | Following of string
  | Following_level of int

let pp_misplacement_explanation ppf = function
  | Following s ->
      Pretty.print ppf "following@ %s" s
  | Following_level l ->
      Pretty.print ppf "following@ item@ at@ level@ %02d" l

(** more general diagnostics *)
type diagnostic =
  | Error_diagnostic of
      error
  | Extraneous_clause of
      {
        clause_name: string;
        clause_loc: srcloc;
      }

let diagnostic_severity = function
  | Error_diagnostic _ -> DIAGS.Error
  | Extraneous_clause _ -> DIAGS.Warn

let error_loc = function
  | Duplicate_clause { second_loc = loc; _ }
  | Invalid_level_number { level = { loc; _ }; _ }
  | Invalid_renaming_of_variable_length_range { loc; _ }
  | Item_not_allowed_in_section { level = { loc; _ }; _ }
  | Item_not_found { qualname = { loc; _ } }
  | Misplaced_redefinition { loc; _ }
  | Misplaced_renaming { loc; _ }
  | Missing_picture_clause_for_elementary_item { item_loc = loc; _ }
  | Occurs_in_rename_operand { operand = { loc; _ }; _ }
  | Pending_feature { loc; _ }
  | Picture_error { error = { loc; _ }; _ }
  | Redefinition_of_ODO_item { redef_loc = loc; _ }
  | Redefinition_of_table_item { redef_loc = loc; _ }
  | Unexpected_level_number { level = { loc; _ }; _ }
  | Unexpected_picture_clause_for_group_item { picture = { loc; _ }; _ }
  | Unexpected_redefinition_level { redef_level = { loc; _ }; _ }
  | Unexpected_redefinition_name { redef_redefines = { loc; _ }; _ }
  | Unexpected_table_value_clause { value_loc = loc; _ } ->
      Some loc

let diagnostic_loc = function
  | Error_diagnostic e ->
      error_loc e
  | Extraneous_clause { clause_loc = loc; _ } ->
      Some loc

let pp_error ppf = function
  | Item_not_allowed_in_section { level; section } ->
      Pretty.print ppf "%d-level item not allowed in %a section." ~&level
        Cobol_data.Types.pp_data_storage section
  | Invalid_level_number { level } ->
      Pretty.print ppf "Invalid level number: %02d" ~&level
  | Unexpected_level_number { level; expected } ->
      Pretty.print ppf "Unexpected level number %02d: expected %a" ~&level
        (pp_one_of Fmt.(fmt "%02d")) expected
  | Misplaced_redefinition { expl; _ } ->
      Pretty.print ppf "Misplaced REDEFINES %a"
        pp_misplacement_explanation expl
  | Unexpected_redefinition_level { expected_level; redef_level;
                                    redef_name; _ } ->
      Pretty.print ppf "Invalid level %02d for %a with REDEFINES clause; \
                        expected level %02d"
        ~&redef_level pp_data_name'_opt redef_name expected_level
  | Unexpected_redefinition_name { expected_name; redef_redefines;
                                   redef_name; alias_used; _ } ->
      Pretty.print ppf "Unexpected target name '%s' in REDEFINES clause for %a; \
                        expected %s'%s'"
        ~&redef_redefines pp_data_name'_opt redef_name
        (if alias_used = None then "" else "name of original definition ")
        expected_name
  | Redefinition_of_ODO_item { odo_item; _ } ->
      Pretty.print ppf "Invalid redefinition of record with subordinate OCCURS \
                        DEPENDING%a"
        Fmt.(option (sp ++ Cobol_ptree.pp_qualname)) odo_item.item_qualname
  | Redefinition_of_table_item { table_item; _ } ->
      Pretty.print ppf "Invalid redefinition of item with OCCURS clause%a"
        Fmt.(option (sp ++ Cobol_ptree.pp_qualname)) table_item.item_qualname
  | Misplaced_renaming { expl; _ } ->
      Pretty.print ppf "Misplaced@ RENAMES@ %a\
                       " pp_misplacement_explanation expl
  | Missing_picture_clause_for_elementary_item { item_name; _ } ->
      Pretty.print ppf "Missing PICTURE clause for %a"
        pp_data_name'_opt item_name
  | Unexpected_picture_clause_for_group_item { item_name; _ } ->
      Pretty.print ppf "Unexpected PICTURE clause for group %a"
        pp_data_name'_opt item_name
  | Unexpected_table_value_clause { item_name; _ } ->
      Pretty.print ppf "Unexpected table VALUE clause for %a"
        pp_data_name'_opt item_name
  | Occurs_in_rename_operand { operand; _ } ->
      Pretty.print ppf "RENAMES operand '%a' has an OCCURS clause"
        Cobol_ptree.pp_qualname' operand
  | Invalid_renaming_of_variable_length_range { depending_vars = vars; _ } ->
      Pretty.print ppf "Renaming of variable-length range (length depends on: %a)"
        Fmt.(list ~sep:comma Cobol_ptree.pp_qualname) (NEL.to_list vars)
  | Duplicate_clause { clause_name; _ } ->   (* TODO: addendum with second loc *)
      Pretty.print ppf "Duplicate '%s' clause" clause_name
  | Picture_error { error; _ } ->
      Cobol_data.Picture.pp_error ppf ~&error
  | Item_not_found { qualname; _ } ->
      Pretty.print ppf "Item '%a' not found" Cobol_ptree.pp_qualname' qualname
  | Pending_feature { name; _ } ->
      Pretty.print ppf "%s is not supported yet" name

let pp_diagnostic ppf = function
  | Error_diagnostic e ->
      pp_error ppf e
  | Extraneous_clause { clause_name; _ } ->
      Pretty.print ppf "Extraneous %s clause" clause_name

(* --- *)

type diagnostics = diagnostic list

let translate_diags diagnostics =
  (* Temporary hack: reverse errors list so order of generated diagnostics
     corresponds to order of emission in the code below. *)
  List.fold_left begin fun diags diag ->
    DIAGS.Acc.diag (diagnostic_severity diag) diags
      ?loc:(diagnostic_loc diag) "%a" pp_diagnostic diag
  end DIAGS.Set.none (List.rev diagnostics)

let diags_union d1 d2 = d2 @ d1

(* --- *)

type acc =
  {
    current_storage: Cobol_data.Types.data_storage;
    current_qualification: Cobol_ptree.qualname option;
    item_stack: item_stack;
    (* pending_renames: Cobol_data.Types.record_renaming list; *)
    current_qualmap: Cobol_data.Types.item_definition Cobol_unit.Qualmap.t;
    filler_count: int;
    (* pending_record_definition: committed_record_definition option; *)
    definitions: Cobol_unit.Types.data_definitions;
    picture_config: Cobol_data.Types.picture_config;
    diags: diagnostics;
  }
and item_stack = item_under_construction list
and item_under_construction =               (* item currently being assembled *)
  {
    item_level: int;                                         (* 01 <= _ <= 49 *)
    item_name: Cobol_ptree.data_name with_loc option;
    item_loc: srcloc;
    item_qualname: Cobol_ptree.qualname option;        (* full qualified name *)
    item_offset: Cobol_data.Memory.offset;
    item_size: Cobol_data.Memory.size;                   (* if non-elementary *)
    item_clauses: data_clauses;
    item_redefines: (string * StringSet.t) option;   (* REDEFINES iff <> None *)
    item_rev_fields: Cobol_data.Types.item_definition list;
    item_rev_renames: Cobol_data.Types.record_renaming list;
    (* TODO: subscripting info? + others? *)
  }
and data_clauses =
  {
    occurs: occurs_under_construction;
    picture: Cobol_ptree.picture_clause with_loc option;
    values: Cobol_ptree.data_value_clause with_loc list;
    redefines: Cobol_ptree.name with_loc option;
    clause_diags: diagnostics;
  }
(* and committed_record_definition = *)
(*   | Committed of           (\* ... but can still be amended by renames/redefines *\) *)
(*       { *)
(*         def: (\* _  *\)Cobol_data.Types.item_definition; *)
(*         (\* rev_renames: renames_under_construction with_loc list; *\) *)
(*       } *)

and occurs_under_construction =
  | OccursOnce
  | FixedOccurs of { length: Cobol_ptree.integer with_loc;
                     (* value: Cobol_ptree.literal option; *)
                     loc: srcloc }
  | OccursDepending of { min_occurs: Cobol_ptree.integer with_loc;
                         max_occurs: Cobol_ptree.integer with_loc;
                         depending: Cobol_ptree.qualname with_loc;
                         (* value: Cobol_ptree.literal with_loc option; *)
                         loc: srcloc }
  | OccursDynamic of { capacity: Cobol_ptree.name with_loc option;
                       min_capacity: Cobol_ptree.integer with_loc option;
                       max_capacity: Cobol_ptree.integer with_loc option;
                       initialized: bool with_loc;
                       loc: srcloc }

(* and rename_under_construction = *)
(*   { *)
(*     (\* rename_to: Cobol_ptree.name Cobol_ptree.qual_ Cobol_ptree.term; *\) *)
(*     rename_to: Cobol_ptree.qualname; *)
(*     rename_renamed: Cobol_ptree.qualname with_loc; *)
(*     rename_through: Cobol_ptree.qualname with_loc option; *)
(*     rename_loc: srcloc; *)
(*   } *)

open Cobol_unit.Types
open Cobol_data.Types

let init (config: unit_config) =
  {
    current_storage = File;
    current_qualification = None;
    item_stack = [];
    (* pending_renames = []; *)
    current_qualmap = Cobol_unit.Qualmap.empty;
    filler_count = 0;
    (* pending_record_definition = None; *)
    definitions =
      {
        data_items = { named = Cobol_unit.Qualmap.empty; list = [] };
        data_records = [];
      };
    picture_config = {
      max_pic_length = 100;
      decimal_char = config.unit_decimal_point;
      currency_signs = config.unit_currency_signs;
    };
    diags = [];
  }

let error acc error = { acc with diags = Error_diagnostic error :: acc.diags }
let warn acc d = { acc with diags = d :: acc.diags }

let result { definitions; diags; _ } =
  DIAGS.result ~diags:(translate_diags diags) definitions

let pp_item_stack ppf acc =
  Fmt.(hbox @@ any "[" ++ list ~sep:comma pp_data_name'_opt ++ any "]") ppf @@
  List.rev_map (fun { item_name; _ } -> item_name) acc.item_stack


let record_name acc : Cobol_ptree.qualname option -> acc * string = function
  | None ->
      { acc with filler_count = succ acc.filler_count },
      Pretty.to_string "FILLER %u" (succ acc.filler_count)
  | Some qn ->
      acc, name_of qn

(* --- *)


let define_record acc ~renamings record_item =
  let acc, record_name = record_name acc record_item.item_qualname in
  (* let record_renamings = List.rev acc.pending_renames in *)
  let record = { record_name; record_storage = acc.current_storage;
                 record_item; record_renamings = renamings } in
  let add_named_def qn def { named; list } =
    { named = Cobol_unit.Qualmap.add qn def named;
      list = def :: list }
  and add_anonymous_def def { named; list } =
    { named;
      list = def :: list }
  in
  let data_items =
    Cobol_data.Item.fold_definitions record_item acc.definitions.data_items
      ~f:begin function
        | { item_qualname = Some qn; _ } as def ->
            add_named_def qn (Data_item { record; def })
        | { item_qualname = None; _ } as def ->
            add_anonymous_def (Data_item { record; def })
      end
  in
  let data_items =                                      (* add renaming items *)
    List.fold_left begin fun data_items def ->
      add_named_def def.renaming_name (Data_renaming { record; def }) data_items
    end data_items renamings
  in
  { acc with
    current_qualmap = Cobol_unit.Qualmap.empty;
    definitions = { data_items;
                    data_records = record :: acc.definitions.data_records } }


let commit_def ~renamings def acc =
  match acc.item_stack with
  | [] ->
      define_record acc ~renamings def
  | { item_loc; item_size; _ } as top_item :: item_stack ->
      let field_size = def.item_size in
      let top_item =
        { top_item with
          item_loc = Cobol_common.Srcloc.concat item_loc def.item_layout.loc;
          item_size = Cobol_data.Memory.increase item_size ~by:field_size;
          item_rev_fields = def :: top_item.item_rev_fields } in
      { acc with item_stack = top_item :: item_stack }


let dummy_picture_clause acc pic =
  match Cobol_data.Picture.(of_string acc.picture_config pic) with
  | Ok picture -> Ok (acc, picture)
  | Error _ -> Error acc


let translate_picture_clause acc
    { payload = Cobol_ptree.{ picture;
                              picture_locale = _;
                              picture_depending = _ }; loc = _ } =
  match Cobol_data.Picture.(of_string acc.picture_config ~&picture) with
  | Ok picture ->
      Ok (acc, picture)
  | Error (errors, _) ->                    (* note: errors are still reversed *)
      Cobol_data.Picture.rev_errors_with_loc ~loc:~@picture errors |>
      List.fold_left begin fun acc err ->
        error acc (Picture_error { picture_loc = ~@picture; error = err })
      end acc |>
      Result.error


let warn_about_extraneous_clauses ~clause_name acc clauses =
  List.fold_left begin fun acc { loc; _ } ->
    warn acc (Extraneous_clause { clause_name; clause_loc = loc })
  end acc clauses


let elementary_picture_n_value acc
    { item_name; item_loc; item_clauses = { picture; values; _ };_ } =
  let acc, value, extra_clauses = match List.rev values with
    | { payload = ValueTable _; loc } :: extra_clauses ->
        error acc @@
        Unexpected_table_value_clause { item_name; value_loc = loc },
        None, extra_clauses
    | { payload = ValueData literal; _ } :: extra_clauses ->
        acc, Some literal, extra_clauses
    | [] ->
        acc, None, []
  in
  let acc =
    warn_about_extraneous_clauses acc extra_clauses ~clause_name:"VALUE"
  in
  match picture, value with
  | Some picture, _ ->
      (match translate_picture_clause acc picture with
       | Error acc -> Error acc
       | Ok (acc, picture) -> Ok (acc, picture, None))
  | None, Some _ ->
      (match dummy_picture_clause acc "X" with             (* XXX: temporary! *)
       | Error acc -> Error acc
       | Ok (acc, picture) -> Ok (acc, picture, value))
  | None, _ ->
      let acc =
        error acc (Missing_picture_clause_for_elementary_item { item_name;
                                                                item_loc })
      in
      (match dummy_picture_clause acc "X" with
       | Error acc -> Error acc
       | Ok (acc, picture) -> Ok (acc, picture, None))


(* let group_picture_n_value acc *)
(*     { item_name; item_loc; item_clauses = { picture; values; _ };_ } = *)
(*   let acc, value = match value with *)
(*     | Some { payload = ValueTable _; loc } -> *)
(*         error acc (Unexpected_table_value_clause { item_name; *)
(*                                                    value_loc = loc }), None *)
(*     | Some { payload = ValueData literal; _ } -> *)
(*         acc, Some literal *)
(*     | None -> *)
(*         acc, None *)
(*   in *)
(*   match picture, value with *)
(*   | Some picture, _ -> *)
(*       (match translate_picture_clause acc picture with *)
(*        | Error acc -> Error acc *)
(*        | Ok (acc, picture) -> Ok (acc, picture, None)) *)
(*   | None, Some _ -> *)
(*       (match dummy_picture_clause acc "X" with             (\* XXX: temporary! *\) *)
(*        | Error acc -> Error acc *)
(*        | Ok (acc, picture) -> Ok (acc, picture, value)) *)
(*   | None, _ -> *)
(*       let acc = *)
(*         error acc (Missing_picture_clause_for_elementary_item { item_name; *)
(*                                                                 item_loc }) *)
(*       in *)
(*       (match dummy_picture_clause acc "X" with *)
(*        | Error acc -> Error acc *)
(*        | Ok (acc, picture) -> Ok (acc, picture, None)) *)


let register_def acc def =
  match def.item_qualname with
  | Some qn ->
      { acc with
        current_qualmap = Cobol_unit.Qualmap.add qn def acc.current_qualmap }
  | None -> acc


let item_definition acc
    ({ item_name; item_loc; item_qualname; item_offset; item_size;
       item_clauses = { occurs; picture; values; clause_diags; _ };
       item_rev_fields; _ } as item)
  : (acc * Cobol_data.Types.item_definition, acc) result =
  let open Cobol_data.Types in
  let acc = { acc with diags = diags_union acc.diags clause_diags } in
  let item_definitions_length item_definitions =
    NEL.fold_left Fixed_length item_definitions ~f:begin fun l item_def ->
      if item_def.item_length = Variable_length
      then Variable_length
      else l
    end
  in
  let qualify n = match n, item_qualname with
    | n, Some qn -> qual n (qual_of qn) &@<- n
    | n, None -> name n &@<- n
  in
  let opt = Option.map in
  let int = Cobol_common.Srcloc.locmap int_of_string in
  let elementary acc ({ item_layout = { loc; _ }; _ } as item) =
    match occurs with
    | OccursOnce ->
        register_def acc item, item
    | FixedOccurs { length; loc = _ } ->
        let length = int length in
        register_def acc item, {
          item_qualname = None;                            (* implicit FILLER *)
          item_layout = FixedTable { items = NEL.One item;
                                     length; value = None } &@ loc;
          item_offset = item.item_offset;
          item_size = Cobol_data.Memory.mult_int item.item_size ~&length;
          item_length = Fixed_length;
          item_redefinitions = [];
        }
    | OccursDepending { min_occurs; max_occurs; depending; loc = _ } ->
        let dep_size = Cobol_data.Memory.valof ~&depending in
        register_def acc item, {
          item_qualname = None;                            (* implicit FILLER *)
          item_layout = DependingTable { items = NEL.One item;
                                         min_occurs = int min_occurs;
                                         max_occurs = int max_occurs;
                                         depending;
                                         value = None } &@ loc;
          item_offset = item.item_offset;
          item_size = Cobol_data.Memory.repeat item.item_size ~by:dep_size;
          item_length = Variable_length;
          item_redefinitions = [];
        }
    | OccursDynamic { capacity; min_capacity; max_capacity;
                      initialized; loc = _ } ->
        register_def acc item, {
          item_qualname = None;                            (* implicit FILLER *)
          item_layout = DynamicTable { items = NEL.One item;
                                       capacity = opt qualify capacity;
                                       min_capacity = opt int min_capacity;
                                       max_capacity = opt int max_capacity;
                                       initialized;
                                       value = None } &@ loc;
          item_offset = item.item_offset;
          item_size = Cobol_data.Memory.size_of_dynamic_table;
          item_length = Fixed_length;
          item_redefinitions = [];
        }
  in
  let group items =
    let item_layout, item_length =
      match occurs with
      | OccursOnce ->
          Struct { fields = items },
          item_definitions_length items
      | FixedOccurs { length; loc = _ } ->
          FixedTable { items;
                       length = int length;
                       value = None },
          item_definitions_length items
      | OccursDepending { min_occurs; max_occurs; depending; loc = _ } ->
          DependingTable { items;
                           min_occurs = int min_occurs;
                           max_occurs = int max_occurs;
                           depending; value = None },
          Variable_length
      | OccursDynamic { capacity; min_capacity; max_capacity;
                        initialized; loc = _ } ->
          DynamicTable { items;
                         capacity = opt qualify capacity;
                         min_capacity = opt int min_capacity;
                         max_capacity = opt int max_capacity;
                         initialized; value = None },
          Fixed_length
    in
    {
      item_qualname;
      item_layout = item_layout &@ item_loc;
      item_offset = (NEL.hd items).item_offset;
      item_size;
      item_length;
      item_redefinitions = []
    }
  in
  if item_rev_fields = [] then         (* elementary (or table of elementary) *)
    match elementary_picture_n_value acc item with
    | Ok (acc, picture, value) ->
        let data_size = Cobol_data.Picture.data_size picture.category in
        Ok (elementary acc
              { item_qualname;
                item_layout = Elementary { picture;
                                           value } &@ item_loc;
                item_offset;
                item_size = Cobol_data.Memory.const_size data_size;
                item_length = Fixed_length;
                item_redefinitions = [] })
    | Error acc ->
        Error acc                                               (* just skip *)
  else                                                          (* group item *)
    let items = NEL.of_rev_list item_rev_fields in
    match picture, values with
    | Some picture, _ ->                   (* TODO: recover to proceed anyways *)
        Error (error acc @@
               Unexpected_picture_clause_for_group_item { picture;
                                                          item_name;
                                                          item_loc })
    | _ ->
        let def = group items in
        Ok (register_def acc def, def)


let item_definitions acc items =
  List.fold_left begin fun (acc, rev_defs, renamings) item ->
    match item_definition acc item with
    | Ok (acc, def) ->
        acc, def :: rev_defs, List.rev_append item.item_rev_renames renamings
    | Error acc ->
        acc, rev_defs, renamings
  end (acc, [], []) items


let def_n_redef_items item_stack =
  let rec aux redefs = function
    | i :: tl when i.item_redefines <> None -> aux (i :: redefs) tl
    | i :: tl -> i :: redefs, tl      (* pick first non-redefining: it's the def *)
    | tl -> redefs, tl              (* [redefs] should always be empty here *)
  in
  aux [] item_stack


let lower_level_items item_stack =
  snd @@ def_n_redef_items item_stack


let current_item_offset: item_stack -> Cobol_data.Memory.offset = function
  | [] ->
      Cobol_data.Memory.no_offset
  | { item_offset; item_size; _ } :: _ ->
      Cobol_data.Memory.shift item_offset ~by:item_size


let extend_item_def_with_redefs def redefs =
  let item_redefinitions = NEL.to_list redefs in
  let item_loc =
    Option.get @@           (* cannot fail as the list of locs is never empty *)
    Cobol_common.Srcloc.concat_locs @@
    List.map (fun def -> def.item_layout) @@
    def :: item_redefinitions
  in
  { def with
    item_layout = ~&(def.item_layout) &@ item_loc;
    item_redefinitions }


let commit_item acc =
  match def_n_redef_items acc.item_stack with
  | [], _ ->                                                (* stack was empty *)
      acc
  | items, item_stack ->         (* [items] = def, possibly followed by redefs *)
      let acc, rev_defs, renamings =
        item_definitions { acc with item_stack } items in
      match NEL.of_rev_list rev_defs with
      | One def ->
          commit_def def ~renamings acc
      | def :: redefs ->
          commit_def (extend_item_def_with_redefs def redefs) ~renamings acc
      | exception Invalid_argument _ ->      (* error in every item definition *)
          acc


let rec flush_item_stack ?down_to_level acc =
  match acc.item_stack, down_to_level with
  | [], _ -> acc
  | { item_level; _ } :: _, Some level when item_level < level -> acc
  | _ -> flush_item_stack ?down_to_level @@ commit_item acc


let flush_all acc =
  flush_item_stack acc


(* --- data clauses --- *)

let init_clauses =
  {
    occurs = OccursOnce;
    picture = None;
    values = [];
    redefines = None;
    clause_diags = [];
  }

let clause_error (acc: data_clauses) error =
  { acc with clause_diags = Error_diagnostic error :: acc.clause_diags }

let on_occurs_clause acc (c: Cobol_ptree.data_occurs_clause with_loc) =
  match acc.occurs, ~&c with
  | FixedOccurs { loc; _ }, _
  | OccursDepending { loc; _ }, _
  | OccursDynamic { loc; _ }, _ ->
      clause_error acc (Duplicate_clause { clause_name = "OCCURS";
                                           first_loc = loc;
                                           second_loc = ~@c })
  | OccursOnce, OccursFixed { times; _ } ->
      { acc with occurs = FixedOccurs { length = times;
                                        loc = ~@c } }
  | OccursOnce, OccursDepending { from; to_; depending; _ } ->
      { acc with occurs = OccursDepending { min_occurs = from;
                                            max_occurs = to_;
                                            depending;
                                            loc = ~@c } }
  | OccursOnce, OccursDynamic { capacity_in; from; to_; initialized; _ } ->
      { acc with occurs = OccursDynamic { capacity = capacity_in;
                                          min_capacity = from;
                                          max_capacity = to_;
                                          initialized;
                                          loc = ~@c } }

let on_unique_clause ~clause_name ~f prev_val acc clause =
  match prev_val with
  | Some { loc; _ } ->
      clause_error acc (Duplicate_clause { clause_name;
                                           first_loc = loc;
                                           second_loc = ~@clause })
  | None ->
      f acc clause

let on_picture_clause acc =
  on_unique_clause ~clause_name:"PICTURE" acc.picture acc
    ~f:(fun acc clause -> { acc with picture = Some clause })

let on_value_clause acc clause =
  { acc with values = clause :: acc.values }

let on_redefines_clause acc =
  on_unique_clause ~clause_name:"REDEFINES" acc.redefines acc
    ~f:(fun acc clause -> { acc with redefines = Some ~&clause })

let traverse_data_clauses
    (data_clauses: Cobol_ptree.data_clause with_loc list) =
  List.fold_left begin fun acc { payload = clause; loc } ->
    match (clause: Cobol_ptree.data_clause) with
    | DataOccurs    o -> on_occurs_clause acc (o &@ loc)
    | DataRedefines r -> on_redefines_clause acc (r &@ loc)
    | DataPicture   p -> on_picture_clause acc p
    | DataValue     d -> on_value_clause acc (d &@ loc)
    | _ -> acc
  end init_clauses data_clauses

(* --- data items --- *)

(** [qualname data_name item_stack] returns the qualified name of a new item
    with base name [data_name] that would be pushed onto [item_stack]. *)
let qualname
    (data_name: Cobol_ptree.data_name with_loc option)
    (item_stack: item_stack) =
  match data_name, item_stack with
  | Some { payload = DataName n; _ }, { item_qualname = qn; _ } :: _ ->
      Some (qual n qn)
  | Some { payload = DataName n; _ }, [] ->
      Some (name n)
  | _ ->
      None

type item_variant =                                          (* coarse layout *)
  | Simple
  | Table

let item_def_variant: item_definition -> item_variant = fun item_def ->
  match ~&(item_def.item_layout) with
  | Elementary _ -> Simple
  | Struct _ -> Simple
  | FixedTable _ -> Table
  | DependingTable _ -> Table
  | DynamicTable _ -> Table

let on_redefinition_item acc item_clauses ~level ~name ~redefined_name ~loc =
  let acc = flush_item_stack ~down_to_level:(~&level + 1) acc in
  match acc.item_stack with
  | [] ->                                               (* no redefinable item *)
      error acc @@
      Misplaced_redefinition { loc; expl = Following "no definition" }
  | { item_level; item_redefines; item_loc; item_rev_fields; _ } :: _ as items ->
      let acc =
        if item_level = ~&level then acc else
          error acc @@
          Unexpected_redefinition_level { redef_loc = loc;
                                          redef_name = name;
                                          redef_level = level;
                                          expected_level = item_level;
                                          expected_redefined_loc = item_loc }
      in
      let acc, redef_name, redef_aliases = match item_redefines with
        | Some (original_name, aliases)
          when ~&redefined_name <> original_name ->
            let alias_used =
              if StringSet.mem ~&redefined_name aliases
              then Some ~&redefined_name
              else None
            in
            error acc @@
            Unexpected_redefinition_name { redef_loc = loc;
                                           redef_name = name;
                                           redef_redefines = redefined_name;
                                           expected_name = original_name;
                                           alias_used },
            original_name,
            aliases
        | Some (_, aliases) ->
            acc, ~&redefined_name, aliases
        | None ->
            acc, ~&redefined_name, StringSet.empty
      in
      let acc =
        List.fold_left begin fun acc field_def ->
          match field_def.item_length, item_def_variant field_def with
          | Fixed_length, Simple ->
              acc                                                       (* ok *)
          | Variable_length, Simple ->
              error acc @@
              Redefinition_of_ODO_item { redef_loc = loc;
                                         redef_name = name;
                                         redef_redefines = redefined_name;
                                         odo_item = field_def }
          | _, Table ->
              error acc @@
              Redefinition_of_table_item { redef_loc = loc;
                                           redef_name = name;
                                           redef_redefines = redefined_name;
                                           table_item = field_def }
        end acc item_rev_fields
      in
      let base_stack = lower_level_items acc.item_stack in
      let item_qualname = qualname name base_stack in
      let redef_aliases = match item_qualname with
        | None -> redef_aliases
        | Some qn -> StringSet.add (name_of qn) redef_aliases
      in
      { acc with                            (* push on stack, with same level *)
        item_stack = { item_level;
                       item_name = name;
                       item_qualname;
                       item_loc = loc;
                       item_offset = current_item_offset base_stack;
                       item_size = Cobol_data.Memory.point_size;
                       item_redefines = Some (redef_name, redef_aliases);
                       item_clauses;
                       item_rev_fields = [];
                       item_rev_renames = [] } :: items }

let on_item ~at_level { payload = Cobol_ptree.{ data_level; data_name;
                                                data_clauses };
                        loc } acc =
  let item_clauses = traverse_data_clauses data_clauses in
  match item_clauses.redefines with
  | Some redefined_name ->
      on_redefinition_item acc item_clauses
        ~level:data_level ~name:data_name ~redefined_name ~loc
  | None ->
      let acc = flush_item_stack ~down_to_level:at_level acc in
      let acc = match acc.item_stack with
        | { item_rev_renames = _ :: _; _ } :: _ ->
            (* non-01/77 item that follows a RENAMES *)
            let expected = [01; 66; 77; 78; 88] in                 (* CHECKME *)
            flush_all @@                       (* flush full stack to recover *)
            error acc (Unexpected_level_number { level = data_level; expected })
        | _ ->
            acc
      in
      { acc with
        item_stack = { item_level = ~&data_level;
                       item_name = data_name;
                       item_loc = loc;
                       item_qualname = qualname data_name acc.item_stack;
                       item_offset = current_item_offset acc.item_stack;
                       item_size = Cobol_data.Memory.point_size;
                       item_redefines = None;
                       item_clauses;
                       item_rev_fields = [];
                       item_rev_renames = [] } :: acc.item_stack }

let find_in_current_record qualname acc =
  try
    Ok (Cobol_unit.Qualmap.find ~&qualname acc.current_qualmap)
  with Not_found ->
    Pretty.error "current_qualmap: @[%a@]@."
      (Cobol_unit.Qualmap.pp_qualmap (fun _ _ -> ())) acc.current_qualmap;
    Error (error acc @@ Item_not_found { qualname })

let dummy_renamed_elementary =
  RenamedElementary { picture = Cobol_data.Picture.alphanumeric ~size:0 }

let report_occurs acc operand item =
  (* TODO: Check required subscripting to avoid case of subordination to an
     OCCURS. *)
  match item_def_variant item with
  | Simple -> acc
  | Table -> error acc @@ Occurs_in_rename_operand { operand; item }

(* let report_renaming_of_variable_length acc vars ~loc = *)
(*   error acc @@ Invalid_renaming_of_variable_length_range { loc }, *)

let renaming acc ~renamed_qualname
    { payload = Cobol_ptree.{ rename_to = name;
                              rename_renamed = from;
                              rename_through = thru; _ };
      loc } =
  let renaming_name = qual name renamed_qualname in
  let from = requal ~&from renamed_qualname &@<- from in
  let acc, from_item = match find_in_current_record from acc with
    | Ok def -> report_occurs acc from def, Some def
    | Error acc -> acc, None
  in
  let acc, thru_name, thru_item = match thru with
    | None -> acc, None, None
    | Some thru ->
        let thru = requal ~&thru renamed_qualname &@<- thru in
        match find_in_current_record thru acc with
        | Ok def -> report_occurs acc thru def, Some thru, Some def
        | Error acc -> acc, Some thru, None
  in
  match from_item, thru_item with
  | None, _ ->
      Error acc
  | Some from_item, None ->
      (* TODO: check not subordinate to an OCCURS (via subscripting info
         maybe). *)
      let renaming_layout = match ~&(from_item.item_layout) with
        | Elementary { picture; _ } -> RenamedElementary { picture }
        | Struct { fields } -> RenamedStruct { fields }
        | _ -> dummy_renamed_elementary           (* OCCURS (already reported) *)
      in
      Ok (acc, { renaming_name;
                 renaming_offset = from_item.item_offset;
                 renaming_size = from_item.item_size;
                 renaming_layout;
                 renaming_from = from;
                 renaming_thru = None;
                 renaming_loc = loc })
  | Some from_item, Some thru_item ->
      let from_offset = from_item.item_offset
      and thru_offset = thru_item.item_offset
      and thru_size = thru_item.item_size in
      let end_ = Cobol_data.Memory.increase thru_offset ~by:thru_size in
      let size_ = Cobol_data.Memory.size ~from:from_offset ~to_:end_ in
      let acc, renaming_layout =
        try
          let size = Cobol_data.Memory.as_int size_ in
          let picture = Cobol_data.Picture.alphanumeric ~size in
          acc, RenamedElementary { picture }
        with Cobol_data.Memory.NOT_SCALAR (`Vars vars) ->
          let depending_vars =
            NEL.map vars ~f:(fun (Cobol_data.Memory.Valof qn) -> qn) in
          error acc @@
          Invalid_renaming_of_variable_length_range { loc; depending_vars },
          dummy_renamed_elementary
      in
      Ok (acc, { renaming_name;
                 renaming_offset = from_offset;
                 renaming_size = size_;
                 renaming_layout;
                 renaming_from = from;
                 renaming_thru = thru_name;
                 renaming_loc = loc })

let on_rename ({ loc; _ } as rename_item) acc =
  (* Flush everything but any 01 item in stack *)
  let acc = flush_item_stack ~down_to_level:02 acc in
  match acc.item_stack with
  | [] ->
      error acc @@ Misplaced_renaming { loc; expl = Following "no definition" }
  | { item_level; item_qualname; _ } as top_item :: item_stack ->
      let acc = match item_level with
        | 01 ->                                      (* For later: or FD or SD *)
            acc
        | l ->                  (* report misplacement, but keep going anyways *)
            error acc @@ Misplaced_renaming { loc; expl = Following_level l }
      in
      match renaming acc ~renamed_qualname:item_qualname rename_item with
      | Ok (acc, r) ->
          let top_item =
            { top_item with item_rev_renames = r :: top_item.item_rev_renames } in
          { acc with item_stack = top_item :: item_stack }
      | Error acc ->
          acc

let enter_section section acc =
  let acc = flush_all acc in
  { acc with current_storage = section }

let data_definitions = object
  inherit [acc] Cobol_ptree.Visitor.folder

  method! fold_nested_programs _ =
    Visitor.skip

  method! fold_file_section _ acc =
    Visitor.do_children @@ enter_section File acc
  method! fold_working_storage_section _ acc =
    Visitor.do_children @@ enter_section WorkingStorage acc
  method! fold_local_storage_section _ acc =
    Visitor.do_children @@ enter_section LocalStorage acc
  method! fold_linkage_section _ acc =
    Visitor.do_children @@ enter_section Linkage acc

  method! fold_data_item' ({ payload = { data_level; _ };
                             loc } as item) acc =
    Visitor.skip_children @@ match ~&data_level, acc.current_storage with
    | l, _ when 1 <= l && l <= 49 ->
        on_item ~at_level:~&data_level item acc                    (* regular *)
    | 77, (Linkage | LocalStorage | WorkingStorage) ->
        on_item ~at_level:01 item acc            (* non-contiguous elementary *)
    | 77, section ->
        error acc (Item_not_allowed_in_section { level = data_level; section })
    | 78, _ ->
        error acc (Pending_feature { name = "level 78 constant item"; loc })
    | _, _ ->
        error acc (Invalid_level_number { level = data_level })

  method! fold_rename_item' ({ payload = { rename_level; _ }; _ } as item) acc =
    Visitor.skip_children @@ match ~&rename_level with
    | 66 ->
        on_rename item acc
    | _ ->
        error acc (Invalid_level_number { level = rename_level }) |>
        on_rename item                                      (* rename anyways *)

  (* TODO: fold_constant_item' *)
  (* TODO: fold_condition_name_item' *)
  (* TODO: fold_screen_item' *)
  (* TODO: fold_report_group_item' *)
end

let of_compilation_unit config cu' =
  init config |>
  Cobol_ptree.Visitor.fold_compilation_unit' data_definitions cu' |>
  flush_all |>
  result
