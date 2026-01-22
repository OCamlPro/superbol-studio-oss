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

(* [@@@warning "-32"] *)

open Cobol_unit.Types
open Cobol_data.Types
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module Visitor = Cobol_common.Visitor
module Qualmap = Cobol_unit.Qualmap
module NEL = Cobol_common.Basics.NEL
module PIC = Cobol_data.Picture

(* --- *)

type output =
  {
    definitions: Cobol_unit.Types.data_definitions;
    references: Typeck_outputs.qualrefmap;
  }

(* --- *)

type acc =
  {
    current_storage: Cobol_data.Types.data_storage;
    current_qualification: Cobol_ptree.qualname option;
    item_stack: item_stack;
    current_qualmap: Cobol_data.Types.field_definition with_loc Qualmap.t;
    pending_conditions: condition_name_under_construction list;
    filler_count: int;
    definitions: Cobol_unit.Types.data_definitions;
    references: srcloc list Cobol_unit.Qual.MAP.t;
    picture_config: Cobol_data.Types.picture_config;
    diags: Typeck_diagnostics.t;
  }
and item_stack = item_under_construction list
and item_under_construction =               (* item currently being assembled *)
  {
    item_level: int;                                       (* 01 <= _ <= 49 *)
    item_name: Cobol_ptree.data_name with_loc option;      (* given item name *)
    item_loc: srcloc;
    item_qualname: Cobol_ptree.qualname with_loc option;     (* full qualname *)
    item_qualifier: Cobol_ptree.qualname option;  (* qualifier for sub. items *)
    item_redefines: Cobol_ptree.qualname with_loc option;     (* if REDEFINES *)
    item_offset: Cobol_data.Memory.offset;
    item_size: Cobol_data.Memory.size;
    item_clauses: Typeck_clauses.data_clauses;
    item_range: table_range option;
    item_rev_leading_ranges: table_range list;       (* ~> leading subscripts *)
    item_rev_fields: Cobol_data.Types.item_definition with_loc list;
    item_rev_renames: Cobol_data.Types.record_renaming with_loc list;
    item_rev_conditions: Cobol_data.Types.condition_names;
  }

and condition_name_under_construction =
  Cobol_data.Types.condition_name with_loc *
  Cobol_data.Types.field_definition with_loc

(* --- *)

let name = Cobol_unit.Qual.name
let qual = Cobol_unit.Qual.qual
let name_of = Cobol_unit.Qual.name_of
(* let qual_of = Cobol_unit.Qual.qual_of *)
let requal = Cobol_unit.Qual.requal

let init (config: unit_config) =
  {
    current_storage = Local_storage;                         (* dummy default *)
    current_qualification = None;
    current_qualmap = Qualmap.empty;
    item_stack = [];
    pending_conditions = [];
    filler_count = 0;
    definitions =
      {
        data_items = { named = Qualmap.empty; list = [] };
        data_records = [];
      };
    references = Cobol_unit.Qual.MAP.empty;
    picture_config =
      {
        max_pic_length = 100;
        decimal_char = config.unit_decimal_point;
        currency_signs = config.unit_currency_signs;
      };
    diags = [];
  }

let error acc error = { acc with diags = Data_error error :: acc.diags }
let warn acc d = { acc with diags = Data_warning d :: acc.diags }

let result { definitions = { data_items; data_records };
             references; diags; _ } : output * Typeck_diagnostics.t =
  { definitions =
      { data_items = { data_items with list = List.rev data_items.list };
        data_records = List.rev data_records };
    references },
  diags


(* let pp_item_stack ppf acc = *)
(*   Fmt.(hbox @@ any "[" ++ list ~sep:comma pp_data_name'_opt ++ any "]") ppf @@ *)
(*   List.rev_map (fun { item_name; _ } -> item_name) acc.item_stack *)


(* Managing and scanning the stack of items under construction ([item_stack]) *)


let def_n_redef_items (item_stack: item_stack) =
  let rec aux redefs : item_stack -> _ = function
    | i :: tl when i.item_redefines <> None -> aux (i :: redefs) tl
    | i :: tl -> i :: redefs, tl      (* pick first non-redefining: it's the def *)
    | tl -> redefs, tl              (* [redefs] should always be empty here *)
  in
  aux [] item_stack


(** [qualify name item_stack] returns the qualified name of a new item with name
    [name] that would be pushed onto [item_stack]. *)
let qualify: string with_loc -> item_stack -> Cobol_ptree.qualname with_loc =
  fun n -> function
    | { item_qualifier = qn; _ } :: _ -> qual n qn &@<- n
    | [] -> name n &@<- n


(** [qualname data_name item_stack] returns the qualified name of a new item
    with base name [data_name] that would be pushed onto [item_stack]. *)
let qualname
    (data_name: Cobol_ptree.data_name with_loc option)
    (item_stack: item_stack) =
  match data_name, item_stack with
  | Some { payload = DataName n; loc }, { item_qualifier = qn; _ } :: _ ->
      Some (qual n qn &@ loc)
  | Some { payload = DataName n; loc }, [] ->
      Some (name n &@ loc)
  | _ ->
      None


(** [qualifier data_name item_stack] returns the qualification of a new item
    with base name [data_name] that would be pushed onto [item_stack]. *)
let qualifier
    (data_name: Cobol_ptree.data_name with_loc option)
    (item_stack: item_stack) =
  match data_name, item_stack with
  | Some { payload = DataName n; _ }, { item_qualifier = qn; _ } :: _ ->
      Some (qual n qn)
  | Some { payload = DataName n; _ }, [] ->
      Some (name n)
  | _, { item_qualifier = qn; _ } :: _ ->
      qn
  | _ -> None


let current_item_offset: item_stack -> Cobol_data.Memory.offset = function
  | [] ->
      Cobol_data.Memory.no_offset
  | { item_offset; item_size; _ } :: _ ->
      Cobol_data.Memory.shift item_offset ~by:item_size


(* --- *)


let record_name: acc -> Cobol_ptree.qualname with_loc option -> acc * string =
  fun acc -> function
    | None ->
        { acc with filler_count = succ acc.filler_count },
        Pretty.to_string "FILLER %u" (succ acc.filler_count)
    | Some qn ->
        acc, name_of ~&qn


(* --- *)


let commit_record acc ~renamings (record_item: item_definition with_loc) =
  let record_item_name = Cobol_data.Item.qualname ~&record_item in
  let acc, record_name = record_name acc record_item_name in
  let record = { record_name; record_storage = acc.current_storage;
                 record_item; record_renamings = renamings } in
  let add_named_def qn def { named; list } =
    { named = Qualmap.add ~&qn def named;
      list = def :: list }
  and add_anonymous_def def { named; list } =
    { named;
      list = def :: list }
  in
  let data_items =
    Cobol_data.Item.fold_definitions ~fold_redefinitions:true record_item
      acc.definitions.data_items
      ~field:begin fun def -> match ~&def.field_qualname with  (* regular fields *)
        | Some qn ->
            add_named_def qn (Data_field { record; def })
        | None ->
            add_anonymous_def (Data_field { record; def })
      end
      ~table:begin fun table acc ->                             (* table indexes *)
        List.fold_left begin fun acc qualname ->
          add_named_def qualname (Table_index { record; table; qualname }) acc
        end acc ~&table.table_range.range_indexes
      end
  in
  let data_items =                                      (* add renaming items *)
    List.fold_left begin fun data_items def ->
      add_named_def ~&def.renaming_name
        (Data_renaming { record; def }) data_items
    end data_items renamings
  in
  let data_items =                                     (* add condition names *)
    List.fold_left begin fun data_items (def, field) ->
      (* TODO: [validation] entries have no name.  (The current parse-tree does
         not allow those). *)
      add_named_def ~&def.condition_name_qualname
        (Data_condition { record; def; field }) data_items
    end data_items acc.pending_conditions
  in
  { acc with
    current_qualmap = Qualmap.empty;
    pending_conditions = [];
    definitions = { data_items;
                    data_records = record :: acc.definitions.data_records } }


let commit_item_definition ~renamings def acc =
  match acc.item_stack with
  | [] ->
      commit_record acc ~renamings def
  | { item_loc; item_size; _ } as top_item :: item_stack ->
      let field_size = Cobol_data.Item.size ~&def in
      let top_item =
        { top_item with
          item_loc = Cobol_common.Srcloc.concat item_loc ~@def;
          item_size = Cobol_data.Memory.add item_size field_size;
          item_rev_fields = def :: top_item.item_rev_fields } in
      { acc with item_stack = top_item :: item_stack }



let check_inherited_usage acc ~item_name item_clauses item_stack =
  let top_item_usage =
    match item_stack with
    | [] -> None
    | { item_clauses; _ } :: _ -> item_clauses.usage
  in
  match item_clauses.Typeck_clauses.usage, top_item_usage with
  | _, None ->
      acc, item_clauses
  | None, (Some _ as usage) ->
      acc, { item_clauses with usage }
  | Some u, Some p ->
      let acc =         (* check matching usage (according to the standard) *)
        if Cobol_ptree.compare_usage_clause ~&u ~&p <> 0 then
          warn acc @@ Mismatching_usage_in_group { item_name;
                                                   item_usage = u;
                                                   group_usage = p }
        else acc
      in
      acc, item_clauses


let item_range acc ~item_qualifier
    (item_clauses: Typeck_clauses.data_clauses) (item_stack: item_stack) =
  let qualify_as_subordinate n = qual n item_qualifier &@<- n in
  let qualify n = qualify n item_stack in                         (* CHECKME! *)
  let opt = Option.map in
  let int = Cobol_common.Srcloc.map_payload int_of_string in
  let range = match Option.map (~&) item_clauses.occurs with
    | None ->
        None
    | Some OccursFixed { times; indexed_by; _ } ->
        Some { range_span = Fixed_span { occurs_times = int times };
               range_indexes = List.map qualify_as_subordinate indexed_by }
    | Some OccursDepending { from; to_; depending; indexed_by; _ } ->
        Some { range_span = Depending_span { occurs_depending_min = int from;
                                             occurs_depending_max = int to_;
                                             occurs_depending = depending };
               range_indexes = List.map qualify_as_subordinate indexed_by }
    | Some OccursDynamic { capacity_in; from; to_;
                           initialized; indexed_by; _ } ->
        let range_span
          = Dynamic_span { occurs_dynamic_capacity = opt qualify capacity_in;
                           occurs_dynamic_capacity_min = opt int from;
                           occurs_dynamic_capacity_max = opt int to_;
                           occurs_dynamic_initialized = initialized } in
        Some { range_span;
               range_indexes = List.map qualify_as_subordinate indexed_by }
  in
  let leading_ranges = match range, item_stack with
    | Some r, { item_rev_leading_ranges = l; _ } :: _ -> r :: l
    | Some r, [] -> [r]
    | None, { item_rev_leading_ranges = l; _ } :: _ -> l
    | None, [] -> []
  in
  acc, range, leading_ranges


let register_field_def acc (def: field_definition with_loc) =
  match ~&def.field_qualname with
  | Some qn ->
      { acc with
        current_qualmap = Qualmap.add ~&qn def acc.current_qualmap }
  | None -> acc


let error_if_picture ~item_name ~item_loc ~reason acc = function
  | None ->
      acc
  | Some picture ->
      error acc @@ Unexpected_picture_clause { picture; reason;
                                               item_name; item_loc }


let field_usage_n_value acc { item_name; item_loc; item_clauses; _ } =
  let diags, usage, value =
    Typeck_clauses.to_usage_n_value item_clauses ~item_name ~item_loc
      ~picture_config:acc.picture_config
  in
  let acc = { acc with diags = Typeck_diagnostics.union acc.diags diags } in
  acc, usage, value


let field_layout_n_size acc ~usage ~init_value { item_name;
                                                 item_loc;
                                                 item_size;
                                                 item_clauses;
                                                 item_rev_fields; _ } =
  match item_rev_fields, usage with
  | [], Some usage ->
      acc,
      Elementary_field { usage; init_value },
      Typeck_utils.size_of ~usage
  | [], None ->          (* missing usage (reported as missing pic string) *)
      error acc @@
      Missing_picture_clause_for_elementary_item { item_name; item_loc },
      Elementary_field { usage = Display (PIC.alphanumeric ~size:1);
                         init_value = None },
      Cobol_data.Memory.byte_size
  | flds, _ ->
      error_if_picture ~item_name ~item_loc ~reason:`Group_item acc
        item_clauses.picture,
      Struct_field { subfields = NEL.of_rev_list flds },
      item_size                (* accumulated during commits of subfields *)


let item_definition acc ({ item_loc;
                           item_qualname;
                           item_redefines;
                           item_offset;
                           item_range;
                           item_rev_leading_ranges;
                           item_rev_conditions; _ } as item) =
  let acc, usage, init_value = field_usage_n_value acc item in
  let acc, field_layout, field_size =
    field_layout_n_size acc item ~usage ~init_value in
  let field =
    { field_qualname = item_qualname;
      field_redefines = None;
      field_leading_ranges = List.rev item_rev_leading_ranges;
      field_layout;
      field_offset = item_offset;
      field_size;
      field_length_variability = Fixed_length;
      field_conditions = List.rev item_rev_conditions;
      field_redefinitions = [] } &@ item_loc
  in
  match item_range with
  | None ->
      let field
        = { ~&field with field_redefines = item_redefines } &@<- field in
      register_field_def acc field,
      Field ~&field &@<- field
  | Some ({ range_span = Fixed_span { occurs_times }; _ } as range) ->
      let table_size
        = Cobol_data.Memory.mult_int ~&field.field_size ~&occurs_times in
      register_field_def acc field,
      Table { table_field = field;
              table_offset = ~&field.field_offset;
              table_size;
              table_range = range;
              table_init_values = [];
              table_redefines = item_redefines;
              table_redefinitions = [] } &@<- field
  | Some ({ range_span = Depending_span { occurs_depending; _ }; _ } as range) ->
      let dep_size = Cobol_data.Memory.valof ~&occurs_depending in
      let table_size
        = Cobol_data.Memory.repeat ~&field.field_size ~by:dep_size in
      register_field_def acc field,
      Table { table_field = field;
              table_offset = ~&field.field_offset;
              table_size;
              table_range = range;
              table_init_values = [];
              table_redefines = item_redefines;
              table_redefinitions = [] } &@<- field
  | Some ({ range_span = Dynamic_span _; _ } as range) ->
      register_field_def acc field,
      Table { table_field = field;
              table_offset = ~&field.field_offset;
              table_size = Cobol_data.Memory.size_of_dynamic_table;
              table_range = range;
              table_init_values = [];
              table_redefines = item_redefines;
              table_redefinitions = [] } &@<- field


let commit_conditions acc def { item_rev_conditions; _ } =
  match ~&def with
  | Field field ->
      let pending_conditions =
        List.fold_left begin fun pcs cond_name ->
          (cond_name, field &@<- def) :: pcs
        end acc.pending_conditions item_rev_conditions
      in
      { acc with pending_conditions }
  | Table _ ->                                                (* no conditions *)
      acc


let item_definitions acc items =
  List.fold_left begin fun (acc, rev_defs, renamings) item ->
    let acc, def = item_definition acc item in
    commit_conditions acc def item,
    def :: rev_defs,
    List.rev_append item.item_rev_renames renamings
  end (acc, [], []) items


let extend_item_def_with_redefs def redefs =
  let item_redefinitions = NEL.to_list redefs in
  let item_loc =
    Option.get @@           (* cannot fail as the list of locs is never empty *)
    Cobol_common.Srcloc.concat_locs @@ def :: item_redefinitions
  in
  match ~&def with
  | Field field ->
      Field { field with
              field_redefinitions = item_redefinitions } &@ item_loc
  | Table table ->
      Table { table with
              table_redefinitions = item_redefinitions } &@ item_loc


(* --- data items --- *)


let commit_item acc =
  match def_n_redef_items acc.item_stack with
  | [], _ ->                                                (* stack was empty *)
      acc
  | items, item_stack ->         (* [items] = def, possibly followed by redefs *)
      let acc, rev_defs, renamings =
        item_definitions { acc with item_stack } items in
      match NEL.of_rev_list rev_defs with
      | One def ->
          commit_item_definition def ~renamings acc
      | def :: redefs ->
          commit_item_definition (extend_item_def_with_redefs def redefs) acc
            ~renamings
      | exception Invalid_argument _ ->      (* error in every item definition *)
          acc


let rec flush_item_stack ?down_to_level acc =
  match acc.item_stack, down_to_level with
  | [], _ -> acc
  | { item_level; _ } :: _, Some level when item_level < level -> acc
  | _ -> flush_item_stack ?down_to_level @@ commit_item acc


let register_ref ~from:{ loc; _ } ~to_:qualname_opt acc =
  match qualname_opt with
  | None ->
      acc
  | Some qn ->
      { acc with
        references = Typeck_outputs.register_qualref ~&qn ~loc acc.references }


let find_in_current_record qualname acc =
  try
    let res = Qualmap.find ~&qualname acc.current_qualmap in
    Ok (register_ref ~from:qualname ~to_:~&res.field_qualname acc, res)
  with Not_found ->
    Error (error acc @@ Item_not_found { qualname })


(* --- *)


let on_redefinition_item acc item_clauses
    ~level ~item_name ~redefined_name ~loc
  =
  let acc = flush_item_stack ~down_to_level:(~&level + 1) acc in
  match def_n_redef_items acc.item_stack with
  | [], _ ->                                            (* no redefinable item *)
      error acc @@ Misplaced { entry = Redefines_entry; loc;
                               expl = Following "no definition" }
  | { item_level = expected_level;
      item_loc = expected_redefined_loc; _ } as redefined_item :: _, base_stack ->
      let redefined_qualname = redefined_item.item_qualname in
      let acc =
        if ~&level = expected_level then acc else
          error acc @@
          Unexpected_redefinition_level { redef_loc = loc;
                                          redef_name = item_name;
                                          redef_level = level;
                                          expected_level;
                                          expected_redefined_loc }
      in
      let acc = match redefined_qualname with
        | Some qn
          when Cobol_ptree.compare_name ~&redefined_name (name_of ~&qn) <> 0 ->
            error acc @@
            Unexpected_redefinition_name { redef_loc = loc;
                                           redef_name = item_name;
                                           redef_redefines = redefined_name;
                                           expected_name = name_of ~&qn }
        | _ ->
            acc
      in
      let acc =
        register_ref ~from:redefined_name ~to_:redefined_qualname acc
      in
      let acc =
        match redefined_item.item_range with
        | None ->                                                        (* ok *)
            acc
        | Some _ ->
            warn acc @@
            Redefinition_of_table_item { redef_loc = loc;
                                         redef_name = item_name;
                                         redef_redefines = redefined_name;
                                         table_item_name = redefined_qualname }
      in
      let acc =
        List.fold_left begin fun acc field_def ->
          match ~&field_def with
          | Field { field_length_variability = Variable_length; _ }
          | Table { table_range = { range_span = Depending_span _; _ }; _ }
          | Table { table_field =
                      { payload = { field_length_variability = Variable_length;
                                    _ }; _ }; _ } ->
              error acc @@
              Redefinition_of_ODO_item { redef_loc = loc;
                                         redef_name = item_name;
                                         redef_redefines = redefined_name;
                                         odo_item = field_def }
          | Field { field_length_variability = Fixed_length; _ }
          | Table _ ->
              acc                                                       (* ok *)
        end acc redefined_item.item_rev_fields
      in
      let acc, item_clauses =
        check_inherited_usage acc ~item_name item_clauses base_stack
      and item_qualname = qualname item_name base_stack
      and item_qualifier = qualifier item_name base_stack
      and item_redefines = Some (qualify redefined_name base_stack) in
      let acc, item_range, item_rev_leading_ranges =
        item_range acc ~item_qualifier item_clauses base_stack in
      { acc with                            (* push on stack, with same level *)
        item_stack = { item_level = expected_level;
                       item_name;
                       item_loc = loc;
                       item_qualname;
                       item_qualifier;
                       item_redefines;
                       item_offset = current_item_offset base_stack;
                       item_size = Cobol_data.Memory.point_size;
                       item_clauses;
                       item_range;
                       item_rev_leading_ranges;
                       item_rev_fields = [];
                       item_rev_renames = [];
                       item_rev_conditions = [] } :: acc.item_stack }


let on_item acc ~at_level
    { payload = Cobol_ptree.{ data_level;
                              data_name = item_name;
                              data_clauses }; loc }
  =
  let item_clauses = Typeck_clauses.of_data_item data_clauses in
  let acc =
    { acc with
      diags = Typeck_diagnostics.union acc.diags item_clauses.clause_diags } in
  match item_clauses.redefines with
  | Some redefined_name ->
      on_redefinition_item acc item_clauses
        ~level:data_level ~item_name ~redefined_name ~loc
  | None ->
      let acc = flush_item_stack ~down_to_level:at_level acc in
      let base_stack = acc.item_stack in
      let acc = match base_stack with
        | { item_rev_renames = _ :: _; _ } :: _ ->
            (* non-01/77 item that follows a RENAMES *)
            let expected = [01; 66; 77; 78; 88] in                 (* CHECKME *)
            flush_item_stack @@                (* flush full stack to recover *)
            error acc (Unexpected_level_number { level = data_level; expected })
        | _ ->
            acc
      in
      let acc, item_clauses =
        check_inherited_usage ~item_name acc item_clauses base_stack
      and item_qualname = qualname item_name base_stack
      and item_qualifier = qualifier item_name base_stack in
      let acc, item_range, item_rev_leading_ranges =
        item_range acc ~item_qualifier item_clauses base_stack in
      { acc with
        item_stack = { item_level = ~&data_level;
                       item_name;
                       item_loc = loc;
                       item_qualname;
                       item_qualifier;
                       item_redefines = None;
                       item_offset = current_item_offset acc.item_stack;
                       item_size = Cobol_data.Memory.point_size;
                       item_clauses;
                       item_range;
                       item_rev_leading_ranges;
                       item_rev_fields = [];
                       item_rev_renames = [];
                       item_rev_conditions = [] } :: base_stack }


let dummy_renamed_elementary =
  Renamed_elementary { usage = Display (PIC.alphanumeric ~size:0) }


let report_occurs acc operand field =
  (* Check required subscripting to avoid case of OCCURS or subordination to an
     OCCURS. *)
  if ~&field.field_leading_ranges = []
  then acc
  else error acc @@ Occurs_in_rename_operand { operand; field }


let renaming acc
    ?(renaming_qualifier: Cobol_ptree.qualname option)
    { payload = Cobol_ptree.{ rename_to = name;
                              rename_from = from;
                              rename_thru = thru; _ };
      loc } =
  let renaming_name = qual name renaming_qualifier &@<- name in
  let from = requal ~&from renaming_qualifier &@<- from in
  let acc, from_item = match find_in_current_record from acc with
    | Ok (acc, def) ->
        report_occurs acc from def, Some def
    | Error acc ->
        acc, None
  in
  let acc, thru_item_n_name = match thru with
    | None ->
        acc, None
    | Some thru ->
        let thru = requal ~&thru renaming_qualifier &@<- thru in
        match find_in_current_record thru acc with
        | Ok (acc, def) ->
            report_occurs acc thru def, Some (def, thru)
        | Error acc ->
            acc, None
  in
  match from_item, thru_item_n_name with
  | None, _ ->
      Error acc
  | Some from_field, None ->
      let renaming_layout = match ~&from_field.field_layout with
        | Elementary_field { usage; _ } -> Renamed_elementary { usage }
        | Struct_field { subfields } -> Renamed_struct { subfields }
      in
      Ok (acc, { renaming_name;
                 renaming_layout;
                 renaming_offset = ~&from_field.field_offset;
                 renaming_size = ~&from_field.field_size;
                 renaming_from = from;
                 renaming_thru = None } &@ loc)
  | Some from_field, Some (thru_field, thru_name) ->
      let from_offset = ~&from_field.field_offset
      and thru_offset = ~&thru_field.field_offset
      and thru_size = ~&thru_field.field_size in
      let end_ = Cobol_data.Memory.shift thru_offset ~by:thru_size in
      let size_ = Cobol_data.Memory.size ~from:from_offset ~to_:end_ in
      let acc, renaming_layout =
        try
          let bits = Cobol_data.Memory.as_bits size_ in
          let size = bits / 8 in
          if size > 0 && bits mod 8 = 0 then
            acc, Renamed_elementary { usage = Display (PIC.alphanumeric ~size) }
          else if size > 0 then
            error acc @@ Invalid_renaming_size { loc; from_field; thru_field;
                                                 bits },
            Renamed_elementary { usage = Display (PIC.alphanumeric ~size) }
          else
            error acc @@ Invalid_renaming_range { loc; from_field; thru_field },
            dummy_renamed_elementary
        with Cobol_data.Memory.NOT_SCALAR (`Vars vars) ->
          let depending_vars =
            NEL.map vars ~f:(fun (Cobol_data.Memory.Valof qn) -> qn) in
          error acc @@
          Invalid_renaming_of_variable_length_range { loc; depending_vars },
          dummy_renamed_elementary
      in
      Ok (acc, { renaming_name;
                 renaming_layout;
                 renaming_offset = from_offset;
                 renaming_size = size_;
                 renaming_from = from;
                 renaming_thru = Some thru_name } &@ loc)


let on_rename ({ loc; _ } as rename_item) acc =
  (* Flush everything but any 01 item in stack *)
  let acc = flush_item_stack ~down_to_level:02 acc in
  match acc.item_stack with
  | [] ->
      error acc @@ Misplaced { entry = Renames_entry; loc;
                               expl = Following "no definition" }
  | { item_level; item_qualifier; _ } as top_item :: item_stack ->
      let acc = match item_level with
        | 01 ->                                      (* For later: or FD or SD *)
            acc
        | l ->                  (* report misplacement, but keep going anyways *)
            error acc @@ Misplaced { entry = Renames_entry; loc;
                                     expl = Following_level l }
      in
      match renaming acc ?renaming_qualifier:item_qualifier rename_item with
      | Ok (acc, renaming) ->
          let item_rev_renames = renaming :: top_item.item_rev_renames in
          { acc with
            item_stack = { top_item with item_rev_renames } :: item_stack }
      | Error acc ->
          acc


let on_condition_name (cond_name: Cobol_ptree.condition_name_item with_loc) acc =
  match acc.item_stack with
  | [] ->
      error acc @@ Misplaced { entry = Condition_name_entry;
                               loc = ~@cond_name;
                               expl = Following "no definition" }
  | top_item :: item_stack ->
      let condition_name_qualname
        = qualify ~&cond_name.condition_name acc.item_stack in
      let condition_name
        = { condition_name_qualname;
            condition_name_item = ~&cond_name } &@<- cond_name in
      let top_item =
        { top_item with
          item_rev_conditions = condition_name :: top_item.item_rev_conditions }
      in
      { acc with item_stack = top_item :: item_stack }


(* --- *)


let enter_section section acc =
  let acc = flush_item_stack acc in
  { acc with current_storage = section }

let data_definitions = object
  inherit [acc] Cobol_ptree.Visitor.folder

  method! fold_nested_programs _ =
    Visitor.skip

  method! fold_file_descr { file_name; _ } acc =
    Visitor.do_children @@ enter_section (File file_name) acc
  method! fold_working_storage_section _ acc =
    Visitor.do_children @@ enter_section Working_storage acc
  method! fold_local_storage_section _ acc =
    Visitor.do_children @@ enter_section Local_storage acc
  method! fold_linkage_section _ acc =
    Visitor.do_children @@ enter_section Linkage acc

  (* TODO *)
  method! fold_communication_section _  = Visitor.skip
  method! fold_report_section _  = Visitor.skip
  method! fold_screen_section _  = Visitor.skip

  method! fold_data_item' ({ payload = { data_level; _ };
                             loc } as item) acc =
    Visitor.skip_children @@ match ~&data_level, acc.current_storage with
    | l, _ when 1 <= l && l <= 49 ->
        on_item ~at_level:~&data_level acc item                    (* regular *)
    | 77, (Linkage | Local_storage | Working_storage) ->
        on_item ~at_level:01 acc item            (* non-contiguous elementary *)
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

  (* TODO: fold_constant_item';
     see https://github.com/OCamlPro/superbol-studio-oss/issues/53 *)

  method! fold_condition_name_item' ({ payload = { condition_name_level;
                                                   _ }; _ } as item) acc =
    Visitor.skip_children @@ match ~&condition_name_level with (* check in case *)
    | 88 ->
        on_condition_name item acc
    | _ ->
        error acc (Invalid_level_number { level = condition_name_level }) |>
        on_condition_name item                                (* emit anyways *)

  (* TODO: fold_screen_item' *)
  (* TODO: fold_report_group_item' *)
end

let of_compilation_unit config cu' =
  init config |>
  Cobol_ptree.Visitor.fold_compilation_unit' data_definitions cu' |>
  flush_item_stack |>
  result
