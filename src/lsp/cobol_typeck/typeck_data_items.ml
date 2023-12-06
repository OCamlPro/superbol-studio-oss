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

open Cobol_unit.Types
open Cobol_data.Types
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module Visitor = Cobol_common.Visitor
module Qualmap = Cobol_unit.Qualmap
module NEL = Cobol_common.Basics.NEL

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
    current_qualmap: Cobol_data.Types.item_definition with_loc Qualmap.t;
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
    item_level: int;                                         (* 01 <= _ <= 49 *)
    item_name: Cobol_ptree.data_name with_loc option;
    item_loc: srcloc;
    item_qualname: Cobol_ptree.qualname with_loc option;    (* full qual name *)
    item_qualifier: Cobol_ptree.qualname option;
    item_offset: Cobol_data.Memory.offset;
    item_size: Cobol_data.Memory.size;                   (* if non-elementary *)
    item_clauses: Typeck_clauses.data_clauses;
    item_redefines: Cobol_ptree.qualname with_loc option; (* REDEFINES iff <> None *)
    item_rev_fields: Cobol_data.Types.item_definition with_loc list;
    item_rev_renames: Cobol_data.Types.record_renaming with_loc list;
    item_rev_conditions: Cobol_data.Types.condition_names;
    (* TODO: subscripting info? + others? *)
  }
and condition_name_under_construction =
  Cobol_data.Types.condition_name with_loc *
  Cobol_data.Types.item_definition with_loc

(* --- *)

let name = Cobol_unit.Qual.name
let qual = Cobol_unit.Qual.qual
let name_of = Cobol_unit.Qual.name_of
(* let qual_of = Cobol_unit.Qual.qual_of *)
let requal = Cobol_unit.Qual.requal

let init (config: unit_config) =
  {
    current_storage = File;
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


(* --- *)

let record_name: acc -> Cobol_ptree.qualname with_loc option -> acc * string =
  fun acc -> function
    | None ->
        { acc with filler_count = succ acc.filler_count },
        Pretty.to_string "FILLER %u" (succ acc.filler_count)
    | Some qn ->
        acc, name_of ~&qn

(* --- *)


let define_record acc ~renamings (record_item: item_definition with_loc) =
  let acc, record_name = record_name acc ~&record_item.item_qualname in
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
    Cobol_data.Item.fold_definitions record_item acc.definitions.data_items
      ~fold_redefinitions:true
      ~f:begin function
        | { payload = { item_qualname = Some qn; _ }; _ } as def ->
            add_named_def qn (Data_item { record; def })
        | { payload = { item_qualname = None; _ }; _ } as def ->
            add_anonymous_def (Data_item { record; def })
      end
  in
  let data_items =                                      (* add renaming items *)
    List.fold_left begin fun data_items def ->
      add_named_def ~&def.renaming_name
        (Data_renaming { record; def }) data_items
    end data_items renamings
  in
  let data_items =                                     (* add condition names *)
    List.fold_left begin fun data_items (def, item) ->
      (* TODO: [validation] entries have no name.  (The current parse-tree does
         not allow those). *)
      add_named_def ~&def.condition_name_qualname
        (Data_condition { record; def; item }) data_items
    end data_items acc.pending_conditions
  in
  { acc with
    current_qualmap = Qualmap.empty;
    pending_conditions = [];
    definitions = { data_items;
                    data_records = record :: acc.definitions.data_records } }


let commit_def ~renamings def acc =
  match acc.item_stack with
  | [] ->
      define_record acc ~renamings def
  | { item_loc; item_size; _ } as top_item :: item_stack ->
      let field_size = ~&def.item_size in
      let top_item =
        { top_item with
          item_loc = Cobol_common.Srcloc.concat item_loc ~@def;
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


let elementary_usage_n_value acc
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
       | Ok (acc, picture) -> Ok (acc, Usage picture, None))
  | None, Some _ ->
      (match dummy_picture_clause acc "X" with             (* XXX: temporary! *)
       | Error acc -> Error acc
       | Ok (acc, picture) -> Ok (acc, Usage picture, value))
  | None, _ ->
      let acc =
        error acc (Missing_picture_clause_for_elementary_item { item_name;
                                                                item_loc })
      in
      (match dummy_picture_clause acc "X" with
       | Error acc -> Error acc
       | Ok (acc, picture) -> Ok (acc, Usage picture, None))


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


let register_def acc (def: item_definition with_loc) =
  match ~&def.item_qualname with
  | Some qn ->
      { acc with
        current_qualmap = Qualmap.add ~&qn def acc.current_qualmap }
  | None -> acc


let item_definition acc
    ({ item_name;
       item_loc;
       item_qualname;
       item_qualifier;
       item_redefines;
       item_offset;
       item_size;
       item_clauses = { occurs; picture; values; clause_diags; _ };
       item_rev_fields;
       item_rev_conditions; _ } as item)
  : (acc * Cobol_data.Types.item_definition with_loc, acc) result =
  let open Cobol_data.Types in
  let acc =
    { acc with diags = Typeck_diagnostics.union acc.diags clause_diags } in
  let item_conditions = List.rev item_rev_conditions in
  let item_definitions_length item_definitions =
    NEL.fold_left Fixed_length item_definitions ~f:begin fun l item_def ->
      if ~&item_def.item_length = Variable_length
      then Variable_length
      else l
    end
  in
  let qualify n = qual n item_qualifier &@<- n in
  let opt = Option.map in
  let int = Cobol_common.Srcloc.locmap int_of_string in
  let elementary acc ({ loc; payload = item } as item') =
    match occurs with
    | OccursOnce ->
        let item' = { ~&item' with item_redefines } &@<- item' in
        register_def acc item', item'
    | FixedOccurs { length; loc = _ } ->
        let length = int length in
        register_def acc item', {
          item_qualname = None;                            (* implicit FILLER *)
          item_redefines;
          item_layout = Fixed_table { items = NEL.One item';
                                      length;
                                      init_values = [] };
          item_offset = item.item_offset;
          item_size = Cobol_data.Memory.mult_int item.item_size ~&length;
          item_length = Fixed_length;
          item_redefinitions = [];
          item_conditions = [];
        } &@ loc
    | OccursDepending { min_occurs; max_occurs; depending; loc = _ } ->
        let dep_size = Cobol_data.Memory.valof ~&depending in
        register_def acc item', {
          item_qualname = None;                            (* implicit FILLER *)
          item_redefines;
          item_layout = Depending_table { items = NEL.One item';
                                          min_occurs = int min_occurs;
                                          max_occurs = int max_occurs;
                                          depending;
                                          init_values = [] };
          item_offset = item.item_offset;
          item_size = Cobol_data.Memory.repeat item.item_size ~by:dep_size;
          item_length = Variable_length;
          item_redefinitions = [];
          item_conditions = [];
        } &@ loc
    | OccursDynamic { capacity; min_capacity; max_capacity;
                      initialized; loc = _ } ->
        register_def acc item', {
          item_qualname = None;                            (* implicit FILLER *)
          item_redefines;
          item_layout = Dynamic_table { items = NEL.One item';
                                        capacity = opt qualify capacity;
                                        min_capacity = opt int min_capacity;
                                        max_capacity = opt int max_capacity;
                                        initialized;
                                        init_values = [] };
          item_offset = item.item_offset;
          item_size = Cobol_data.Memory.size_of_dynamic_table;
          item_length = Fixed_length;
          item_redefinitions = [];
          item_conditions = [];
        } &@ loc
  in
  let group items =
    let item_layout, item_length =
      match occurs with
      | OccursOnce ->
          Struct_item { fields = items },
          item_definitions_length items
      | FixedOccurs { length; loc = _ } ->
          Fixed_table { items;
                        length = int length;
                        init_values = [] },
          item_definitions_length items
      | OccursDepending { min_occurs; max_occurs; depending; loc = _ } ->
          Depending_table { items;
                            min_occurs = int min_occurs;
                            max_occurs = int max_occurs;
                            depending;
                            init_values = [] },
          Variable_length
      | OccursDynamic { capacity; min_capacity; max_capacity;
                        initialized; loc = _ } ->
          Dynamic_table { items;
                          capacity = opt qualify capacity;
                          min_capacity = opt int min_capacity;
                          max_capacity = opt int max_capacity;
                          initialized;
                          init_values = [] },
          Fixed_length
    in
    {
      item_qualname;
      item_redefines;
      item_layout = item_layout;
      item_offset = ~&(NEL.hd items).item_offset;
      item_size;
      item_length;
      item_redefinitions = [];
      item_conditions;
    } &@ item_loc
  in
  if item_rev_fields = [] then         (* elementary (or table of elementary) *)
    match elementary_usage_n_value acc item with
    | Ok (acc, (Usage picture as usage), init_value) ->
        let data_size = Cobol_data.Picture.data_size picture.category in
        Ok (elementary acc
              ({ item_qualname;
                 item_redefines = None;
                 item_layout = Elementary_item { usage; init_value };
                 item_offset;
                 item_size = Cobol_data.Memory.const_size data_size;
                 item_length = Fixed_length;
                 item_redefinitions = [];
                 item_conditions } &@ item_loc))
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


let commit_conditions acc def { item_rev_conditions; _ } =
  let pending_conditions =
    List.fold_left begin fun pcs cond_name ->
      (cond_name, def) :: pcs
    end acc.pending_conditions item_rev_conditions
  in
  { acc with pending_conditions }


let item_definitions acc items =
  List.fold_left begin fun (acc, rev_defs, renamings) item ->
    match item_definition acc item with
    | Ok (acc, def) ->
        commit_conditions acc def item,
        def :: rev_defs,
        List.rev_append item.item_rev_renames renamings
    | Error acc ->
        acc, rev_defs, renamings
  end (acc, [], []) items


let extend_item_def_with_redefs def redefs =
  let item_redefinitions = NEL.to_list redefs in
  let item_loc =
    Option.get @@           (* cannot fail as the list of locs is never empty *)
    Cobol_common.Srcloc.concat_locs @@ def :: item_redefinitions
  in
  { ~&def with
    item_layout = ~&def.item_layout;
    item_redefinitions } &@ item_loc


let def_n_redef_items (item_stack: item_stack) =
  let rec aux redefs : item_stack -> _ = function
    | i :: tl when i.item_redefines <> None -> aux (i :: redefs) tl
    | i :: tl -> i :: redefs, tl      (* pick first non-redefining: it's the def *)
    | tl -> redefs, tl              (* [redefs] should always be empty here *)
  in
  aux [] item_stack


(* --- data items --- *)


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
    Ok (register_ref ~from:qualname ~to_:~&res.item_qualname acc, res)
  with Not_found ->
    Error (error acc @@ Item_not_found { qualname })


(* TODO: just rely on dimensions *)
type item_variant =                                          (* coarse layout *)
  | Simple
  | Table

let item_def_variant: item_definition with_loc -> item_variant = fun item_def ->
  match ~&item_def.item_layout with
  | Elementary_item _ -> Simple
  | Struct_item _ -> Simple
  | Fixed_table _ -> Table
  | Depending_table _ -> Table
  | Dynamic_table _ -> Table


(* --- *)


let on_redefinition_item acc item_clauses ~level ~name ~redefined_name ~loc =
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
                                          redef_name = name;
                                          redef_level = level;
                                          expected_level;
                                          expected_redefined_loc }
      in
      let acc = match redefined_qualname with
        | Some qn
          when ~&redefined_name <> name_of ~&qn ->
            error acc @@
            Unexpected_redefinition_name { redef_loc = loc;
                                           redef_name = name;
                                           redef_redefines = redefined_name;
                                           expected_name = name_of ~&qn }
        | _ ->
            acc
      in
      let acc =
        register_ref ~from:redefined_name ~to_:redefined_qualname acc
      in
      let acc =
        match redefined_item.item_clauses.occurs with
        | OccursOnce ->                                                  (* ok *)
            acc
        | _ ->
            warn acc @@
            Redefinition_of_table_item { redef_loc = loc;
                                         redef_name = name;
                                         redef_redefines = redefined_name;
                                         table_item_name = redefined_qualname }
      in
      let acc =
        List.fold_left begin fun acc field_def ->
          match ~&field_def.item_length with
          | Fixed_length ->
              acc                                                       (* ok *)
          | Variable_length ->
              error acc @@
              Redefinition_of_ODO_item { redef_loc = loc;
                                         redef_name = name;
                                         redef_redefines = redefined_name;
                                         odo_item = field_def }
        end acc redefined_item.item_rev_fields
      in
      let item_redefines = Some (qualify redefined_name base_stack) in
      { acc with                            (* push on stack, with same level *)
        item_stack = { item_level = expected_level;
                       item_name = name;
                       item_qualname = qualname name base_stack;
                       item_qualifier = qualifier name base_stack;
                       item_loc = loc;
                       item_offset = current_item_offset base_stack;
                       item_size = Cobol_data.Memory.point_size;
                       item_redefines;
                       item_clauses;
                       item_rev_fields = [];
                       item_rev_renames = [];
                       item_rev_conditions = [] } :: acc.item_stack }


let on_item ~at_level { payload = Cobol_ptree.{ data_level; data_name;
                                                data_clauses };
                        loc } acc =
  let item_clauses = Typeck_clauses.of_data_item data_clauses in
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
            flush_item_stack @@                (* flush full stack to recover *)
            error acc (Unexpected_level_number { level = data_level; expected })
        | _ ->
            acc
      in
      let item_qualname = qualname data_name acc.item_stack
      and item_qualifier = qualifier data_name acc.item_stack in
      { acc with
        item_stack = { item_level = ~&data_level;
                       item_name = data_name;
                       item_loc = loc;
                       item_qualname;
                       item_qualifier;
                       item_offset = current_item_offset acc.item_stack;
                       item_size = Cobol_data.Memory.point_size;
                       item_redefines = None;
                       item_clauses;
                       item_rev_fields = [];
                       item_rev_renames = [];
                       item_rev_conditions = [] } :: acc.item_stack }


let dummy_renamed_elementary =
  let picture = Cobol_data.Picture.alphanumeric ~size:0 in
  Renamed_elementary { usage = Usage picture }


let report_occurs acc operand item =
  (* TODO: Check required subscripting to avoid case of subordination to an
     OCCURS. *)
  match item_def_variant item with
  | Simple -> acc
  | Table -> error acc @@ Occurs_in_rename_operand { operand; item }


let renaming acc
    ?(renaming_qualifier: Cobol_ptree.qualname option)
    { payload = Cobol_ptree.{ rename_to = name;
                              rename_from = from;
                              rename_thru = thru; _ };
      loc } =
  let renaming_name = qual name renaming_qualifier &@<- name in
  let from = requal ~&from renaming_qualifier &@<- from in
  let acc, from_item = match find_in_current_record from acc with
    | Ok (acc, def) -> report_occurs acc from def, Some def
    | Error acc -> acc, None
  in
  let acc, thru_name, thru_item = match thru with
    | None -> acc, None, None
    | Some thru ->
        let thru = requal ~&thru renaming_qualifier &@<- thru in
        match find_in_current_record thru acc with
        | Ok (acc, def) -> report_occurs acc thru def, Some thru, Some def
        | Error acc -> acc, Some thru, None
  in
  match from_item, thru_item with
  | None, _ ->
      Error acc
  | Some from_item, None ->
      (* TODO: check not subordinate to an OCCURS (via subscripting info
         maybe). *)
      let renaming_layout = match ~&from_item.item_layout with
        | Elementary_item { usage; _ } -> Renamed_elementary { usage }
        | Struct_item { fields } -> Renamed_struct { fields }
        | _ -> dummy_renamed_elementary           (* OCCURS (already reported) *)
      in
      Ok (acc, { renaming_name;
                 renaming_layout;
                 renaming_offset = ~&from_item.item_offset;
                 renaming_size = ~&from_item.item_size;
                 renaming_from = from;
                 renaming_thru = None } &@ loc)
  | Some from_item, Some thru_item ->
      let from_offset = ~&from_item.item_offset
      and thru_offset = ~&thru_item.item_offset
      and thru_size = ~&thru_item.item_size in
      let end_ = Cobol_data.Memory.increase thru_offset ~by:thru_size in
      let size_ = Cobol_data.Memory.size ~from:from_offset ~to_:end_ in
      let acc, renaming_layout =
        try
          let size = Cobol_data.Memory.as_int size_ in
          let picture = Cobol_data.Picture.alphanumeric ~size in
          acc, Renamed_elementary { usage = Usage picture }
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
                 renaming_thru = thru_name } &@ loc)

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

  method! fold_file_section _ acc =
    Visitor.do_children @@ enter_section File acc
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
        on_item ~at_level:~&data_level item acc                    (* regular *)
    | 77, (Linkage | Local_storage | Working_storage) ->
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
