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

open Cobol_common                                          (* Srcloc, Visitor *)
open Cobol_common.Srcloc.INFIX
open Cobol_data.Types
open Lsp_imports

module TYPES = struct

  (** Type definitions to help representing and querying (partially) parsed
      COBOL programs. *)

  open Cobol_common.Srcloc                          (* with_loc & pp_with_loc *)

  (** Information returned by {!val:Lookup.names_at_position}. *)
  type names_at_position =
    {
      qualname_at_position: Cobol_ptree.qualname option;
      enclosing_compilation_unit_name: string option;
    }

  type name_definition =
    {
      as_paragraph: paragraph_definition option;   (* link to AST's paragraph *)
      as_item: item_definition option;
    }                                                          [@@deriving show]
  and paragraph_definition =
    Cobol_ptree.paragraph with_loc                             [@@deriving show]
  and item_definition =
    {
      item_definition: Cobol_ptree.any_item_descr with_loc;
      item_redefinitions: Cobol_ptree.name with_loc list;
    }                                                          [@@deriving show]

  type name_definitions_in_compilation_unit =
    name_definition Cobol_data.Qualmap.t                       [@@deriving show]

  type name_references_in_compilation_unit =
    Srcloc.srcloc list Cobol_data.Qualmap.t                    [@@deriving show]

  type copy_operation = string with_loc            (** with loc of [COPY ...] *)

end
open TYPES

(* --- *)

let bare name : Cobol_ptree.qualname = Cobol_ptree.Name name
let qual name : Cobol_ptree.qualname option -> Cobol_ptree.qualname = function
  | None -> Cobol_ptree.Name name
  | Some qn -> Cobol_ptree.Qual (name, qn)
let simple_name : Cobol_ptree.qualname -> string = function
  | Qual (n, _) | Name n -> ~&n
let baseloc_of_qualname: Cobol_ptree.qualname -> srcloc = function
  | Name name
  | Qual (name, _) -> ~@name

(* TODO: manipulate positions that are combined with a filename instead of
   passing `filename` around all the time. *)

(** [lexloc_of_qualname_in ~filename qualname] returns the full location of
    [qualname], from the first qualifier to the last.  This function is
    temporary and is expected to be replaced once a better way of getting this
    location is found. *)
let lexloc_of_qualname_in ~filename (qn: Cobol_ptree.qualname) =
  let rec end_pos: Cobol_ptree.qualname -> Lexing.position = function
    | Name n -> Cobol_common.Srcloc.end_pos_in ~filename ~@n
    | Qual (_, qn) -> end_pos qn
  in
  match qn with
  | Name n -> Cobol_common.Srcloc.lexloc_in ~filename ~@n
  | Qual (n, qn) -> Cobol_common.Srcloc.start_pos_in ~filename ~@n, end_pos qn

(** [qualname_at_pos ~filename qualname pos] returns the qualname built from all
    the qualifiers of [qualname] that are after or at position [pos] ion
    [filename].  This function is temporary and is expected to be replaced once
    a better way of finding the qualname is implemented. *)
let rec qualname_at_pos ~filename (qn: Cobol_ptree.qualname) pos =
  match qn with
  | Name _ ->
      qn
  | Qual (name, qn') ->
      let lexloc = lexloc_of_qualname_in ~filename qn in
      if not (Lsp_position.is_after_lexloc pos lexloc) &&
         not (Lsp_position.is_in_srcloc ~filename pos ~@name)
      then qualname_at_pos ~filename qn' pos
      else qn

let name_of_compunit (cu: Cobol_ptree.compilation_unit with_loc) =
  match ~&cu with
  | Program {program_name = name; _}
  | Function {function_name = name; _}
  | ClassDefinition {class_name = name; _}
  | InterfaceDefinition {interface_name = name; _} -> ~&name

let compilation_unit_by_name (cu_name: Cobol_ptree.name)
    (ptree: Cobol_ptree.compilation_group) =
  List.find_opt (fun cu -> cu_name = name_of_compunit cu)
    ptree.compilation_units

(* --- *)

(** [names_at_position pos ptree] seeks the compilation unit name and qualified
    name at the given position [pos], in compilation group parse-tree
    [ptree]. *)
let names_at_position ~uri pos ptree : names_at_position =
  let filename = Lsp.Uri.to_path uri in
  let open struct

    type acc =
      {
        names: names_at_position;
        qualifiers: (Cobol_ptree.qualname * int) list;      (* qualifiers stack *)
        qualifiers_for_redef: (Cobol_ptree.qualname * int) list;(* all qualnames can be redefined*)
      }

    let init =
      {
        names = { qualname_at_position = None;
                  enclosing_compilation_unit_name = None };
        qualifiers = [];
        qualifiers_for_redef = [];
      }

    let result acc = acc.names

    let qualify n = function
      | [] -> bare n
      | (qn, _) :: _ -> qual n (Some qn)

    let rec pop_qualifiers l = function
      | [] -> []
      | (_, l') :: tl when l <= l' -> pop_qualifiers l tl
      | l -> l

    let push_qualifier n l qualifiers =
      let qn = qualify n qualifiers in
      qn, (qn, l) :: qualifiers

    let update_qualifiers n l qualifiers =
      push_qualifier n l @@ pop_qualifiers l qualifiers

    let reset_qualifiers acc = {acc with qualifiers = []}

  end in

  let srcloc_contains pos loc =
    Lsp_position.is_in_srcloc ~filename pos loc
  in

  let on_name n loc ({ names; qualifiers; _ } as acc) =
    Visitor.do_children @@
    if srcloc_contains pos loc
    then { acc with
           names = { names with
                     qualname_at_position = Some (qualify n qualifiers) } }
    else acc
  in

  let on_item n l loc { names; qualifiers; qualifiers_for_redef } =
    Visitor.do_children @@
      let qn, qualifiers = update_qualifiers n l qualifiers in
      let qualifiers_for_redef =
        (qn, l) :: pop_qualifiers (l + 1) qualifiers_for_redef in
      { names =
          if srcloc_contains pos loc
          then { names with qualname_at_position = Some qn }
          else names;
        qualifiers; qualifiers_for_redef}
  in

  let on_filler l ({qualifiers; qualifiers_for_redef; _} as acc) =
    Visitor.do_children @@
      let qualifiers_for_redef = pop_qualifiers (l + 1) qualifiers_for_redef in
      let qualifiers = pop_qualifiers l qualifiers in
      { acc with qualifiers_for_redef; qualifiers}
  in

  let on_group_item name level loc acc =
    match name with
    | _ when acc.names.qualname_at_position <> None ->
        Visitor.skip_children acc
    | Some { payload = Cobol_ptree.DataFiller; _ } | None ->
        on_filler level acc
    | Some { payload = DataName name; _ } ->
        on_item name level loc acc
  in

  Cobol_ptree.Visitor.fold_compilation_group (object
    inherit [acc] Cobol_ptree.Visitor.folder
    inherit! [acc] Lsp_position.sieve ~filename ~pos

    method! fold_compilation_unit' cu ({ names; _ } as acc) =
      if Lsp_position.is_in_srcloc ~filename pos ~@cu then
        let cu_name = name_of_compunit cu in
        let names = { names with
                      enclosing_compilation_unit_name = Some cu_name } in
        Visitor.do_children { acc with names }
      else
        Visitor.skip_children acc

    method! fold_qualname qn ({ names; _ } as acc) =
      if Lsp_position.is_in_lexloc pos (lexloc_of_qualname_in ~filename qn) then
        let qn = qualname_at_pos ~filename qn pos in
        let names = { names with qualname_at_position = Some qn } in
        Visitor.skip_children { acc with names }
      else
        Visitor.skip_children acc

    (* Note: we bypass the sieve on the following items, and re-implement (sic)
       the name qualification mechanism in sections of the DATA DIVISION. *)

    (* TODO: check whether it is worth resetting the qualifiers stack when
       entering the relevant sections. *)

    method! fold_data_clause clause ({qualifiers_for_redef; names; _} as acc) =
      Visitor.skip_children @@ match clause with
        | DataRedefines name when srcloc_contains pos ~@name ->
            let x = List.find_opt
              (fun (qn, _) ->
                simple_name qn = ~&name)
                qualifiers_for_redef
            in
            begin match x with
            | None -> acc
            | Some (qn, _) ->
                {acc with names = {names with qualname_at_position = Some qn }}
            end
        | _ -> acc

    method! fold_data_item' { loc; payload = { data_level = l;
                                               data_name = n; _ } } =
      on_group_item n ~&l loc

    method! fold_screen_item' { loc; payload = { screen_level = l;
                                                 screen_data_name = n; _ } } =
      on_group_item n l loc

    method! fold_report_group_item' { loc;
                                      payload = { report_level = l;
                                                  report_data_name = n; _ } } =
      on_group_item n l loc

    method! fold_constant_item' { loc; payload = { constant_level = l;  (* 01 *)
                                                   constant_name = n; _ } } acc =
      on_item n ~&l loc (reset_qualifiers acc)

    method! fold_rename_item' { loc; payload = { rename_to; _ } } acc =
      (*rename_item can only be qualified by the data-name of level 01
        ISO/IEC 1989:2014 P379 13.18.45.2(3)                          *)
      on_name rename_to loc {acc with qualifiers = pop_qualifiers 01 acc.qualifiers}

    method! fold_condition_name_item' { loc; payload = { condition_name; _ } } =
      on_name condition_name loc

  end) ptree init |> result

(* --- *)

(** Name definitions retrieval *)

let def_binding qn map =
  (* TODO: Qualmap.update? *)
  try Cobol_data.Qualmap.find qn map
  with Not_found -> { as_paragraph = None; as_item = None }

let def_paragraph qn para map =
  let binding = def_binding qn map in
  Cobol_data.Qualmap.add qn { binding with as_paragraph = Some para } map

let def_item qn item map =
  let binding = def_binding qn map in
  Cobol_data.Qualmap.add qn { binding with as_item = Some item } map

let def_redefinition qn redefine_item map =
  let binding = def_binding qn map in
  match binding.as_item with
  | None ->
      map                                                   (* ignore binding *)
  | Some ({ item_redefinitions = redefs; _ } as item) ->
      def_item qn { item with item_redefinitions = redefine_item :: redefs } map


(** Creates a mapping of item definitions from a type-checked group item. *)
(*TODO: Make it with QualName so we have better definition finding *)
let definitions: compilation_unit -> name_definitions_in_compilation_unit =
  let rec def_group ?cur_qn map { loc; payload = group } =
    let group_infos = match group with
      | Cobol_data.Group.Elementary { name; data_item = Data item; _ } ->
          Some (name, Cobol_ptree.Data item, [])
      | Constant {name; constant_item_descr = Constant c; _ } ->
          Some (Some name, Constant c, [])
      | Group { name; data_item = Data item; elements = children; _ } ->
          Some (name, Data item, children)
      | Renames _                                (* TODO (needs _ item_descr) *)
      | ConditionName _ ->                        (* TODO: ditto*)
          None
    in
    match group_infos with
    | Some (Some group_name, item_definition, children) ->
        (* TODO: group_name: `string with_loc` instead of just `string` *)
        let qn = qual (group_name (* &@ loc *)) cur_qn in
        let map = def_item qn { item_definition = item_definition &@ loc;
                                item_redefinitions = [] } map in
        List.fold_left (def_group ~cur_qn:qn) map children
    | Some (None, item_definition, children) ->
        let map, qn = match cur_qn with
          | Some qn ->
              def_item qn
                { item_definition = item_definition &@ loc;
                  item_redefinitions = [] } map,
              Some qn
          | None ->
              map, None
        in
        List.fold_left (def_group ?cur_qn:qn) map children
    | None ->                                                        (* ignore *)
        map
  in

  fun { cu_wss; _ } ->
    List.fold_left def_group Cobol_data.Qualmap.empty cu_wss


let update_definitions_based_on_compilation_group_ptree ~f ptree defs =
  let cus = CUMap.compilation_units defs in
  List.fold_left begin fun defs cu' ->
    try
      let cu = CUs.find_by_name (name_of_compunit cu') cus in
      CUMap.update cu begin fun cu_defs ->
        Some (f cu' @@ Option.value cu_defs ~default:Cobol_data.Qualmap.empty)
      end defs
    with Not_found -> defs
  end defs ptree.Cobol_ptree.compilation_units


(*TODO: remove this once Cobol_typeck implements Renames*)
let add_rename_item_definitions ptree defs =
(*rename_item can only be qualified by the data-name of level 01
  ISO/IEC 1989:2014 P379 13.18.45.2(3)                          *)
  let visitor = object
    inherit [name_definitions_in_compilation_unit *
             Cobol_ptree.qualname option] Cobol_ptree.Visitor.folder

    method! fold_data_item { data_level; data_name; _ } ((map, _) as acc) =
      Visitor.do_children @@
      match data_name with
      | Some { payload = DataFiller; _ } | None when ~&data_level = 1 ->
          map, None
      | Some { payload = DataName name; _ } when ~&data_level = 1 ->
          map, Some (bare name)
      | _ ->
          acc

    method! fold_rename_item' { payload = { rename_to = name; _ } as rename;
                                loc } (map, cur_qn) =
      Visitor.skip_children @@
      (def_item (qual name cur_qn)
         { item_definition = Cobol_ptree.Renames rename &@ loc;
           item_redefinitions = [] } map,
       cur_qn)
  end in

  update_definitions_based_on_compilation_group_ptree ptree defs
    ~f:begin fun cu' defs ->
      Cobol_ptree.Visitor.fold_compilation_unit' visitor
        cu' (defs, None) |> fst
    end


let add_paragraph_definitions ptree defs =

  let register_section name s (defs, _section_name) =
    let qn = bare name in
    Visitor.do_children (def_paragraph qn s defs, Some qn)

  and register_paragraph name p (defs, section_name) =
    let qn = qual name section_name in
    Visitor.do_children (def_paragraph qn p defs, section_name)
  in

  let visitor = object
    inherit [name_definitions_in_compilation_unit *
             Cobol_ptree.qualname option] Cobol_ptree.Visitor.folder

    method! fold_environment_division _ = Visitor.skip
    method! fold_data_division' _ = Visitor.skip

    method! fold_paragraph' p = match ~&p with
      | { paragraph_name = None; _ } -> Visitor.skip_children
      | { paragraph_is_section = true;
          paragraph_name = Some name; _ } -> register_section name p
      | { paragraph_name = Some name; _ } -> register_paragraph name p

  end in
  update_definitions_based_on_compilation_group_ptree ptree defs
    ~f:begin fun cu' defs ->
      Cobol_ptree.Visitor.fold_compilation_unit' visitor
        cu' (defs, None) |> fst
    end


let add_redefine_definitions ptree defs =
  let open struct
    type acc =
      {
        defs: name_definitions_in_compilation_unit;
        qualifiers: (Cobol_ptree.qualname * int) list;      (* qualifiers stack *)
        aux: (Cobol_ptree.qualname * int) list;             (* all qualnames can be redefined*)
      }

    let qualify n = function
      | [] -> bare n
      | (qn, _) :: _ -> qual n (Some qn)

    let rec pop_qualifiers l = function
      | [] -> []
      | (_, l') :: tl when l <= l' -> pop_qualifiers l tl
      | l -> l

    let push_qualifier n l qualifiers =
      let qn = qualify n qualifiers in
      qn, (qn, l) :: qualifiers

    let update_qualifiers n l qualifiers =
      push_qualifier n l @@ pop_qualifiers l qualifiers

    let result acc = acc.defs

  end in

  let visitor = object
    inherit [acc] Cobol_ptree.Visitor.folder

    method! fold_environment_division _ = Visitor.skip
    method! fold_procedure_division _ = Visitor.skip

    method! fold_data_item {data_level; data_name; _} ({qualifiers; aux; _} as acc) =
      let l = ~&data_level in
      match data_name with
      | Some { payload = Cobol_ptree.DataFiller; _ } | None ->
          Visitor.do_children @@
            let aux = pop_qualifiers (l + 1) aux in
            let qualifiers = pop_qualifiers l qualifiers in
            { acc with aux; qualifiers}
      | Some { payload = DataName name; _ } ->
          Visitor.do_children @@
            let qn, qualifiers = update_qualifiers name l qualifiers in
            let aux = (qn, l) :: pop_qualifiers (l + 1) aux in
            { acc with qualifiers; aux}

    method! fold_data_clause clause ({defs; aux; _} as acc) =
      Visitor.skip_children @@ match clause with
        | DataRedefines name ->
            let x = List.find_opt
              (fun (qn, _) ->
                simple_name qn = ~&name)
              aux
            in
            begin match x with
            | None -> acc
            | Some (qn, _) ->
                {acc with defs = def_redefinition qn name defs}
            end
        | _ -> acc

  end in
  update_definitions_based_on_compilation_group_ptree ptree defs
    ~f:begin fun cu' defs ->
      Cobol_ptree.Visitor.fold_compilation_unit' visitor
        cu' {defs; aux = []; qualifiers = []} |> result
    end


let references_of_qualname qn cu cu_defs =

  let visitor key = object
    (*[key] is the full qualname of the data*)
    inherit [srcloc list] Cobol_ptree.Visitor.folder
    method! fold_environment_division _ = Visitor.skip
    method! fold_qualname qn locs =
      match Cobol_data.Qualmap.find_full_qualname_opt qn cu_defs with
      | Some full_qn when Cobol_ptree.compare_qualname full_qn key = 0 ->
          Visitor.skip_children @@ baseloc_of_qualname qn :: locs
      | Some _ ->
          Visitor.do_children locs
      | None ->
          Visitor.skip_children locs
  end in

  let qn = match Cobol_data.Qualmap.find_full_qualname_opt qn cu_defs with
    | Some qn -> qn
    | None -> qn
  in
  let refs_in_data_div_or_proc_div =
    Cobol_ptree.Visitor.fold_compilation_unit'
      (visitor qn) cu []
  and refs_in_redef_clauses =                            (* redefinitions *)
    match Cobol_data.Qualmap.find qn cu_defs with
    | { as_item = Some { item_redefinitions = redefs; _ }; _ } ->
        List.rev_map (~@) redefs
    | _ -> []
    | exception Not_found -> []
  in
  refs_in_redef_clauses @ List.rev refs_in_data_div_or_proc_div

let references cu_defs cu =
  Cobol_data.Qualmap.fold
    (fun qn _ map ->
      Cobol_data.Qualmap.add qn (references_of_qualname qn cu cu_defs) map)
    cu_defs Cobol_data.Qualmap.empty

let copy_at_pos ~filename pos ptree =
  Cobol_ptree.Visitor.fold_compilation_group (object
    inherit [copy_operation option] Cobol_ptree.Visitor.folder
    method! fold' { loc; _ } = function
      | Some _ as acc ->
          Visitor.skip_children acc
      | None ->
          match Srcloc.as_copy loc with
          | Some { loc; _ } as copy
            when Lsp_position.is_in_srcloc ~filename pos loc ->
              Visitor.skip_children copy
          | _ ->
              Visitor.do_children None
  end) ptree None
