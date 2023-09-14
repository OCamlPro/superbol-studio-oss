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

open Cobol_ast
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Pictured_ast.Data_sections

(** This module implements data_groups which are how we group together all the
    {!Cobol_parser.Ptree.data_description} in grouped items before typing them. *)

(*TODO: Maybe renames can be a list of qualnames*)
type t' =
  | Renames of {name: name; targets: t list }
  | ConditionName of {name: name; values: condition_name_value list; target: t }
  | Constant of { name: name; value: constant_value with_loc;
                  constant_item_descr: constant_item_descr }
  | Elementary of { name: name; data_item: data_item_descr }
  | Group of { name: name; elements: t list; data_item: data_item_descr }
[@@deriving show]

and t = t' with_loc
[@@deriving show]

let pp_data_group_list ppf =
  Pretty.list ~fsep:"@ " ~fopen:"@[" ~fclose:"@]" ~fempty:""
    pp ppf

let name_of: t -> name = fun g -> match ~&g with
  | Group {name; _} | Elementary {name; _} | ConditionName {name; _}
  | Constant {name; _} | Renames {name; _} ->
      name

(* let name_location g = ~@(name_of g) *)

(** [group_at_level level acc groups] returns all the groups with the level [level]. The value
    returned is of type
    {!type:(data_item * condition_name_item list * (data_item * condition_name_item list) list) list}
    which represent the group at level [level] and the condition names associated with every entry of
    the group and a list of subgroups associated with every item of the group at level [level]. *)
let rec group_at_level level acc (group: working_storage_item_descr list) =
  match group with
  | [] -> acc
  | (Data hd)::tl ->
      let conditions, rest = conditions_from [] tl in
      let sub_group, rest = subgroup_of [] level rest in
      group_at_level level ((hd, conditions, sub_group)::acc) rest
  | (CondName cond)::rest ->
      begin match acc with
      | (hd, conds, sub_group)::tl ->
          group_at_level level ((hd, cond::conds, sub_group)::tl) rest
      | _ -> invalid_arg "Empty list"
      end
  | _ -> invalid_arg "Expecting Data or CondName"

(** [conditions_from acc group] returns a list of condition names and the rest of the group from [group]. *)
and conditions_from acc group =
match group with
  | (CondName cond)::tl ->
      conditions_from (cond::acc) tl
  | _ ->
      List.rev acc, group

(** [subgroup_from acc level group] returns a list of elements that are subordinate to the level
    [level] and the rest of the group [group] which is not contained in the current subgroup
    (i.e any item following an item of level [level] or lower including the item of level
    [level] or lower). *)
and subgroup_of acc curr_level group =
  match group with
  | [] -> List.rev acc, []
  | (Data ({data_level; _} as hd))::tl ->
      if data_level > curr_level then
         subgroup_of ((hd, [])::acc) curr_level tl
      else
        List.rev acc, group
  | (CondName cond)::rest ->
      begin match acc with
        | (hd, conds)::tl ->
            subgroup_of ((hd, (cond::conds))::tl) curr_level rest
        | _ ->
            invalid_arg "Empty list"
      end
  | _  ->
      invalid_arg "Expecting Data or CondName"

(** [subgroup_of' level group] has the same behavior as [subgroup_of level acc group] but
    the [group] is of type {!type:(data_item * condition_name_item list) list} instead of type
    {!type:working_storage_item_descr list}*)
let subgroup_of' curr_level (group: (data_item * condition_name_item list) list) =
  let rec aux acc curr_level group =
    match group with
    | [] -> (List.rev acc, [])
    | (({data_level; _}, _) as hd)::tl ->
        if ~&data_level > ~&curr_level then
            aux (hd::acc) curr_level tl
        else
          List.rev acc, group
  in
  aux [] curr_level group

(** [group_at_level' level group] has the same behavior as [group_at_level level acc group] but
    the [group] is of type {!type:(data_item * 'a) list} instead of type {!type:working_storage_item_descr list}*)
let group_at_level' level group =
  let rec aux acc level group =
    match group with
    | [] -> acc
    | hd::tl ->
        let sub_group, rest = subgroup_of' level tl in
        aux ((hd, sub_group)::acc) level rest
  in
  aux [] level group

(** [make_data_subgroup (module Diags) group] returns a {!t list} from a {!(dde * condition_name list) list}
    specifically for groups of levels higher than 1 and different of 77. *)
let make_data_subgroup (module Diags: Cobol_common.Diagnostics.STATEFUL) group =
  let rec aux group =
    match group with
    | [] -> invalid_arg "Empty list"
    | (({data_name; _}: data_item) as data_item, cond_names)::[] ->
        begin match data_name with
          | Some { payload = DataName name; loc } ->
              let element = Elementary {name = ~&name;
                                        data_item = Data data_item } &@ loc in
              element :: List.map
                (fun {condition_name; condition_name_values; _} ->
                   ConditionName { name = ~&condition_name;
                                   values = condition_name_values;
                                   target = element } &@<- condition_name)
                cond_names
          | _ -> invalid_arg "Expecting a name"
        end
    | ({data_level = level; _}, _)::_ ->
        let groups = List.rev (group_at_level' level group) in
        List.flatten @@ List.map
          (fun (({ data_name = name; _ } as data_item, cond_names), sub_groups) ->
            match name with
            | Some { payload = DataName name; loc } ->
                begin match sub_groups with
                  | [] ->
                      let element = Elementary { name = ~&name;
                                                 data_item = Data data_item } &@ loc in
                      element::
                      (List.map
                         (fun {condition_name = name; condition_name_values = values; _} ->
                            ConditionName { name = ~&name; values;
                                            target = element } &@<- name)
                         cond_names)
                  | sub_groups ->
                      let group = Group { name = ~&name; elements = aux sub_groups;
                                          data_item = Data data_item } &@ loc in
                      group::
                      (List.map
                         (fun {condition_name = name; condition_name_values = values; _} ->
                            ConditionName {name = ~&name; values;
                                           target = group} &@<- name)
                         cond_names)
                end
            | _ -> invalid_arg "Expecting a name")
          groups
  in
  aux group

(** [make_data_group (module Diags) group] makes a {!(t list, unit) result} from a
    {!working_storage_item_descr list} which respect the hierarchy of the list in entry and results
    in error if any of the elements of the entry list does not respect the COBOL data groups rules. *)
let make_data_group (module Diags: Cobol_common.Diagnostics.STATEFUL) group =
  match (group: working_storage_item_descr list) with (* TODO: with_loc *)
  | [Constant {constant_name; constant_value; _} as constant_item_descr] ->
      begin match constant_name with
        | Some { payload = DataName name; loc } ->
            Result.ok [Constant { name = ~&name;
                                  value = constant_value;
                                  constant_item_descr } &@ loc ]
        | _ ->
            Diags.error "Undefined constant name";
            Result.Error ()
      end
  | [Data { data_name; _ } as data_item] ->
      begin match data_name with
        | Some { payload = DataName name; loc } ->
            Result.ok @@ [Elementary {name = ~&name; data_item } &@ loc ]
        | _ ->
            Diags.error "Unkown data item";
            Result.Error ()
      end
  | CondName {condition_name = {loc; _}; _}::_ ->
      Diags.error ~loc "A@ condition@ name@ must@ follow@ another@ data@ item@ entry";
      Result.Error ()
  | Data {data_level = level; _} ::_ ->
      let same_level_groups = List.rev (group_at_level level [] group) in
      List.map
        (fun (data_item, cond_names, sub_group) ->
           let name =
             match data_item.data_name with
             | Some {payload = DataName name; _} ->
                 Result.Ok name
             | _ ->
                 Diags.error "Unnamed data item";
                 Result.Error ()
           in
           match sub_group with
           | [] ->
               Result.map
                 (fun name ->
                    let element = Elementary {name = ~&name;
                                              data_item = Data data_item} &@<- name in
                    element::
                    List.map
                      (fun {condition_name = name; condition_name_values = values; _} ->
                         ConditionName { name = ~&name; values;
                                         target = element } &@<- name)
                      cond_names)
                 name
           | lst ->
               Result.map
                 (fun name ->
                    let group =
                      Group { name = ~&name;
                              elements = make_data_subgroup (module Diags) lst;
                              data_item = Data data_item } &@<- name
                    in
                    group::
                    (List.map
                       (fun {condition_name = name; condition_name_values = values; _} ->
                          ConditionName {name = ~&name; values;
                                         target = group} &@<- name)
                       cond_names))
                 name)
        same_level_groups
      |> Cobol_common.join_all
      |> Result.map List.flatten
  | _ -> Result.error ()

(** [group_of_name name group] return a list of groups that are named [name]. *)
let groups_of_name {payload=group_name;_} group =
  let rec aux acc group =
    match ~&group with
    | Elementary {name; _} | Group {name; _} when name = group_name ->
        group::acc
    | Group {elements; _} ->
        List.fold_left aux acc elements
    | _ ->
        acc
  in
  aux [] group

(** [group_of_list name_list groups] returns a list of groups from [groups] whose name correspond to
    [name_list], the groups must respect the hierarchy of name list with the following rules:
    - [name_list] should be hierarchical with the first value being the lowest level name (i.e level 01 first),
    - if [name_list] is empty all the groups from [groups] are returned. *)
let rec groups_of_list name_list groups =
  match name_list with
  | [] -> groups
  | hd::tl ->
      let groups =
        List.flatten @@ List.fold_left (fun acc group ->
            (groups_of_name hd group)::acc)
          []
          groups
      in
      groups_of_list tl groups

(** [group_range (module Diags) first last group] returns a list of every {!Elementary _} or
    {!Group _} which are defined between [first] and [last] (included).
    [first] and [last] should be qualified names that are in the form of a list, with
    lowest level name first.
    [first] should not be the name of any item succeding [last] in the group and [last] should
    not be name of any item defined before [first].
    [first] and [last] should be the name of item defined inside [group].
    [first] and [last] should not be ambigious as to which item they define. *)
let group_range (module Diags: Cobol_common.Diagnostics.STATEFUL) first last group =
  let rec aux (first, last, first_found, last_found, acc) group =
    if first_found && last_found then
      first, last, first_found, last_found, acc
    else
      match ~&group with
      | Elementary {name; _} ->
          if first_found && not @@ last_found then
            let last_found =
              match last with
              | hd::[] when name = ~&hd -> true
              | _ -> false
            in
            first, last, first_found, last_found, group::acc
          else
            begin match first with
              | hd::[] when name = ~&hd ->
                  let last_found =
                    match last with
                    | hd::[] when name = ~&hd -> true
                    | _ -> false
                  in
                  first, last, true, last_found, group::acc
              | _ ->
                  first, last, first_found, last_found, acc
            end
      | Group {name; elements; _} ->
          if first_found && not last_found then
            let last_found =
              match last with
              | hd::[] when name = ~&hd -> true
              | _ -> false
            in
            match last with
            | hd::[] when name = ~&hd ->
                List.fold_left aux (first, last, first_found, last_found, acc) elements
            | hd::tl when name = ~&hd ->
                List.fold_left aux (first, tl, first_found, last_found, acc) elements
            | _ ->
                first, last, first_found, last_found, group::acc
          else
            let _, _, first_found, last_found, acc =
              (* Fmt.pr "Name: %a, First: %a\n" pp_name name Fmt.(list pp_name) first; *)
              let first =
                match first with
                | hd::tl when name = ~&hd ->
                    (* Fmt.pr "New first: %a\n" Fmt.(list pp_name) tl; *)
                    tl
                | _ -> first
              in
              let last =
                match last with
                | hd::tl when name = ~&hd ->
                    tl
                | _ ->
                    last
              in
              List.fold_left aux (first, last, first_found, last_found, acc) elements
            in
            first, last, first_found, last_found, acc
      | _ -> first, last, first_found, last_found, acc

  in
  let _, _, _, _, groups = aux (first, last, false, false, []) group in
  groups

(** [make_renames (module Diags) group renames] returns a list of {!(t, unit) result} which
    are all of the form [Renames _] and correspond to renaming items from the group [group]. *)
let make_renames (module Diags: Cobol_common.Diagnostics.STATEFUL) group renames =
  (* TODO: take `rename with_loc` entries, with more "general" locations *)
  List.fold_left begin fun acc { rename_to; rename_renamed; rename_through; _ } ->
    match rename_through with
    | Some through_name ->
        (* TODO: avoid reliance on a list representation. *)
        let first = Cobol_ast.list_of_qualname rename_renamed in
        let last = Cobol_ast.list_of_qualname through_name in
        let groups =
          group_range
            (module Diags: Cobol_common.Diagnostics.STATEFUL)
            first
            last
            (List.hd group)
        in
        Ok (Renames {name = ~&rename_to;
                     targets = List.rev groups} &@<- rename_to) :: acc
    | None ->
        let name_list = Cobol_ast.list_of_qualname rename_renamed in
        let sub_groups = groups_of_list name_list group in
        if List.length sub_groups <> 1 then
          begin
            Diags.error ~loc:~@rename_to "Could not find a unique name to rename.";
            Error () :: acc
          end
        else
          let group = List.hd sub_groups in
          Ok (Renames {name = ~&rename_to;
                       targets = [group]} &@<- rename_to) :: acc
  end [] renames

(** [of_working_storage (module Diags) wss] returns the list of groups from wss in a hierarchical
    form. *)
let of_working_storage
    (module Diags: Cobol_common.Diagnostics.STATEFUL)
    (wss: working_storage_item_descr with_loc list) =
  let groups =
    List.fold_left (fun acc {payload; loc} ->
        Result.bind acc (fun acc ->
            match (payload: working_storage_item_descr) with
            | Constant _ ->
                Result.ok @@ ([payload], None)::acc
            | Data {data_level = level; _} when ~&level = 1 || ~&level = 77 ->
                Result.ok @@ ([payload], None)::acc
            | Data _ ->
                begin match acc with
                  | (payload_list, None)::tl ->
                      Result.ok @@ (payload::payload_list, None)::tl
                  | (_, Some _)::_ ->
                      Diags.error ~loc "A@ renames@ entry@ can@ only@ be@ followed@ by@ a@ 01,@ 66,@ 77@ \
                                        or@ 88@ level@ data@ item.";
                      Result.Error ()
                  | [] ->
                      Diags.error ~loc "A@ non@ 01@ or@ 77@ level@ must@ follow@ a@ 01@ level@ data@ item.";
                      Result.Error ()
                end
            | Renames rename ->
                begin match acc with
                  | (data_list, None)::tl ->
                      Result.ok @@ (data_list, Some ([rename]))::tl
                  | (data_list, Some renames)::tl ->
                      Result.ok @@ (data_list, Some (rename::renames))::tl
                  | [] ->
                      Diags.error ~loc "A@ 66@ level@ entry@ must@ follow@ directly@ a@ record@ entry.";
                      Result.Error ()
                end
            | CondName _ ->
                begin match acc with
                  | (data_list, renames)::tl ->
                      Result.ok @@ (payload::data_list, renames)::tl
                  | [] ->
                      Diags.error ~loc "An@ 88@ level@ entry@ must@ follow@ another@ data@ entry.";
                      Result.Error ()
                end))
      (Result.Ok [])
      wss
    |> Result.map (List.map (fun (data, renames) -> List.rev data, Option.map List.rev renames))
    |> Result.map List.rev
  in
  Result.bind groups (fun groups ->
      Cobol_common.join_all @@
      List.map (fun (group, rename) ->
          let group = make_data_group (module Diags) group in
          Result.bind group (fun group ->
              Option.fold
                ~none:[]
                ~some:(fun renames -> make_renames (module Diags) group renames)
                rename
              |> Cobol_common.join_all
              |> Result.map (fun renames ->
                  List.map (function
                      | { payload = Group {name; elements; data_item}; loc } ->
                          Group { name;
                                  elements = elements@renames;
                                  data_item } &@ loc
                      | _ as elem ->
                          elem)
                    group)))
        groups)
  |> Result.map List.flatten
