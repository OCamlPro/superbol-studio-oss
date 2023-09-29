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

open Cobol_common.Srcloc.INFIX
open Pictured_ast.Data_sections
open Types
open Cobol_ast

let cobol_class_of_picture: Picture.t -> Types.elementary_data_class = fun pic ->
  match pic.category with
  | Alphabetic _ -> Alphabetic
  | Alphanumeric _ -> Alphanumeric
  | Boolean _ -> Boolean
  | National _ -> National
  | FixedNum _ | FloatNum _ -> Numeric

let rec of_data_group
    ((module Diags: Cobol_common.Diagnostics.STATEFUL) as diags)
    program_environment data_group =
  match ~&data_group with
  | Group.Elementary {data_item = Data dde; name = _} ->
      let level = ~&(dde.data_level) and loc = ~@data_group in
      let picture, usage, occurs =
        List.fold_left begin fun (pic, usage, occurs) { payload = clause; _ } ->
          match clause with
          | DataPicture { payload = { picture = pic; _ }; _ } ->
              Some pic, usage, occurs
          | DataUsage usage_clause ->
              pic, Some usage_clause, occurs
          | DataOccurs occurs_clause ->
              pic, usage, Some occurs_clause
          | _ ->
              pic, usage, occurs
        end (None, None, None) dde.data_clauses
      in
      let element =
        match picture, usage with
        | Some pic, None ->
            Ok (Elementary ({ typ = cobol_class_of_picture ~&pic; level },
                            Some ~&pic) &@ loc)
        | None, Some usage ->
            begin match usage with
              | Index ->
                  Ok (Elementary ({typ = Index; level}, None) &@ loc)
              | Pointer _
              | FunctionPointer _
              | ProgramPointer _ ->
                  Ok (Elementary ({typ = Pointer; level}, None) &@ loc)
              (* | ProgramPointer _ -> *)
              (*     Result.ok @@ Elementary (({typ = Types.Pointer (\* L8 *\); level}, None) &@ loc) *)
              | ObjectReference _ ->
                  Ok (Elementary ({typ = Types.Object; level}, None) &@ loc)
              | BinaryChar _
              | BinaryShort _
              | BinaryLong _
              | BinaryDouble _
              | FloatBinary32 _
              | FloatBinary64 _
              | FloatBinary128 _
              | FloatDecimal16 _
              | FloatDecimal34 _
              | FloatShort
              | FloatLong
              | FloatExtended ->
                  (* As per ISO/IEC 1989:2014, 8.5.2.10 Numeric category *)
                  Ok (Elementary ({typ = Numeric; level}, None) &@ loc)
              | UsagePending (`Comp1 | `Comp2) ->
                  Ok (Elementary ({typ = Numeric; level}, None) &@ loc)
              | _ ->
                  Diags.error ~loc "Missing@ PICTURE@ clause";
                  Result.Error ()
            end
        | Some picture, Some usage ->
            let cobol_class = cobol_class_of_picture ~&picture in
            begin match usage, cobol_class with
              | (Binary | PackedDecimal |
                UsagePending (`Comp3 | `Comp5 | `Comp6 | `CompX)), Numeric ->
                  Ok (Elementary ({ typ = cobol_class; level }, Some ~&picture) &@ loc)
              | (Binary | PackedDecimal |
                UsagePending (`Comp3 | `Comp5 | `Comp6 | `CompX)), _ ->
                  Diags.error ~loc
                    "The picture associated with a USAGE clause of type BINARY \
                     (COMP) or PACKED-DECIMAL must be a numeric picture";
                  Error ()
              | _ ->
                  Ok (Elementary ({typ = cobol_class; level}, Some ~&picture) &@ loc)
            end
        | None, None ->
            Diags.error ~loc "Missing@ USAGE@ or@ PICTURE@ clause@ for@ \
                              elementary@ item";
            Error ()
      in
      Result.bind
        element
        (fun element -> match occurs with
           | Some occurs_clause ->
               begin match occurs_clause with
                 | OccursFixed occurs_fixed ->
                     Ok (Table { typ = { elements_type = element;
                                         length = Fixed occurs_fixed.times };
                                 level } &@ loc)
                 | OccursDepending { from; to_; depending; _ } ->
                     let length = Types.OccursDepending { min_size = from;
                                                          max_size = to_;
                                                          depending } in
                     Ok (Table { typ = { elements_type = element; length }; level } &@ loc)
                 | OccursDynamic _ ->
                     Diags.warn ~loc "Table with dynamic capacity are not implemented yet.";
                     Result.Error ()
               end
           | None -> Result.Ok element)
  | Group {elements = dgl; data_item = Data dde; name = _} ->
      let level = ~&(dde.data_level) and loc = ~@data_group in
      let elements =
        (* If a group has usage clause then it is applied to every elementary
           item of this group. *)
        try
          let usage =
            List.find (function { payload = DataUsage _; _ } -> true | _ -> false)
              dde.data_clauses
          in
          let rec prepend_usage usage ({ payload; loc } as dg) =
            match payload with
            | Group.Elementary ({data_item = Data ({ data_clauses;
                                                     _} as data_item);
                                 _ } as element) ->
                let data_item = Data { data_item with
                                       data_clauses = usage :: data_clauses } in
                Group.Elementary { element with data_item } &@ loc
            | Group ({ elements; _ } as group) ->
                let elements = List.map (prepend_usage usage) elements in
                Group.Group { group with elements } &@ loc
            | _  ->
                dg
          in
          List.map (prepend_usage usage) dgl
        with Not_found -> dgl
      in
      let elements =
        List.map
          (fun dg -> of_data_group diags program_environment dg)
          elements
        |> Cobol_common.join_all
      in
      let occurs =
        List.find_opt begin fun ddc -> match ~&ddc with
          | DataOccurs _ -> true
          | _ -> false
        end dde.data_clauses
      in
      Result.map
        (fun elements ->
           match occurs with
           | Some { payload = DataOccurs occurs_clause; _ } ->
               let group = Group { typ = elements; level } &@ loc in
               begin
                 match occurs_clause with
                 | OccursFixed occurs_fixed ->
                     Types.Table { typ = { elements_type = group;
                                           length = Fixed occurs_fixed.times };
                                   level } &@ loc
                 | OccursDepending { from; to_; depending; _ } ->
                     let length =
                       Types.OccursDepending { min_size = from;
                                               max_size = to_;
                                               depending }
                     in
                     Table { typ = { elements_type = group; length }; level } &@ loc
                 | _ -> failwith "Not implemented yet"
               end
           | Some _ -> failwith "Unreachable"
           | None -> Group { typ = elements; level } &@ loc)
        elements
  | Renames { targets; _ } ->
      (* RENAMES rules: first and last must not be occurs or subordinate to occurs, but can
                          have occurs subordinate to them. These subordinate occurs must be of
                          fixed length. *)
      let loc = ~@data_group in
      let has_occurs = function
        | { payload = DataOccurs _; _ } -> true
        | _ -> false
      and has_complex_occurs = function
        | { payload = DataOccurs (OccursDepending _ |
                                  OccursDynamic _); _ } -> true
        | _ -> false
      in
      let rec check_data_group is_first_or_last (data_group: Group.t) =
        match ~&data_group with
        | Renames _ | Constant _ -> true
        | ConditionName _  ->
            Diags.error ~loc "RENAMES@ may@ not@ reference@ a@ level@ 88.";
            false
        | Elementary { data_item = Data data_item; _ }
        | Group { data_item = Data data_item; _ }
          when is_first_or_last &&
               List.exists has_occurs data_item.data_clauses ->
            Diags.error ~loc "Forbidden@ RENAMES@ of@ data@ item@ with@ \
                              OCCURS@ clause.";
            false
        | Elementary _
        | Group _
          when is_first_or_last ->
            true
        | Elementary { data_item = Data data_item; _ }
          when List.exists has_complex_occurs data_item.data_clauses ->
            Diags.error ~loc "RENAMES@ cannot@ reference@ a@ data@ item@ with@ \
                              OCCURS@ DEPENDING@ or@ OCCURS@ DYNAMIC@ clause";
            false
        | Elementary _ ->
            true
        | Group { data_item = Data data_item; _ }
          when List.exists has_complex_occurs data_item.data_clauses ->
            Diags.error ~loc "RENAMES@ cannot@ reference@ a@ data@ item@ with@ \
                              OCCURS@ DEPENDING@ or@ OCCURS@ DYNAMIC@ clause";
            false
        | Group { elements; _ } ->
            List.for_all (check_data_group false) elements
      in
      begin match targets with
        | hd::[] ->
            if check_data_group true hd then
              of_data_group diags program_environment hd
            else
              Error ()
        | (hd::tl) ->
            let tl_rev = List.rev tl in
            let last = List.hd tl_rev in
            let middle = List.rev @@ List.tl tl_rev in
            if check_data_group true hd && check_data_group true last
               && List.for_all (check_data_group false) middle
            then
              let elements =
                List.map (of_data_group diags program_environment) targets
                |> Cobol_common.join_all
              in
              Result.map (fun elements -> Group {typ = elements; level = 66} &@ loc) elements
            else
              Error ()
        | [] ->
            Diags.error ~loc "The@ RENAMES@ clause@ cannot@ be@ typed@ (empty@ group@ types)";
            Error ()
      end
  | Constant _ ->
      Diags.error "Not implemented yet: Type of constant.";
      Result.Error ()
  | ConditionName {name = _; _} ->
      (* Condition-names are not elementary data items.  Instead, they are
         subordicate to a group. *)
      (* Ok (Elementary (({typ = Conditional; level = 88L }, None) &@<- data_group)) *)
      Diags.error "Not implemented yet: Type of condition name";
      Result.Error ()

(*TODO: * Check if first or last item has an occurs clauses.
        * Take into account subordinates occurs clauses: Idea:
  - In the environment make a fixed lenght table have the same number of sub items as
              the number of occurs, i.e 10 TAB PIC XX OCCURS 3 TIMES would have the sub items
              TAB1, TAB2, TAB3 of type Alphanumeric 2.
*)
(* let of_rename_entry (prog_env: Cobol_env.PROG_ENV.t) dde_before rename_entry =
   let renamed_qualname = QualMap.find_full_qualname ~&rename_entry.renamed_item prog_env.data_items in
   match ~&rename_entry.through_opt with
   | None ->
      let renamed_data_item = QualMap.find renamed_qualname prog_env.data_items in
      let data_item_type = match renamed_data_item.typ with
        | None -> failwith "Please type the classic data items before typing the renames entry."
        | Some (typ) -> typ
      in
      data_item_type
   | Some(through_item) ->
      let through_qualname = QualMap.find_full_qualname through_item prog_env.data_items in
      let paths = Cobol_data_group.range_of_data_groups renamed_qualname through_qualname [dde_before] in
      let renamed_elements = List.fold_left (fun acc (occurs, path) ->
          let data_item_qualname = Group.of_list path in
          let data_item =  QualMap.find data_item_qualname prog_env.data_items in
          let data_name = data_item.name in
          let data_type = match data_item.typ with
            | None -> failwith "The items must be typed"
            | Some typ -> typ
          in
          let rec add_occurs acc n data_name data_type =
            match n with
            | 1 -> List.rev acc
            | _ ->
                let new_name = Format.sprintf "__%s_%d" data_name (n - 2) in
                add_occurs ({field_name = new_name; field_type = data_type}::acc) (n-1) data_name data_type
          in
          let new_list =
            add_occurs
              [{field_name = ~&data_name; field_type = data_type}]
              occurs
              ~&data_name
              data_type
          in
          acc@new_list
        ) [] paths
      in
      let typ =
        Cobol_types.Group ({ typ = renamed_elements; level = 66L } &@<-
                           rename_entry) in
      let name = ~&rename_entry.data_name in
      print_endline ([%derive.show: (string * cob_data_type)] (~&name, typ));
      typ *)
