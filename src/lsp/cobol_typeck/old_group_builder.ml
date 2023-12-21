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

open Cobol_ptree
open Cobol_common.Srcloc.INFIX
open Cobol_common.Diagnostics.TYPES

module Cobol_data = Cobol_data.OLD

module DIAGS = Cobol_common.Diagnostics
module Visitor = Cobol_common.Visitor

let rev_and_validate_data_item_descrs
    (type k) : k item_descr with_loc list with_diags -> _ =
  fun { result = rev_data_item_descrs; diags } ->
  (* Associate each item with an `is_elementary` flag during reversal, and
     then map the result again to obtain messages in order of declarations. *)

  let _, flagged_data_item_descrs =
    List.fold_left begin fun (next_level, acc) descr ->
      let is_elementary curr_level = curr_level >= next_level in
      match (~&descr: k item_descr) with
      | Data { data_level; _ } when ~&data_level = 77 ->
          0, (descr, true) :: acc              (* special noncontiguous item *)
      | Data { data_level = { payload = l; _ }; _ }
      | Renames { rename_level = {payload = 66 as l; _}; _ }
      | Screen { screen_level = l; _ }
      | ReportGroup { report_level = l; _ } ->
          l, (descr, is_elementary l) :: acc
      | Constant _ ->
          0, (descr, false) :: acc
      | Renames _ ->                                       (* force level 66 *)
          66, (descr, false) :: acc
      | CondName _ ->
          88, (descr, false (* CHECKME: ??? *)) :: acc
    end (0, []) rev_data_item_descrs
  in

  let diags, res =
    List.fold_left_map begin fun diags (descr, is_elementary) ->
      match (~&descr: k item_descr) with
      | Data { data_level = l; _ } when ~&l < 1 || (~&l > 49 && ~&l <> 77) ->
          DIAGS.Acc.error diags
            ~loc:~@l "Invalid@ level@ %d@ for@ data@ item" ~&l,
          descr
      | Renames { rename_level = {payload = l; _}; _ } when l <> 66 ->
          DIAGS.Acc.warn diags
            ~loc:~@descr "RENAMES@ data@ item@ should@ be@ at@ level@ 66",
          descr
      | Data item ->
          DIAGS.Set.union diags @@
          Cobol_validation.validate_data_clauses ~is_elementary
            (item &@<- descr),
          descr
      | _ ->                                                         (* TODO *)
          diags,
          descr
    end diags flagged_data_item_descrs
  in
  DIAGS.result res ~diags

let cobol_class_of_picture
  : Cobol_data.Picture.t with_loc -> Cobol_data.Types.elementary_data_class =
  fun { payload = pic; _ } ->
  match pic.category with
  | Alphabetic _ -> Alphabetic
  | Alphanumeric _ -> Alphanumeric
  | Boolean _ -> Boolean
  | National _ -> National
  | FixedNum _ | FloatNum _ -> Numeric

let rec from_item_descrs config prog_env data_group : _ with_diags =

  let module Config = (val config: Cobol_config.T) in

  let picture_of_string Cobol_data.PROG_ENV.{ decimal_point;
                                              currency_signs; _ } s =
    let module E = struct
      let decimal_char = decimal_point
      let currency_signs = currency_signs
    end in
    let module PIC = Cobol_data.Picture.Make (val config) (E) in
    try DIAGS.result @@ PIC.of_string s with
      PIC.InvalidPicture (str, diags, dummy) ->
        DIAGS.result ~diags (dummy &@<- str)
  in

  let acc_occurs ~occurs_clause ~level ~loc element =
    DIAGS.more_result ~f:begin fun element ->
      match element, Option.map (~&) occurs_clause with
      | None, _ ->
          DIAGS.result None
      | Some element, None ->
          DIAGS.some_result element
      | Some element, Some OccursFixed occurs_fixed ->
          DIAGS.some_result @@
          (Cobol_data.Types.Table { typ = { elements_type = element;
                                            length = Fixed ~&(occurs_fixed.times) };
                                    level } &@ loc)
      | Some element, Some OccursDepending { from; to_; depending; _ } ->
          DIAGS.some_result @@
          (Cobol_data.Types.Table { typ = { elements_type = element;
                                            length = OccursDepending { min_size = ~&from;
                                                                       max_size = ~&to_;
                                                                       depending } };
                   level } &@ loc)
      | Some _, Some OccursDynamic _ ->
          DIAGS.warn_result None ~loc
            "Table with dynamic capacity are not implemented yet."
    end element
  in

  match ~&data_group with
  | Cobol_data.Group.Elementary {data_item = Data dde; name = _} ->
      let level = ~&(dde.data_level) and loc = ~@data_group in
      let picture, usage, occurs =
        List.fold_left begin fun (pic, usage, occurs) { payload = clause; loc } ->
          match clause with
          | DataPicture { payload = { picture = pic; _ }; _ } ->
              Some pic, usage, occurs
          | DataUsage usage_clause ->
              pic, Some usage_clause, occurs
          | DataOccurs occurs_clause ->
              pic, usage, Some (occurs_clause &@ loc)
          | _ ->
              pic, usage, occurs
        end (None, None, None) dde.data_clauses
      in
      let element =
        match picture, usage with
        | Some pic, None ->
            let pic = picture_of_string prog_env pic in
            DIAGS.map_result ~f:begin fun pic ->
              Some (Cobol_data.Types.Elementary ({ typ = cobol_class_of_picture pic; level },
                                                 Some ~&pic) &@ loc)
            end pic
        | None, Some usage ->
            begin match usage with
              | Index ->
                  DIAGS.some_result
                    (Cobol_data.Types.Elementary ({typ = Index; level}, None) &@ loc)
              | Pointer _
              | FunctionPointer _
              | ProgramPointer _ ->
                  DIAGS.some_result
                    (Cobol_data.Types.Elementary ({typ = Pointer; level}, None) &@ loc)
              (* | ProgramPointer _ -> *)
              (*     Result.ok @@ Elementary (({typ = Types.Pointer (\* L8 *\); level}, None) &@ loc) *)
              | ObjectReference _ ->
                  DIAGS.some_result
                    (Cobol_data.Types.Elementary ({typ = Object; level}, None) &@ loc)
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
                  DIAGS.some_result
                    (Cobol_data.Types.Elementary ({typ = Numeric; level}, None) &@ loc)
              | UsagePending (`Comp1 | `Comp2 | `BinaryCLong _) ->
                  DIAGS.some_result
                    (Cobol_data.Types.Elementary ({typ = Numeric; level}, None) &@ loc)
              | _ ->
                  DIAGS.error_result None ~loc "Missing@ PICTURE@ clause"
            end
        | Some picture, Some usage ->
            let pic = picture_of_string prog_env picture in
            let cls = DIAGS.map_result ~f:cobol_class_of_picture pic in
            DIAGS.map2_results ~f:begin fun pic (cls: Cobol_data.Types.elementary_data_class) ->
              match usage, cls with
              | (Binary | PackedDecimal |
                 UsagePending (`Comp3 | `Comp5 | `Comp6 | `CompX)),
                (Numeric as cls) ->
                  DIAGS.some_result
                    (Cobol_data.Types.Elementary ({ typ = cls; level },
                                                  Some ~&pic) &@ loc)
              | (Binary | PackedDecimal |
                 UsagePending (`Comp3 | `Comp5 | `Comp6 | `CompX)), _ ->
                  DIAGS.error_result None ~loc:~@data_group
                    "The picture associated with a USAGE clause of type BINARY \
                     (COMP) or PACKED-DECIMAL must be a numeric picture"
              | _ ->
                  DIAGS.some_result
                    (Cobol_data.Types.Elementary ({ typ = cls; level },
                                                  Some ~&pic) &@ loc)
            end pic cls
        | None, None ->
            DIAGS.error_result None
              ~loc "Missing@ USAGE@ or@ PICTURE@ clause@ for@ elementary@ item"
      in
      acc_occurs ~occurs_clause:occurs ~level ~loc element

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
            | Cobol_data.Group.Elementary ({data_item = Data ({ data_clauses;
                                                                _} as data_item);
                                            _ } as element) ->
                let data_item = Data { data_item with
                                       data_clauses = usage :: data_clauses } in
                Cobol_data.Group.Elementary { element with data_item } &@ loc
            | Group ({ elements; _ } as group) ->
                let elements = List.map (prepend_usage usage) elements in
                Cobol_data.Group.Group { group with elements } &@ loc
            | _  ->
                dg
          in
          List.map (prepend_usage usage) dgl
        with Not_found -> dgl
      in
      let elements = of_data_elements config prog_env elements in
      let occurs =
        List.find_map begin function
          | { payload = DataOccurs c; loc } -> Some (c &@ loc)
          | _ -> None
        end dde.data_clauses
      in
      acc_occurs ~occurs_clause:occurs ~level ~loc @@
      DIAGS.map_some_result ~f:begin fun elements ->
        Cobol_data.Types.Group { typ = elements; level } &@ loc
      end elements

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
      let rec check_data_group ~is_first_or_last (data_group: Cobol_data.Group.t) =
        match ~&data_group with
        | Renames _ | Constant _ ->
            DIAGS.Set.none

        | ConditionName _  ->
            DIAGS.Set.error ~loc
              "RENAMES@ may@ not@ reference@ a@ level@ 88."

        | Elementary { data_item = Data data_item; _ }
        | Group { data_item = Data data_item; _ }
          when is_first_or_last &&
               List.exists has_occurs data_item.data_clauses ->
            DIAGS.Set.error ~loc
              "Forbidden@ RENAMES@ of@ data@ item@ with@ OCCURS@ clause."

        | Elementary _
        | Group _
          when is_first_or_last ->
            DIAGS.Set.none

        | Elementary { data_item = Data data_item; _ }
        | Group { data_item = Data data_item; _ }
          when List.exists has_complex_occurs data_item.data_clauses ->
            DIAGS.Set.error ~loc
              "RENAMES@ cannot@ reference@ a@ data@ item@ with@ OCCURS@ \
               DEPENDING@ or@ OCCURS@ DYNAMIC@ clause."

        | Elementary _ ->
            DIAGS.Set.none

        | Group { elements; _ } ->
            check_intermediate_groups_elements elements
      and check_intermediate_groups_elements elements =
            List.fold_left begin fun diags dg ->
              DIAGS.Set.union diags (check_data_group ~is_first_or_last:false dg)
            end DIAGS.Set.none elements
      in
      begin match targets with
        | [hd] ->
            let diags = check_data_group ~is_first_or_last:true hd in
            if DIAGS.Set.has_errors diags
            then
              DIAGS.no_result ~diags
            else
              DIAGS.with_more_diags ~diags @@
              from_item_descrs (module Config) prog_env hd
        | hd :: tl ->
            let tl_rev = List.rev tl in
            let last = List.hd tl_rev in
            let middle = List.rev @@ List.tl tl_rev in
            let diags1 = check_data_group ~is_first_or_last:true hd
            and diags2 = check_intermediate_groups_elements middle
            and diags3 = check_data_group ~is_first_or_last:true last in
            let diags = DIAGS.Set.(union diags1 (union diags2 diags3)) in
            if DIAGS.Set.has_errors diags
            then
              DIAGS.no_result ~diags
            else
              let elements = of_data_elements config prog_env targets in
              DIAGS.with_more_diags ~diags @@
              DIAGS.map_some_result ~f:begin fun elements ->
                Cobol_data.Types.Group { typ = elements; level = 66 } &@ loc
              end elements
        | [] ->
            DIAGS.error_result None ~loc
              "Unable@ to@ type@ RENAMES@ clause:@ empty@ group@ types"
      end
  | Constant _ ->
      DIAGS.error_result None "Not implemented yet: Type of constant."
  | ConditionName {name = _; _} ->
      (* Condition-names are not elementary data items.  Instead, they are
         subordicate to a group. *)
      (* Ok (Elementary (({typ = Conditional; level = 88L }, None) &@<- data_group)) *)
      DIAGS.error_result None "Not implemented yet: Type of condition name"

and of_data_elements config prog_env elements =
  DIAGS.map_result ~f:Option.some @@
  DIAGS.map_result ~f:List.rev @@
  List.fold_left begin fun acc data_group ->
    DIAGS.cons_option_result (from_item_descrs config prog_env data_group) acc
  end (DIAGS.result []) elements

(* --- *)

(* For working-* only for now; to be generalized once we have a proper
   data-group structure. *)
let lookup_working_data_item_descrs = object
  inherit [_] Cobol_ptree.Visitor.folder

  method! fold_working_storage_section _ ({ diags; result } as acc) =
    (* CHECKME: We should only encounter one working-storage section.  If we
       do, then we are traversing a sub-program's working-storage section that
       we can skip. *)
    if result = [] then
      Visitor.do_children_and_then { diags; result = [] }
        rev_and_validate_data_item_descrs
    else
      Visitor.skip acc

  (* TODO: visit other sections (or skip and use other visitor objects for
     sections to be treated differently. *)
  method! fold_linkage_section _ = Visitor.skip (*TODO: This should be handled*)

  (* TODO: Visiting (is/should) be done in order of declarations. So, just
     append to a data group under construction. *)
  method! fold_data_item'
      { loc; payload = { data_level; data_name; data_clauses } }
      { diags; result = acc } =
    let mangle = Cobol_data.Mangling.mangle_data_name ~default_loc:loc in
    let data_item =              (* TODO: no need to mangle at this point *)
      { data_level; data_name = mangle data_name;
        data_clauses (* = convert_data_clauses data_clauses *) } in
    Visitor.proceed @@
    DIAGS.result ~diags ((Data data_item &@ loc) :: acc)

  (* TODO: fold_constant_item' *)
  (* TODO: fold_rename_item' *)
  (* TODO: fold_condition_name_item' *)
  (* TODO: fold_screen_item' *)
  (* TODO: fold_report_group_item' *)
end

let check_data_groups config env working_groups =
  let diags =
    List.fold_left begin fun acc dg ->
      DIAGS.cons_option_result (from_item_descrs config env dg) acc
    end (DIAGS.result []) working_groups |>
    DIAGS.forget_result                   (* NOTE: for now we forget types? *)
  in
  DIAGS.result working_groups ~diags

let working_data_of_compilation_unit' config env root =
  Cobol_ptree.Visitor.fold_compilation_unit'
    lookup_working_data_item_descrs root @@ DIAGS.result [] |>
  DIAGS.more_result ~f:Cobol_data.Group.of_working_item_descrs |>
  DIAGS.more_result ~f:(check_data_groups config env)
