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

(* open Cobol_common.Srcloc.TYPES *)
open Cobol_common.Srcloc.INFIX
(* open Pictured_ast.Data_sections *)
module CharSet = Cobol_common.Basics.CharSet
module DIAGS = Cobol_common.Diagnostics
open Types
open Cobol_ptree

let acc_result f DIAGS.{ result; diags } =
  let module Visitor = Cobol_common.Visitor in
  match f result with
  | Visitor.SkipChildren result ->
      Visitor.SkipChildren (DIAGS.with_more_diags result ~diags)
  | Visitor.DoChildren result ->
      Visitor.DoChildren (DIAGS.with_more_diags result ~diags)
  | Visitor.DoChildrenAndThen (result, f) ->
      Visitor.DoChildrenAndThen (DIAGS.with_more_diags result ~diags, f)

(* TODO: extract this into a Prog_env_initializer module? *)
let initialize_prog_env =
  let module Visitor = Cobol_common.Visitor in
  let valid_picture_symbol = function
    | '0'..'9' | 'a'..'e' | 'A'..'E' | 'N' | 'P' | 'R' | 'S' | 'V'
    | 'X' | 'Z' | 'n' | 'p' | 'r' | 's' | 'v' | 'x' | 'z'
    | '+' | '-' | ',' | '.' | '/' | ';' | '(' | ')' | '='
    | '\'' |'"' | ' ' -> false
    | 'L' | 'G' | 'l' | 'g'           (* <- TODO: Check dialect for those *)
    | _ -> true
  in
  let special_names_clause_folder = object
    inherit [Env.PROG_ENV.t DIAGS.with_diags] Cobol_ptree.Visitor.folder

    method! fold_special_names_clause' { loc; payload = clause } =
      acc_result @@ fun env -> match clause with
      | DecimalPointIsComma ->
          Visitor.skip @@
          DIAGS.result Env.PROG_ENV.{ env with decimal_point = ',' }
      | CurrencySign { sign = (Alphanum (s, _) | National s);
                       picture_symbol = None }
      | CurrencySign { picture_symbol = Some (Alphanum (s, _) | National s); _ }
        when String.length s != 1 ->
          Visitor.skip @@
          DIAGS.error_result env ~loc "%s@ is@ not@ a@ valid@ PICTURE@ symbol." s
      | CurrencySign { sign = Alphanum (s, _) | National s;
                       picture_symbol = None }
      | CurrencySign { picture_symbol = Some (Alphanum (s, _) | National s); _ }
        when not (valid_picture_symbol s.[0]) ->
          Visitor.skip @@
          DIAGS.error_result env ~loc "%c@ is@ not@ a@ valid@ PICTURE@ symbol."
            s.[0]
      | CurrencySign { sign = Alphanum (s, _) | National s;
                       picture_symbol = None }
      | CurrencySign { picture_symbol = Some (Alphanum (s, _) | National s); _ } ->
          Visitor.skip @@
          DIAGS.result { env with
                         currency_signs = CharSet.add s.[0] env.currency_signs }
      | _ ->                       (* TODO: other clauses? *)
          Visitor.proceed @@      (* may report unfinished visitor warnings *)
          DIAGS.result env
  end in
  let ensure_currency_sign_is_defined env =
    (* Currency sign defaults to '$' *)
    if CharSet.is_empty env.Env.PROG_ENV.currency_signs
    then Env.PROG_ENV.{ env with currency_signs = CharSet.singleton '$' }
    else env
  in
  fun env_div base_env ->
    DIAGS.map_result ~f:ensure_currency_sign_is_defined @@
    Cobol_ptree.Visitor.fold_environment_division'_opt
      special_names_clause_folder env_div @@
    DIAGS.result base_env

let try_making_env_of_compilation_unit,
    try_making_env_of_program_unit
  =
  let module Visitor = Cobol_common.Visitor in
  let build_env ?parent_env name env =
    let prog_env = Env.PROG_ENV.make ?parent:parent_env ~&name in
    Visitor.skip @@
    DIAGS.map_result ~f:Option.some (initialize_prog_env env prog_env)
  in
  let env_builder = object
    inherit [(* Env.PROG_ENV.t option DIAGS.with_diags *)_] Cobol_ptree.Visitor.folder
    method! fold_program_unit p =
      acc_result @@ fun parent_env -> build_env p.program_name p.program_env ?parent_env
    method! fold_function_unit f =
      acc_result @@ fun _ -> build_env f.function_name f.function_env
    method! fold_method_definition m =
      acc_result @@ fun _ -> build_env m.method_name m.method_env
    method! fold_class_definition' { payload = c; _ } =
      acc_result @@ fun _ -> build_env c.class_name c.class_env
    method! fold_interface_definition' { payload = i; _ } =
      acc_result @@ fun _ -> build_env i.interface_name i.interface_env
    method! fold_factory_definition _ =
      Visitor.skip                                 (* NOTE: only lacks a name *)
    method! fold_instance_definition _ =
      Visitor.skip                                 (* NOTE: only lacks a name *)
  end in
  (fun cu' ->
     Cobol_ptree.Visitor.fold_compilation_unit' env_builder cu' @@
     DIAGS.result None),
  (fun ~parents pu' ->
     Cobol_ptree.Visitor.fold_program_unit' env_builder pu' @@
     DIAGS.result (match parents with h :: _ -> Some h | [] -> None))



let cobol_class_of_picture: Picture.t with_loc -> Types.elementary_data_class =
  fun { payload = pic; _ } ->
  match pic.category with
  | Alphabetic _ -> Alphabetic
  | Alphanumeric _ -> Alphanumeric
  | Boolean _ -> Boolean
  | National _ -> National
  | FixedNum _ | FloatNum _ -> Numeric

let rec of_data_group
    (module Config: Cobol_config.T) program_environment data_group
  : _ DIAGS.with_diags =

  let picture_of_string Env.PROG_ENV.{ decimal_point;
                                       currency_signs; _ } s =
    let module E = struct
      let decimal_char = decimal_point
      let currency_signs = currency_signs
    end in
    let module PIC = Picture.Make (Config) (E) in
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
          (Table { typ = { elements_type = element;
                           length = Fixed occurs_fixed.times };
                   level } &@ loc)
      | Some element, Some OccursDepending { from; to_; depending; _ } ->
          DIAGS.some_result @@
          (Table { typ = { elements_type = element;
                           length = OccursDepending { min_size = from;
                                                      max_size = to_;
                                                      depending } };
                   level } &@ loc)
      | Some _, Some OccursDynamic _ ->
          DIAGS.warn_result None ~loc
            "Table with dynamic capacity are not implemented yet."
    end element
  in

  match ~&data_group with
  | Group.Elementary {data_item = Data dde; name = _} ->
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
            let pic = picture_of_string program_environment pic in
            DIAGS.map_result ~f:begin fun pic ->
              Some (Elementary ({ typ = cobol_class_of_picture pic; level },
                                Some ~&pic) &@ loc)
            end pic
        | None, Some usage ->
            begin match usage with
              | Index ->
                  DIAGS.some_result (Elementary ({typ = Index; level}, None) &@ loc)
              | Pointer _
              | FunctionPointer _
              | ProgramPointer _ ->
                  DIAGS.some_result (Elementary ({typ = Pointer; level}, None) &@ loc)
              (* | ProgramPointer _ -> *)
              (*     Result.ok @@ Elementary (({typ = Types.Pointer (\* L8 *\); level}, None) &@ loc) *)
              | ObjectReference _ ->
                  DIAGS.some_result (Elementary ({typ = Types.Object; level}, None) &@ loc)
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
                  DIAGS.some_result (Elementary ({typ = Numeric; level}, None) &@ loc)
              | UsagePending (`Comp1 | `Comp2 | `BinaryCLong _) ->
                  DIAGS.some_result (Elementary ({typ = Numeric; level}, None) &@ loc)
              | _ ->
                  DIAGS.error_result None ~loc "Missing@ PICTURE@ clause"
            end
        | Some picture, Some usage ->
            let pic = picture_of_string program_environment picture in
            let cls = DIAGS.map_result ~f:cobol_class_of_picture pic in
            DIAGS.map2_results ~f:begin fun pic (cls: elementary_data_class) ->
              match usage, cls with
              | (Binary | PackedDecimal |
                 UsagePending (`Comp3 | `Comp5 | `Comp6 | `CompX)),
                (Numeric as cls) ->
                  DIAGS.some_result (Elementary ({ typ = cls; level },
                                                 Some ~&pic) &@ loc)
              | (Binary | PackedDecimal |
                 UsagePending (`Comp3 | `Comp5 | `Comp6 | `CompX)), _ ->
                  DIAGS.error_result None ~loc:~@data_group
                    "The picture associated with a USAGE clause of type BINARY \
                     (COMP) or PACKED-DECIMAL must be a numeric picture"
              | _ ->
                  DIAGS.some_result (Elementary ({ typ = cls; level },
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
        DIAGS.map_result ~f:Option.some @@
        DIAGS.map_result ~f:List.rev @@
        List.fold_left begin fun acc data_group ->
          DIAGS.cons_option_result
            (of_data_group (module Config) program_environment data_group)
            acc
        end (DIAGS.result []) elements
      in
      let occurs =
        List.find_map begin function
          | { payload = DataOccurs c; loc } -> Some (c &@ loc)
          | _ -> None
        end dde.data_clauses
      in
      acc_occurs ~occurs_clause:occurs ~level ~loc @@
      DIAGS.map_some_result ~f:begin fun elements ->
        Group { typ = elements; level } &@ loc
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
      let rec check_data_group ~is_first_or_last (data_group: Group.t) =
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
              of_data_group (module Config) program_environment hd
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
              let elements =
                DIAGS.map_result ~f:Option.some @@
                DIAGS.map_result ~f:List.rev @@
                List.fold_left begin fun acc data_group ->
                  DIAGS.cons_option_result
                    (of_data_group (module Config) program_environment data_group)
                    acc
                end (DIAGS.result []) targets
              in
              DIAGS.with_more_diags ~diags @@
              DIAGS.map_some_result ~f:begin fun elements ->
                Group { typ = elements; level = 66 } &@ loc
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
