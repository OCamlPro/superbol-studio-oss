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

(** Type-checking and validation of COBOL compilation groups *)

open Cobol_ast
open Cobol_common.Srcloc.INFIX
module CharSet = Cobol_common.Basics.CharSet
module DIAGS = Cobol_common.Diagnostics
module Visitor = Cobol_common.Visitor
module PTree_visitor = Cobol_parser.PTree_visitor
module CU = Cobol_data.Compilation_unit
module CUs = CU.SET

module Make
    (Config: Cobol_config.T)                      (* for dialect-based checks *)
    (Diags: DIAGS.STATEFUL) =
struct


  open Cobol_parser.PTree

  (* TODO: extract this into a Prog_env_initializer module? *)
  let initialize_prog_env =
    let valid_picture_symbol = function
      | '0'..'9' | 'a'..'e' | 'A'..'E' | 'N' | 'P' | 'R' | 'S' | 'V'
      | 'X' | 'Z' | 'n' | 'p' | 'r' | 's' | 'v' | 'x' | 'z'
      | '+' | '-' | ',' | '.' | '/' | ';' | '(' | ')' | '='
      | '\'' |'"' | ' ' -> false
      | 'L' | 'G' | 'l' | 'g'           (* <- TODO: Check dialect for those *)
      | _ -> true
    in
    let special_names_clause_folder = object
      inherit [Cobol_data.PROG_ENV.t] PTree_visitor.folder

      method! fold_special_names_clause' { loc; payload = clause } env =
        match clause with
        | DecimalPointIsComma ->
            Visitor.skip { env with decimal_point = ',' }
        | CurrencySign { sign = (Alphanum (s, _) | National s);
                         picture_symbol = None }
        | CurrencySign { picture_symbol = Some (Alphanum (s, _) | National s); _ }
          when String.length s != 1 ->
            Diags.error ~loc "%s@ is@ not@ a@ valid@ picture@ symbol." s;
            Visitor.skip env
        | CurrencySign { sign = Alphanum (s, _) | National s;
                         picture_symbol = None }
        | CurrencySign { picture_symbol = Some (Alphanum (s, _) | National s); _ }
          when not (valid_picture_symbol s.[0]) ->
            Diags.error ~loc "%c@ is@ not@ a@ valid@ PICTURE@ symbol." s.[0];
            Visitor.skip env
        | CurrencySign { sign = Alphanum (s, _) | National s;
                         picture_symbol = None }
        | CurrencySign { picture_symbol = Some (Alphanum (s, _) | National s); _ } ->
            Visitor.skip @@
            { env with
              currency_signs = CharSet.add s.[0] env.currency_signs }
        | _ ->                       (* TODO: other clauses? *)
            Visitor.proceed env     (* may report unfinished visitor warnings *)
    end in
    fun env_div base_env ->
      let env =
        PTree_visitor.fold_environment_division'_opt special_names_clause_folder
          env_div base_env
      in
      (* Currency sign defaults to '$' *)
      if CharSet.is_empty env.currency_signs
      then { env with currency_signs = CharSet.singleton '$' }
      else env

  let try_making_env_of_compilation_unit,
      try_making_env_of_program_unit
    =
    let build_env ?parent_env name env =
      let prog_env = Cobol_data.PROG_ENV.make ?parent:parent_env ~&name in
      Visitor.skip @@ Some (initialize_prog_env env prog_env)
    in
    let env_builder = object
      inherit [Cobol_data.PROG_ENV.t option] PTree_visitor.folder
      method! fold_program_unit p parent_env =
        build_env p.program_name p.program_env ?parent_env
      method! fold_function_unit f _ =
        build_env f.function_name f.function_env
      method! fold_method_definition m _ =
        build_env m.method_name m.method_env
      method! fold_class_definition' { payload = c; _ } _ =
        build_env c.class_name c.class_env
      method! fold_interface_definition' { payload = i; _ } _ =
        build_env i.interface_name i.interface_env
      method! fold_factory_definition _ =
        Visitor.skip                               (* NOTE: only lacks a name *)
      method! fold_instance_definition _ =
        Visitor.skip                               (* NOTE: only lacks a name *)
    end in
    (fun cu' ->
       PTree_visitor.fold_compilation_unit' env_builder cu' None),
    (fun ~parents pu' ->
       PTree_visitor.fold_program_unit' env_builder pu'
         (match parents with h :: _ -> Some h | [] -> None))

  let picture_of_string Cobol_data.PROG_ENV.{ decimal_point;
                                              currency_signs; _ } s =
    let module E = struct
      let decimal_char = decimal_point
      let currency_signs = currency_signs
    end in
    let module PIC = Cobol_data.Picture.Make (Config) (E) in
    try PIC.of_string s with
    | PIC.InvalidPicture (str, diags, dummy) ->
        Diags.add_all diags;
        dummy &@<- str


  let validate_data_clauses =
    Cobol_validation.validate_data_clauses (module Diags)

  (* For wss only for now; to be generalized once we have a proper data-group
     structure. *)
  let pictured_data_item_descrs_builder env =
    let open Cobol_data.Pictured_ast in

    let convert_data_clauses clauses =
      PTree_visitor.fold_data_clauses (object
        inherit [_] PTree_visitor.folder
        method! fold_data_clause' c acc =
          let c' = match ~&c with
            | DataPicture { payload = { picture;
                                        picture_locale;
                                        picture_depending }; loc } ->
                let picture_clause =
                  { picture = picture_of_string env picture;
                    picture_locale;
                    picture_depending } in
                Some (DataPicture (picture_clause &@ loc))
            | DataUsage u ->
                Some (DataUsage u)
            | _ ->                             (* TODO: map other data clauses *)
                None
          in
          Visitor.skip (match c' with None -> acc | Some c' -> (c' &@<- c) :: acc)
      end) clauses [] |> List.rev
    in

    let rev_and_validate_data_item_descrs
      : type k. k item_descr with_loc list -> k item_descr with_loc list
      = fun rev_data_item_descrs ->
        (* Associate each item with an `is_elementary` flag during reversal, and
           then map the result again to obtain messages in order of
           declarations. *)

        let _, flagged_data_item_descrs =
          List.fold_left begin fun (next_level, acc) descr ->
            let is_elementary curr_level = curr_level >= next_level in
            match (~&descr: k item_descr) with
            | Data { data_level; _ } when ~&data_level = 77 ->     (* special noncontiguous item *)
                0, (descr, true) :: acc
            | Data { data_level = { payload = l; _ }; _ }
            | Renames { rename_level = {payload = 66 as l; _}; _ }
            | Screen { screen_level = l; _ }
            | ReportGroup { report_level = l; _ } ->
                l, (descr, is_elementary l) :: acc
            | Constant _ ->
                0, (descr, false) :: acc
            | Renames _ ->                                  (* force level 66- *)
                66, (descr, false) :: acc
            | CondName _ ->
                88, (descr, false (* CHECKME: ??? *)) :: acc
          end (0, []) rev_data_item_descrs
        in

        List.map begin fun (descr, is_elementary) ->
          match (~&descr: k item_descr) with
          | Data { data_level = l; _ } when ~&l < 1 || (~&l > 49 && ~&l <> 77) ->
              Diags.error ~loc:~@l "Invalid@ level@ %d@ for@ data@ item" ~&l;
              descr
          | Renames { rename_level = {payload = l; _}; _ } when l <> 66 ->
              Diags.warn ~loc:~@descr "RENAMES@ data@ item@ should@ be@ at@ level@ 66";
              descr
          | Data item ->
              validate_data_clauses ~is_elementary (item &@<- descr);
              descr
          | _ ->                                                       (* TODO *)
              descr
        end flagged_data_item_descrs
    in

    object
      inherit [_] PTree_visitor.folder

      method! fold_working_storage_section _ acc =
        (* We should only encounter one working-storage section.  If we do, then
           we are traversing a sub-program's working-storage section that we can
           skip. *)
        if acc = []
        then Visitor.do_children_and_then [] rev_and_validate_data_item_descrs
        else Visitor.skip acc

      (* TODO: visit other sections (or skip and use other visitor objects for
         sections to be treated differently. *)
      method! fold_linkage_section _ = Visitor.skip (*TODO: This should be handled*)

      (* TODO: Visiting (is/should) be done in order of declarations. So, just
         append to a data group under construction. *)
      method! fold_data_item' { loc; payload = { data_level;
                                                  data_name;
                                                  data_clauses } } acc =
        let mangle = Cobol_data.Mangling.mangle_data_name ~default_loc:loc in
        let data_item =              (* TODO: no need to mangle at this point *)
          { data_level; data_name = mangle data_name;
            data_clauses = convert_data_clauses data_clauses } in
        Visitor.proceed ((Data data_item &@ loc) :: acc)

      (* TODO: fold_constant_item' *)
      (* TODO: fold_rename_item' *)
      (* TODO: fold_condition_name_item' *)
      (* TODO: fold_screen_item' *)
      (* TODO: fold_report_group_item' *)
    end

  let data_of_compilation_unit',
      data_of_program_unit'
    =
    let data_group_of_wss env =
      let check_data_group dg =
        Cobol_data.Typing.of_data_group (module Diags) env dg
      in
      fun wss ->
        (*TODO: Better*)
        match Cobol_data.Group.of_working_storage (module Diags) wss with
        | Ok data_groups ->
            List.filter_map
              (fun dg -> match check_data_group dg with
                 | Ok _ -> Some dg
                 | Error _ -> None) data_groups
        | Error _ -> []
    in
    let data_of_compilation_unit' env u =
      let data_items_checker = pictured_data_item_descrs_builder env in
      let wss = PTree_visitor.fold_compilation_unit' data_items_checker u [] in
      data_group_of_wss env wss
    and data_of_program_unit' env p =
      let data_items_checker = pictured_data_item_descrs_builder env in
      let wss = PTree_visitor.fold_program_unit' data_items_checker p [] in
      data_group_of_wss env wss
    in
    data_of_compilation_unit',
    data_of_program_unit'

  let name_of_compilation_unit = function
    | Program {program_name = name; _}
    | Function {function_name = name; _}
    | ClassDefinition {class_name = name; _}
    | InterfaceDefinition {interface_name = name; _} -> ~&name

  (** Main entry for verification of compilation groups.  This function checks
      full groups and and builds their associated internal representation. *)
  let typeck_compilation_group (compilation_group: compilation_group) =

    let visitor = object
      inherit [Cobol_data.PROG_ENV.t list * CUs.t] PTree_visitor.folder

      method! fold_compilation_unit' cu ((parents, progs) as acc) =
        match ~&cu with
        | Program _ ->
            (* Dealt with in `fold_program_unit'` below *)
            Visitor.do_children acc
        | Function _ | ClassDefinition _ | InterfaceDefinition _ ->
            match try_making_env_of_compilation_unit cu with
            | None ->
                Visitor.skip acc
            | Some cu_env ->
                (* TODO (++): here we built an env for `u`.  Transmit it to
                   another visitor to build the associated data-groups
                   directly (no need to bother with the "pictured"
                   representation; same for the representation of the
                   procedure division. *)
                let cu_name = name_of_compilation_unit ~&cu
                and cu_wss = data_of_compilation_unit' cu_env cu in
                let prog = CU.{ cu_name; cu_loc = ~@cu; cu_env; cu_wss } in
                Visitor.skip @@ (parents, CUs.add prog progs)

      method! fold_program_unit' pu ((parents, progs) as acc) =
        match try_making_env_of_program_unit ~parents pu with
        | None ->
            Visitor.proceed acc
        | Some cu_env ->
            (* TODO: same as (++) above. *)
            (* NB: For now we create an standalone CU (note the "C"), but we
               should register nested PUs as children of their parents PUs
               instead. *)
            let cu_name = ~&(~&pu.program_name)
            and cu_wss = data_of_program_unit' cu_env pu in
            let prog = CU.{ cu_name; cu_loc = ~@pu; cu_env; cu_wss } in
            let acc = cu_env :: parents, CUs.add prog progs in
            (* Proceed with potential nested progs, and then restore stack of
               parents: *)
            Visitor.do_children_and_then acc (fun (_, progs) -> parents, progs)

      (* skip some divisions *)
      method! fold_environment_division _ = Visitor.skip
      method! fold_data_division' _ = Visitor.skip
      method! fold_procedure_division' _ = Visitor.skip
    end in

    PTree_visitor.fold_compilation_group visitor compilation_group
      ([], CUs.empty) |> snd

(*
  let data_groups = DataGroup.of_working_storage (module Diags) mangled_wss in

  Fmt.pr "%a" Fmt.(result ~ok:DataGroup.pp_data_group_list ~error:(any "Error")) data_groups;

  let rec add_data_item qualname env data_group =
    match data_group with
    | DataGroup.Elementary {name; _} ->
        let qualname = Qual (name, qualname) in
        let data_item = CobolEnv.DATA_ITEM.make name in
        { env with
          data_items = QualMap.add qualname data_item env.data_items; }
    | Group {name; elements; _} ->
        let qualname = Qual (name, qualname) in
        let data_item = CobolEnv.DATA_ITEM.make name in
        let env =
          { env with
            data_items = QualMap.add qualname data_item env.data_items; }
        in
        List.fold_left (add_data_item qualname) env elements
    | Renames {name; targets} ->
        let targets =
          List.fold_left (fun acc target ->
              let target_name = DataGroup.name_of target in
              Qual (target_name, qualname)::acc)
            []
            targets
        in
        let data_item = CobolEnv.DATA_ITEM.make name in
        let data_item =
          {data_item with
           renames = targets;}
        in
        let first_level_name = QualMap.QUAL_NAME.get_highest_level qualname in
        let qn = Qual (name, Name first_level_name) in
        { env with
          data_items = QualMap.add qn data_item env.data_items; }
    | ConditionName {name; values; target;} ->
        let target =
          Qual (DataGroup.name_of target, qualname)
        in
        let (condition: CobolEnv.DATA_ITEM.condition option) = Some {target; values;} in
        let cond_name = Qual (name, qualname) in
        let data_item = CobolEnv.DATA_ITEM.make name in
        let data_item =
          {data_item with
           condition;}
        in
        { env with
          data_items = QualMap.add cond_name data_item env.data_items; }
    | Constant _ ->
        let loc = DataGroup.name_location data_group in
        Diags.error ~loc "These should be top level.";
        env
  in

  let env =
    Result.fold
      ~error:(fun _ -> env)
      ~ok:(fun data_groups ->
          List.fold_left (fun env data_group ->
              match data_group with
              | DataGroup.Elementary {name; _} ->
                  let qualname = (Name name: qualname) in
                  let data_item = CobolEnv.DATA_ITEM.make name in
                  { env with
                    data_items = QualMap.add qualname data_item env.data_items; }
              | Group {name; elements; _} ->
                  let qualname = (Name name: qualname) in
                  let data_item = CobolEnv.DATA_ITEM.make name in
                  let env =
                    { env with
                      data_items = QualMap.add qualname data_item env.data_items; }
                  in
                  List.fold_left (add_data_item qualname) env elements
              | Constant {name; value;} ->
                  let qualname = (Name name: qualname) in
                  let data_item = CobolEnv.DATA_ITEM.make name in
                  let data_item =
                    {data_item with
                      constant = Some value;}
                  in
                  {env with
                    data_items = QualMap.add qualname data_item env.data_items; }
              | ConditionName {name; values; target;} ->
                  let target =
                    (Name (DataGroup.name_of target): qualname)
                  in
                  let (condition: CobolEnv.DATA_ITEM.condition option) = Some {target; values;} in
                  let cond_name = (Name name: qualname) in
                  let data_item = CobolEnv.DATA_ITEM.make name in
                  let data_item =
                    {data_item with
                     condition;}
                  in
                  { env with
                    data_items = QualMap.add cond_name data_item env.data_items; }
              | _ ->
                  let loc = ~@(DataGroup.name_of data_group) in
                  Diags.error ~loc "These should not be top level.";
                  env
            )
            env
            data_groups)
      data_groups
  in

  (* REDEFINES must be directly after the group they redefine (modulo other
   * REDEFINES), and must be of the same level number.
   * If concerning level 01 item then can be of bigger length otherwise must be
   * smaller or of same size.
   * The redefined item cannot be qualified as the placement of the REDEFINES
   * does not allow for ambiguity. *)
  (* let _redefines =
    let has_redefines = List.exists (function
        | {payload = PicAST.Data_section.Redefines _; _} -> true
        | _ -> false)
    in
    let rec get_redefines qualname redefines data_group =
      match data_group with
      | DataGroup.Elementary {data_desc; _} ->
          if has_redefines ~&data_desc.data_description_clauses then
            (qualname, data_desc)::redefines
          else
            redefines
      | Group {name; elements; data_desc} ->
          let redefines =
            if has_redefines ~&data_desc.data_description_clauses then
              (qualname, data_desc)::redefines
            else
              redefines
          in
          let new_qualname =
            Option.fold
              ~none:(Some (Name (name &@<- data_desc): qualname))
              ~some:(fun qn -> Some (Qual (name &@<- data_desc, qn)))
              qualname
          in
          List.fold_left (get_redefines new_qualname) redefines elements
    in
    List.fold_left (fun redefines data_group ->
        match data_group with
        | DataGroup.Elementary {data_desc; _} ->
            if has_redefines ~&data_desc.data_description_clauses then
              (None, data_desc)::redefines
            else
              redefines
        | Group {name; elements; data_desc} ->
            let redefines =
              if has_redefines ~&data_desc.data_description_clauses then
                (None, data_desc)::redefines
              else
                redefines
            in
            List.fold_left (get_redefines ( Some (Name (name &@<- data_desc))))
              redefines
              elements)
      []
  in
*)
  env
*)

  (* let data_types =
    List.map (fun (prog_name, data_groups) ->
        let prog_path = List.assoc prog_name prog_paths in
        let prog_env = prog_env_of_path prog_name prog_path environment in
        prog_name,
        List.map begin function
          | (DataGroup.Elementary (name, _) | Group (name, _, _)) as dg ->
              name, Result.get_ok @@ TYPING.of_data_group (module Diags: Diagnostics.STATEFUL) prog_env dg, dg
        end data_groups)
      data_groups
  in

  (* Adds type name and location to every data item in the environment*)
  let environment =
    ENV.mapi (fun prog_name prog_env ->
        let data_items_funs =
          {DATA_ITEM.fm_default with
           data_item = (fun funs (name, typ, dg) data_item ->
               (*The global clause only concerns the level 01 of the items. *)
               let level1_name, dde = match dg with
                 | DataGroup.Elementary (name, dde)
                 | Group (name, _, dde) -> name, dde
               in
               let is_global =
                 (* TODO: extract this as a public predicate with a proper name *)
                 name = level1_name &&
                 List.exists begin Cobol_common.Srcloc.payload >> function
                   | (Global: PicAST.Data_section.data_description_clause) -> true
                   | _ -> false
                 end ~&dde.data_description_clauses
               in
               let sub_items = match typ with
                 | Types.Elementary _
                 | Table { payload = { typ = {elements_type = Elementary _; _}; _}; _ } ->
                     StringMap.empty
                 | Group { payload = { typ = fields; _}; _ }
                 | Table { payload = { typ = {
                       elements_type = Group { payload = {typ = fields; _}; _ }; _}; _;
                     }; _ } ->
                     List.fold_left
                       (fun sub_items ({field_type; field_name}) ->
                          let data_item, _ =
                            DATA_ITEM.fm_data_item
                              funs
                              (field_name, field_type, dg)
                              DATA_ITEM.empty
                          in
                          StringMap.add field_name data_item sub_items)
                       StringMap.empty
                       fields
                 | _ ->
                     failwith
                       "A table cannot directly contain a table type."
               in
               {data_item with
                sub_items;
                data_name = Some name;
                loc = Some(Types.loc_of typ);
                data_type = Some typ;
                level_number = Types.level_of typ;
                global = is_global;
               }, (name, typ, dg));
          }
        in
        PROG_ENV.fm_prog_env
          {PROG_ENV.fm_default with
           prog_env = (fun funs (prog_name, data_types) prog_env ->
               let prog_dg = List.assoc prog_name data_types in
               let data_items = List.fold_left (fun data_items (name, typ, dg) ->
                   let data_item =
                     DATA_ITEM.fm_data_item
                       data_items_funs
                       (name, typ, dg)
                       DATA_ITEM.empty
                     |> fst
                   in
                   StringMap.add name data_item data_items)
                   StringMap.empty
                   prog_dg
               in
               let nested_progs =
                 StringMap.mapi (fun prog_name prog_env ->
                     PROG_ENV.prog_env funs (prog_name, data_types) prog_env
                     |> fst)
                   prog_env.nested_progs
               in
               {prog_env with
                data_items;
                nested_progs}, (prog_name, data_types));
          }
          (prog_name, data_types)
          prog_env
        |> fst)
      environment
  in

  (* Calculate the data items size*)
  let environment =
    ENV.map (fun prog_env ->
        let data_item_funs =
          {DATA_ITEM.fm_default with
           data_type = (fun _ (size, _) data_type ->
               data_type,
               match data_type with
               | Some (Elementary { payload = _, Some pic; _ }) ->
                   size + pic.size, None
               | Some (Table { payload = {
                   typ = { elements_type = Elementary { payload = _, Some pic; _ };
                           length; }; _; }; _ }) ->
                   let pic_size = pic.size in
                   let length =
                     Int64.to_int @@ match length with
                     | Fixed l -> l
                     | OccursDepending {max_size; _} ->
                         max_size
                   in
                   size + (length * pic_size), None
               | Some (Table { payload = { typ = { length; _}; _}; _ }) ->
                   let length =
                     Int64.to_int @@ match length with
                     | Fixed l -> l
                     | OccursDepending {max_size; _} ->
                         max_size
                   in
                   size, Some length
               | _ -> size, None);
           sub_items = (fun funs (size, tab_length) sub_items ->
               let sub_items, sub_size, _ =
                 StringMap.fold
                   (fun data_name data_item (sub_items, size, tab_length) ->
                      let sub_item, (sub_size, _) =
                        DATA_ITEM.fm_data_item funs (0, None) data_item
                      in
                      let tab_mult = Option.value ~default:1 tab_length in
                      StringMap.add data_name sub_item sub_items,
                      (tab_mult * sub_size) + size,
                      tab_length)
                   sub_items
                   (StringMap.empty, 0, tab_length)
               in
               sub_items, (size + sub_size, tab_length));
           data_size = (fun _ (size, _) _ ->
               size, (size, None))
          }
        in
        PROG_ENV.fm_prog_env
          {PROG_ENV.fm_default with
           data_items = (fun _ _ data_items ->
               StringMap.map (fun data_item ->
                   DATA_ITEM.fm_data_item data_item_funs (0, None) data_item
                   |> fst)
                 data_items,
               ());
           nested_progs = (fun funs _ nested_progs ->
               StringMap.map (fun nested_prog ->
                   PROG_ENV.fm_prog_env funs () nested_prog
                   |> fst)
                 nested_progs,
               ());
          }
          ()
          prog_env
        |> fst)
      environment
  in

  (* We add the data items from a containing program to its nested programs
   * TODO: Add a flag in prog_env saying that the item is from upper level
   * program. *)
  let environment =
    ENV.map
      begin
        PROG_ENV.fm_prog_env
          {PROG_ENV.fm_default with
           prog_env = (fun funs acc prog_env ->
               let data_items, acc =
                 PROG_ENV.data_items funs acc prog_env.data_items
               in
               let nested_progs, acc =
                 PROG_ENV.nested_progs funs acc prog_env.nested_progs
               in
               {prog_env with
                data_items;
                nested_progs;
               }, acc);
           data_items = (fun _ acc data_items ->
               let union =
                 StringMap.merge (fun _ (e1: DATA_ITEM.t option) e2 ->
                     match e1,e2 with
                     | None, None -> None
                     | Some e1, None ->
                         if e1.global then
                           Some e1
                         else
                           None
                     | Some _, Some e2
                     | None, Some e2 -> Some e2)
                   acc
                   data_items
               in
               union, union);
           nested_progs = (fun funs acc nested_progs ->
               StringMap.map (fun nested_prog ->
                   PROG_ENV.fm_prog_env funs acc nested_prog
                   |> fst)
                 nested_progs, acc)
          }
          StringMap.empty
        >> fst
      end
      environment
  in

  (* Add toplevel items and warn about qualification *)
  let environment =
    let data_item_funs =
      {DATA_ITEM.fm_default with
       data_name = (fun _ ((prog_env: PROG_ENV.t), removed) data_name ->
           match data_name with
           | Some name ->
               if StringSet.mem name prog_env.toplevel_items then
                 data_name,
                 ({prog_env with
                   toplevel_items =
                     StringSet.remove name prog_env.toplevel_items;
                  },
                  StringSet.add name removed)
               else if StringSet.mem name removed then
                 data_name, (prog_env, removed)
               else
                 data_name,
                 ({prog_env with
                   toplevel_items =
                     StringSet.add name prog_env.toplevel_items;
                  },
                  removed)
           | None ->
               failwith "The data name should be mangled at this point.");
       sub_items = (fun funs acc sub_items ->
           StringMap.fold (fun key sub_item (sub_items, acc) ->
               let sub_item, acc = DATA_ITEM.fm_data_item funs acc sub_item in
               StringMap.add key sub_item sub_items, acc)
             sub_items
             (StringMap.empty, acc));
      }
    in

    ENV.map
      (fun prog_env ->
         PROG_ENV.fm_prog_env
           {PROG_ENV.fm_default with
            data_items = (fun _ (prog_env, _) data_items ->
                let prog_env, removed =
                  StringMap.fold (fun _ data_item (prog_env, removed) ->
                      let prog_env, removed =
                        DATA_ITEM.fm_data_item
                          data_item_funs
                          (prog_env, removed)
                          data_item
                        |> snd
                      in
                      prog_env, removed)
                    data_items
                    (prog_env, StringSet.empty)
                in
                data_items, (prog_env, removed))
           }
           (prog_env, StringSet.empty)
           prog_env
         |> Pair.map_snd
           ~f:(Pair.map_snd
                 ~f:(StringSet.iter @@ Diags.warn
                       "Data-name@ `%s'@ is@ bound@ to@ several@ items;@ use@ of@ \
                        qualification@ is@ recommended"))
         |> fst)
      environment
  in

  (* Add data item paths *)
  let data_item_funs =
    {DATA_ITEM.fm_default with
     data_name = (fun _ (parent_path, _, sub_item_paths, _) data_name ->
         let name = match data_name with
           | Some name -> name
           | None -> failwith "All the data names should be mangled"
         in
         let sub_item_paths =
           match parent_path with
           | [] ->
               sub_item_paths
           | _ ->
               StringMap.add name parent_path sub_item_paths
         in
         data_name, (parent_path, name, sub_item_paths, StringMap.empty));
     sub_items =
       (fun funs (parent_path, curr_name, sub_item_paths, _) sub_items ->
          let sub_items, new_sub_items_paths =
            StringMap.fold
              (fun sub_name sub_item (sub_items, sub_items_paths) ->
                 let sub_item, (_, _, sub_item_paths, _) =
                   DATA_ITEM.fm_data_item
                     funs
                     ([curr_name], "", sub_items_paths, sub_items_paths)
                     sub_item
                 in
                 StringMap.add sub_name sub_item sub_items, sub_item_paths)
              sub_items
              (StringMap.empty, StringMap.empty)
          in
          let sub_item_paths =
            StringMap.merge (fun _ path sub_item_path ->
                match path, sub_item_path with
                | None, None -> None
                | Some e1, None -> Some e1
                | None, Some e2 -> Some ((List.rev parent_path)@e2)
                | Some _, Some _ -> None)
              sub_item_paths
              new_sub_items_paths
          in
          sub_items,
          (parent_path, curr_name, sub_item_paths, new_sub_items_paths));
     sub_items_paths = (fun _ (parent_path, name, paths, sub_item_paths) _ ->
         let sub_item_paths = StringMap.filter_map (fun _ path ->
             match path with
             | [] -> failwith "Look into it"
             | _::[] -> None
             | _::parent -> Some parent)
             sub_item_paths
         in
         sub_item_paths, (parent_path, name, paths, sub_item_paths));
    }
  in

  let environment =
    ENV.map
      begin
        PROG_ENV.fm_prog_env
          {PROG_ENV.fm_default with
           prog_env = (fun _ _ prog_env ->
               let data_items, data_item_paths =
                 StringMap.fold
                   (fun data_name data_item (data_items, paths) ->
                      let data_item, (_, _, paths, _) =
                        DATA_ITEM.fm_data_item
                          data_item_funs
                          ([], "", paths, StringMap.empty)
                          data_item
                      in
                      StringMap.add data_name data_item data_items, paths)
                   prog_env.data_items
                   (StringMap.empty, StringMap.empty)
               in
               {prog_env with
                data_items;
                data_item_paths;
               }, ())
          }
          ()
        >> fst
      end
      environment
  in

  let rec replace_from_path (env: PROG_ENV.t ENV.t) path new_prog_env =
    match path with
    | prog_name::[] -> ENV.replace prog_name new_prog_env env
    | parent_name::rest ->
        let parent_env = match ENV.find_opt parent_name env with
          | Some e -> e
          | None -> Fmt.failwith "Cannot find environment for %s" parent_name (* again???? *)
        in
        let nested_progs =
          replace_from_path parent_env.nested_progs rest new_prog_env
        in
        ENV.replace parent_name {parent_env with nested_progs} env
    | [] -> failwith"The path to the program environment must not be empty" (* argh.... *)
  in

  (* Add redefines *)
  (*TODO: Add redefines validation, ie: They are at the same level as the
          redefined item and their size can be bigger only if they concern
          level 1 items. *)
  let environment =
    (* TODO: Stil way too long; factorize all of this. *)
    List.fold_left (fun env (prog_name, redef_list) ->
        let prog_env_path = List.assoc prog_name prog_paths in
        let prog_env = prog_env_of_path prog_name prog_env_path env in
        let prog_env =
          let open PicAST in
          let lookup_name (dde: Data_section.dde with_loc) =
            match ~&(~&dde.data_name) with
            | Some (Name name) -> ~&name
            (* TODO: get rid of this failwith *)
            | _ -> failwith "All name should be mangled"
          and lookup_redefine dde =
            let redef =
              List.find begin function
                | { payload = Data_section.Redefines _; _ } -> true
                | _ -> false
              end dde
            in
            match redef with
            | { payload = Redefines rd; _ } -> Some ~&rd
            (* TODO: get rid of this failwith *)
            | _ -> failwith "This must be a redefine"
          and replace_subitem name sub_item
              ({ sub_items; _ } as data_item: DATA_ITEM.t) =
            { data_item with sub_items = StringMap.replace name sub_item sub_items }
          in
          List.fold_left begin fun prog_env (path, redef_dde) ->
            let data_item_funs =
              {DATA_ITEM.fm_default with
               data_item =
                 fun funs (path, (redef_dde: Data_section.dde with_loc)) data_item ->
                   match path with
                   | name :: tl ->
                       let sub_item, _ =
                         StringMap.find name data_item.sub_items
                         |> DATA_ITEM.fm_data_item funs (tl, redef_dde)
                       in
                       replace_subitem name sub_item data_item, (path, redef_dde)
                   | [] ->
                       let name = lookup_name redef_dde in
                       let sub_item =
                         { (StringMap.find name data_item.sub_items) with
                           redefines =
                             lookup_redefine ~&redef_dde.data_description_clauses }
                       in
                       replace_subitem name sub_item data_item, (path, redef_dde)
              }
            in
            fst @@ PROG_ENV.fm_prog_env
              {PROG_ENV.fm_default with
               data_items = fun _ _ data_items ->
                 match path with
                 | name :: tl ->
                     let data_item, _ =
                       StringMap.find name data_items
                       |> DATA_ITEM.fm_data_item data_item_funs (tl, redef_dde)
                     in
                     StringMap.replace name data_item data_items, ()
                 | [] ->
                     let name = lookup_name redef_dde in
                     let data_item =
                       { (StringMap.find name data_items) with
                         redefines =
                           lookup_redefine ~&redef_dde.data_description_clauses }
                     in
                     StringMap.replace name data_item data_items, ()
              }
              ()
              prog_env
          end prog_env redef_list
        in
        replace_from_path env (prog_env_path@[prog_name]) prog_env)
      environment
      redefines
  in

  (* Get the type of renames entry. *)
  let renames_types =
    List.map (fun (prog_name, renames_list) ->
        let prog_env_path = List.assoc prog_name prog_paths in
        let prog_env = prog_env_of_path prog_name prog_env_path environment in
        prog_name,
        List.fold_left
          (fun acc (dde_before, (rename: rename_entry with_loc)) ->
             let dde_before =
               DataGroup.of_data_description_entries dde_before
               |>begin function
                 | hd::[] -> hd
                 | _ -> failwith "There must be only one data group before a rename"
               end
             in
             (dde_before,
              ~&rename.data_name,
              TYPING.of_rename_entry prog_env dde_before rename,
              rename)::acc)
          []
          renames_list
        |> List.rev)
      renames
  in

  (* Add the renames entry to the environment. *)
  let environment =
    List.fold_left (fun env (prog_name, renames_types) ->
        let prog_env_path = List.assoc prog_name prog_paths in
        let prog_env = prog_env_of_path prog_name prog_env_path env in
        List.fold_left
          (fun (prog_env: PROG_ENV.t) (dde_before, name, typ, renames) ->
             let renames_range = match typ with
               | Elementary _ ->
                   [~&renames.renamed_item]
               | Group g ->
                   List.map (fun f -> (Name (f.field_name &@<- g): qualname)) (~&g).typ
               | _ ->
                   failwith "RENAMES cannot be a table"
             in
             let name = ~&name in
             let data_item =
               {DATA_ITEM.empty with
                data_name = Some name;
                data_type = Some typ;
                loc = Some (~@renames);
                renames = renames_range;
               }
             in
             let DataGroup.Elementary (level1_name, _)
               | Group (level1_name, _, _) =
               dde_before
             in
             let level1_item = StringMap.find level1_name prog_env.data_items in
             let new_level1 =
               {level1_item with
                sub_items = StringMap.add name data_item level1_item.sub_items
               }
             in
             {prog_env with
              data_items =
                StringMap.replace level1_name new_level1 prog_env.data_items;
              toplevel_items =
                if StringSet.mem name prog_env.toplevel_items then
                  StringSet.remove name prog_env.toplevel_items
                else
                  StringSet.add name prog_env.toplevel_items;
              data_item_paths =
                StringMap.add name [level1_name] prog_env.data_item_paths;
             })
          prog_env
          renames_types
        |> replace_from_path env (prog_env_path@[prog_name]))
      environment
      renames_types
  in

  (* Add the condition names to the environment *)
  let _environment =
    List.fold_left
      (fun env (prog_name, cond_entry_list) ->
         let prog_env_path = List.assoc prog_name prog_paths in
         let prog_env = prog_env_of_path prog_name prog_env_path env in
         List.fold_left
           (fun (prog_env: PROG_ENV.t) (dde_before, cond_entry) ->
              let data_group =
                match DataGroup.of_data_description_entries dde_before with
                | hd::[] -> hd
                | _ ->
                    failwith
                      "There must be only one data group before a condition name"
              in
              let last_item_path = DataGroup.last_item_path data_group in
              let name = ~&(~&cond_entry.condition_name) in
              let cond_entry_item =
                {DATA_ITEM.empty with
                 data_name = Some name;
                 loc = Some ~@cond_entry;
                 data_type = Some (Types.Elementary (({
                     typ = Conditional;
                     level = 88L;
                   }, None) &@<- cond_entry));
                }
              in
              let add_renames_funs =
                {DATA_ITEM.fm_default with
                 data_item = (fun funs (path, cond_name, cond_entry) data_item ->
                     match path with
                     | [] ->
                         {data_item with
                          sub_items =
                            StringMap.add cond_name cond_entry data_item.sub_items;
                         },
                         (path, cond_name, cond_entry)
                     | hd::tl ->
                         (* As the conditional variables do not contains any
                            data but are just a filler for a test, we do not
                            need to change the type of the data item it is contained
                            in. *)
                         let sub_item =
                           DATA_ITEM.fm_data_item
                             funs
                             (tl, cond_name, cond_entry)
                           @@ StringMap.find hd data_item.sub_items
                           |> fst
                         in
                         {data_item with
                          sub_items =
                            StringMap.replace hd sub_item data_item.sub_items;
                         },
                         (path, cond_name, cond_entry));
                }
              in
              let level1_name = List.hd last_item_path in
              let new_data_item =
                DATA_ITEM.fm_data_item
                  add_renames_funs
                  (List.tl last_item_path, name, cond_entry_item)
                @@ StringMap.find level1_name prog_env.data_items
                |> fst
              in
              {prog_env with
               data_items =
                 StringMap.replace level1_name new_data_item prog_env.data_items;
              })
           prog_env
           cond_entry_list
         |> replace_from_path env (prog_env_path@[prog_name]))
      environment
      cond_names
  in

  (*TODO(emilien): Learn to build pretty printer*)
  (*IGNORE ME*)
  let data_item_printer =
    { DATA_ITEM.fm_default with
      data_name =
        (fun _funs _ data_name ->
           let dn = match data_name with
             | Some dn -> dn
             | None -> "None"
           in
           data_name, (dn, []));
      sub_items = (fun funs acc sub_items ->
          let sub_items = StringMap.map (fun item ->
              fst (DATA_ITEM.fm_data_item funs ("", []) item))
              sub_items in
          sub_items, acc);
      renames = (fun _funs (dn, _) renames ->
          renames, (dn, renames));
      redefines = (fun _funs (dn, renames) redefines ->
          let redef = match redefines with
            | None -> "None"
            | Some redef -> redef
          in
          Format.fprintf
            Format.std_formatter
            "(Item: %s; Redefines: %s; Renames: %s;)\n"
            dn
            redef
            ([%derive.show: qualname list] renames);
          redefines, (dn, renames))
    }
  in

  let _prog_env_printer =
    let open Format in
    {PROG_ENV.fm_default with
     name = (fun _ acc name ->
         for _ = 0 to (2 * acc) do
           printf " "
         done;
         printf "Program ID: %a\n" (pp_print_option pp_print_string) name;
         name, acc);
     nested_progs = (fun funs acc nested_progs ->
         if not @@ StringMap.is_empty nested_progs then
           begin
             for _ = 0 to (2 * acc) do
               printf " "
             done;
             printf "with nested programs:\n"
           end;
         StringMap.iter (fun _ nested_env ->
             ignore @@ PROG_ENV.prog_env funs (acc + 1) nested_env)
           nested_progs;
         nested_progs, acc);
     data_items = (fun _ acc data_items ->
         StringMap.iter (fun _ data_item ->
             ignore
             @@ DATA_ITEM.fm_data_item data_item_printer ("", []) data_item)
           data_items;
         data_items, acc)
    }
  in
  Ok (file_env, Cobol_common.Diagnostics.Set.none)
*)
end

let analyze_compilation_group ?(config = Cobol_config.default)
    (type m) : m Cobol_parser.parsed_compilation_group -> _ =

  let analyze_cg (module Diags: DIAGS.STATEFUL) cg =
    let module Typeck = Make (val config) (Diags) in
    Ok (Typeck.typeck_compilation_group cg, DIAGS.Set.none)
  in
  function
  | { parsed_output = Only None | WithTokens (None, _, _);
      parsed_diags; _ } ->
      Error parsed_diags
  | { parsed_output = Only Some cg | WithTokens (Some cg, _, _);
      parsed_diags; _ } ->
      match Cobol_common.catch_diagnostics analyze_cg cg with
      | Ok (res, diags) ->
          Ok (res, cg, DIAGS.Set.union parsed_diags diags)
      | Error diags ->
          Error (DIAGS.Set.union parsed_diags diags)
