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

open Cobol_common.Srcloc.INFIX
module CharSet = Cobol_common.Basics.CharSet
module DIAGS = Cobol_common.Diagnostics
module CU = Cobol_data.Compilation_unit
module CUs = CU.SET

module Make (Config: Cobol_config.T) =            (* for dialect-based checks *)
struct

  open Cobol_ptree
  module Visitor = Cobol_common.Visitor

  (* For wss only for now; to be generalized once we have a proper data-group
     structure. *)
  let data_item_descrs_checker =

    let rev_and_validate_data_item_descrs
        (type k) : k item_descr with_loc list DIAGS.with_diags -> _ =
      fun { result = rev_data_item_descrs; diags } ->
        (* Associate each item with an `is_elementary` flag during reversal, and
           then map the result again to obtain messages in order of
           declarations. *)

        let _, flagged_data_item_descrs =
          List.fold_left begin fun (next_level, acc) descr ->
            let is_elementary curr_level = curr_level >= next_level in
            match (~&descr: k item_descr) with
            | Data { data_level; _ } when ~&data_level = 77 ->
                0, (descr, true) :: acc          (* special noncontiguous item *)
            | Data { data_level = { payload = l; _ }; _ }
            | Renames { rename_level = {payload = 66 as l; _}; _ }
            | Screen { screen_level = l; _ }
            | ReportGroup { report_level = l; _ } ->
                l, (descr, is_elementary l) :: acc
            | Constant _ ->
                0, (descr, false) :: acc
            | Renames _ ->                                   (* force level 66 *)
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
            | _ ->                                                       (* TODO *)
                diags,
                descr
          end diags flagged_data_item_descrs
        in
        DIAGS.result res ~diags
    in

    object
      inherit [_] Cobol_ptree.Visitor.folder

      method! fold_working_storage_section _ (DIAGS.{ diags; result } as acc) =
        (* CHECKME: We should only encounter one working-storage section.  If we
           do, then we are traversing a sub-program's working-storage section
           that we can skip. *)
        if result = [] then
          Visitor.do_children_and_then
            DIAGS.{ diags; result = [] }
            rev_and_validate_data_item_descrs
        else Visitor.skip acc

      (* TODO: visit other sections (or skip and use other visitor objects for
         sections to be treated differently. *)
      method! fold_linkage_section _ = Visitor.skip (*TODO: This should be handled*)

      (* TODO: Visiting (is/should) be done in order of declarations. So, just
         append to a data group under construction. *)
      method! fold_data_item'
          { loc; payload = { data_level; data_name; data_clauses } }
          DIAGS.{ diags; result = acc } =
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

  let data_of_compilation_unit',
      data_of_program_unit'
    =
    let data_of ~fold_root env root =
      let working_item_descrs: _ DIAGS.with_diags =
        fold_root data_item_descrs_checker root (DIAGS.result []) in
      (* Pretty.error "|WSS| = %d@." (List.length working_item_descrs.result); *)
      let working_groups =
        DIAGS.more_result working_item_descrs
          ~f:Cobol_data.Group.of_working_item_descrs
      in
      DIAGS.more_result ~f:begin fun working_groups ->
        let diags =
          DIAGS.forget_result @@
          List.fold_left begin fun acc dg ->
            DIAGS.cons_option_result
              (Cobol_data.Typing.of_data_group (module Config) env dg)
              acc
          end (DIAGS.result []) working_groups
        in
        DIAGS.result working_groups ~diags
      end working_groups
    in
    data_of ~fold_root:Cobol_ptree.Visitor.fold_compilation_unit',
    data_of ~fold_root:Cobol_ptree.Visitor.fold_program_unit'


  let name_of_compilation_unit = function
    | Program {program_name = name; _}
    | Function {function_name = name; _}
    | ClassDefinition {class_name = name; _}
    | InterfaceDefinition {interface_name = name; _} -> ~&name


  (** Main entry for verification of compilation groups.  This function checks
      full groups and builds their associated internal representation. *)
  let typeck_compilation_group (compilation_group: compilation_group) =

    let visitor = object
      inherit [(Cobol_data.PROG_ENV.t list * CUs.t)
                 DIAGS.with_diags] Cobol_ptree.Visitor.folder

      method! fold_compilation_unit' cu acc =
        match ~&cu with
        | Program _ ->
            (* Dealt with in `fold_program_unit'` below *)
            Visitor.do_children acc
        | Function _ | ClassDefinition _ | InterfaceDefinition _ ->
            match Cobol_data.Typing.try_making_env_of_compilation_unit cu with
            | { result = None; diags } ->
                Visitor.skip (DIAGS.with_more_diags ~diags acc)
            | { result = Some cu_env; diags } ->
                (* TODO (++): here we built an env for `u`.  Transmit it to
                   another visitor to build the associated data-groups
                   directly (no need to bother with the "pictured"
                   representation; same for the representation of the
                   procedure division. *)
                let cu_name = name_of_compilation_unit ~&cu
                and cu_wss = data_of_compilation_unit' cu_env cu in
                Visitor.skip @@
                DIAGS.map2_results ~f:begin fun cu_wss (parents, progs) ->
                  let prog = CU.{ cu_name; cu_loc = ~@cu; cu_env;
                                  cu_wss = cu_wss } in
                  DIAGS.result ~diags (parents, CUs.add prog progs)
                end cu_wss acc

      method! fold_program_unit' pu acc =
        let parents, _ = acc.result in
        match Cobol_data.Typing.try_making_env_of_program_unit ~parents pu with
        | { result = None; diags } ->
            Visitor.proceed (DIAGS.with_more_diags ~diags acc)
        | { result = Some cu_env; diags } ->
            (* TODO: same as (++) above. *)
            (* NB: For now we create an standalone CU (note the "C"), but we
               should register nested PUs as children of their parents PUs
               instead. *)
            let acc =
              let cu_name = ~&(~&pu.program_name)
              and cu_wss = data_of_program_unit' cu_env pu in
              DIAGS.map2_results ~f:begin fun cu_wss (parents, progs) ->
                let prog = CU.{ cu_name; cu_loc = ~@pu; cu_env;
                                cu_wss = cu_wss } in
                DIAGS.result ~diags (cu_env :: parents, CUs.add prog progs)
              end cu_wss acc
            in
            (* Proceed with potential nested progs, and then restore stack of
               parents: *)
            Visitor.do_children_and_then acc
              (DIAGS.map_result ~f:(fun (_, progs) -> parents, progs))

      (* skip some divisions *)
      method! fold_environment_division' _ = Visitor.skip
      method! fold_data_division' _ = Visitor.skip
      method! fold_procedure_division' _ = Visitor.skip
    end in

    DIAGS.result ([], CUs.empty) |>
    Cobol_ptree.Visitor.fold_compilation_group visitor compilation_group |>
    DIAGS.map_result ~f:snd
end

let analyze_compilation_group
    (type m) : ?config: _ -> m Cobol_parser.Outputs.parsed_compilation_group -> _ =
  fun ?(config = Cobol_config.default) ->
  let module Typeck = Make (val config) in
  function
  | Only None | WithArtifacts (None, _) ->
      DIAGS.result (CUs.empty, None)
  | Only Some cg | WithArtifacts (Some cg, _) ->
      match Typeck.typeck_compilation_group cg with
      | { diags; _ } when DIAGS.Set.has_errors diags ->
          DIAGS.result ~diags (CUs.empty, Some cg)
      | { diags; result } ->
          DIAGS.result ~diags (result, Some cg)
