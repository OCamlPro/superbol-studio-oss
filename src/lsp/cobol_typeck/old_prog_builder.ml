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

module Cobol_data = Cobol_data.OLD
module Env_builder = Old_env_builder
module Group_builder = Old_group_builder

open Cobol_ptree
open Cobol_common.Srcloc.INFIX
open Cobol_common.Diagnostics.TYPES

module Visitor = Cobol_common.Visitor
module DIAGS = Cobol_common.Diagnostics
module CU = Cobol_data.Compilation_unit
module CUs = CU.SET

let name_of_compilation_unit = function
  | Program { program_name = name; _ }
  | Function { function_name = name; _ }
  | ClassDefinition { class_name = name; _ }
  | InterfaceDefinition { interface_name = name; _ } -> ~&name

let register_cu ~cu_name ~cu_loc ~cu_env ~cu_wss (parents, progs) =
  let prog = CU.{ cu_name; cu_loc; cu_env; cu_wss } in
  DIAGS.result (cu_env :: parents, CUs.add prog progs)

let lookup_cus config = object
  inherit [(Cobol_data.PROG_ENV.t list * CUs.t)
             with_diags] Cobol_ptree.Visitor.folder

  method! fold_compilation_unit' cu acc =
    let parents, _ = acc.result in
    match Env_builder.for_compilation_unit_old ~parents cu with
    | { result = None; diags } ->
        Visitor.skip (DIAGS.with_more_diags ~diags acc)

    | { result = Some cu_env; diags } ->
        (* TODO: here we built an env for `u`.  Transmit it to another visitor
           to build the associated data-groups directly (no need to bother
           with the "pictured" representation; same for the representation of
           the procedure division. *)
        let acc =
          let cu_name = name_of_compilation_unit ~&cu
          and cu_wss =
            Group_builder.working_data_of_compilation_unit' config cu_env cu
          in
          DIAGS.with_more_diags ~diags @@
          DIAGS.map2_results cu_wss acc
            ~f:(fun cu_wss -> register_cu ~cu_name ~cu_loc:~@cu ~cu_env ~cu_wss)
        in
        (* Proceed with potential nested progs, and then restore stack of
           parents: *)
        (* TODO: maybe filter *)
        Visitor.do_children_and_then acc
          (DIAGS.map_result ~f:(fun (_, progs) -> parents, progs))

  (* skip some divisions/sections/paragraphs *)
  method! fold_informational_paragraphs _ = Visitor.skip
  method! fold_options_paragraph' _ = Visitor.skip
  method! fold_environment_division' _ = Visitor.skip
  method! fold_data_division' _ = Visitor.skip
  method! fold_procedure_division' _ = Visitor.skip
end

(** Main entry for verification of compilation groups.  This function checks
    full groups and builds their associated internal representation. *)
let compilation_group config (compilation_group: compilation_group) =
  DIAGS.result ([], CUs.empty) |>
  Cobol_ptree.Visitor.fold_compilation_group
    (lookup_cus config) compilation_group |>
  DIAGS.map_result ~f:snd
