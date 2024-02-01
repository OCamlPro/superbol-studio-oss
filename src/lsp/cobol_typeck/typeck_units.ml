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
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module Visitor = Cobol_common.Visitor
module DIAGS = Cobol_common.Diagnostics
module CUs = Cobol_unit.Collections.SET
module CUMap = Cobol_unit.Collections.MAP

let name_of_compilation_unit: Cobol_ptree.compilation_unit -> _ = function
  | Program { program_name = name; _ }
  | Function { function_name = name; _ }
  | ClassDefinition { class_name = name; _ }
  | InterfaceDefinition { interface_name = name; _ } -> name

type acc =
  {
    parent_name: string with_loc option;
    parent_config: unit_config option;
    cus: CUs.t;                                                    (* = group *)
    artifacts: Typeck_outputs.artifacts;
    diags: Typeck_diagnostics.diagnostics;
  }

let init =
  {
    parent_name = None;
    parent_config = None;
    cus = CUs.empty;
    artifacts = Typeck_outputs.no_artifacts;
    diags = Typeck_diagnostics.none;
    (* unit_config = *)
    (*   { *)
    (*     unit_currency_signs = Cobol_common.Basics.CharSet.singleton '$'; *)
    (*     unit_decimal_point = ','; *)
    (*   } *)
  }

let result ptree acc =
  Typeck_outputs.{
    ptree;
    group = acc.cus;
    artifacts = acc.artifacts;
  },
  acc.diags

let build_units _config = object
  inherit [acc] Cobol_ptree.Visitor.folder

  method! fold_compilation_unit' cu ({ parent_name; parent_config; _ } as acc) =

    let unit_config, unit_config_diags
      = Typeck_config.of_compilation_unit ?parent_config cu in

    let unit_data, unit_data_diags
      = Typeck_data_items.of_compilation_unit unit_config cu in

    let unit_procedure, unit_procedure_diags
      = Typeck_procedure.of_compilation_unit cu
        ~data_definitions:unit_data.definitions in

    let unit =
      {
        unit_name = name_of_compilation_unit ~&cu;
        unit_parent_name = parent_name;
        unit_config;
        unit_data = unit_data.definitions;
        unit_procedure = unit_procedure.procedure;
      } &@<- cu
    in

    let references =
      CUMap.add unit { unit_procedure.references with
                       data_refs = Typeck_outputs.merge_qualrefmaps
                           unit_data.references
                           unit_procedure.references.data_refs }
        acc.artifacts.references
    in

    let acc =
      {
        parent_name = Some ~&unit.unit_name;
        parent_config = Some unit_config;
        cus = CUs.add unit acc.cus;
        artifacts = { references };
        diags =
          Typeck_diagnostics.(union unit_config_diags @@
                              union unit_data_diags   @@ unit_procedure_diags);
      }
    in

    (* Proceed with nested progs, if any, and then restore parent's name and
       config: *)
    Visitor.do_children_and_then acc
      (fun acc -> { acc with parent_name; parent_config })

  (* skip some divisions/sections/paragraphs *)
  method! fold_informational_paragraphs _ = Visitor.skip
  method! fold_options_paragraph' _ = Visitor.skip
  method! fold_environment_division' _ = Visitor.skip
  method! fold_data_division' _ = Visitor.skip
  method! fold_procedure_division' _ = Visitor.skip
end


(** This function builds the internal representation of full compilation
    groups. *)
let of_compilation_group
  : Cobol_config.Types.t -> Cobol_ptree.compilation_group ->
    Typeck_outputs.t * Typeck_diagnostics.t =
  fun config compilation_group_ptree ->
  Cobol_ptree.Visitor.fold_compilation_group
    (build_units config) compilation_group_ptree init |>
  result compilation_group_ptree
