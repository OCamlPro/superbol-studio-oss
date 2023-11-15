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
open Cobol_common.Diagnostics.TYPES

module Visitor = Cobol_common.Visitor
module DIAGS = Cobol_common.Diagnostics
module CUs = Cobol_unit.Collections.SET

let name_of_compilation_unit: Cobol_ptree.compilation_unit -> _ = function
  | Program { program_name = name; _ }
  | Function { function_name = name; _ }
  | ClassDefinition { class_name = name; _ }
  | InterfaceDefinition { interface_name = name; _ } -> name

type acc =
  {
    parent_name: string with_loc option;
    parent_config: unit_config option;
    cus: CUs.t;
  }

let init =
  {
    parent_name = None;
    parent_config = None;
    cus = CUs.empty;
    (* unit_config = *)
    (*   { *)
    (*     unit_currency_signs = Cobol_common.Basics.CharSet.singleton '$'; *)
    (*     unit_decimal_point = ','; *)
    (*   } *)
  }

let result acc = acc.cus

let build_units _config = object
  inherit [acc with_diags] Cobol_ptree.Visitor.folder

  method! fold_compilation_unit' cu acc =
    let { parent_name; parent_config; _ } = acc.result in

    let unit_proc_paragraphs
      = Typeck_proc_div.of_compilation_unit cu in

    let { result = unit_config; diags = unit_config_diags }
      = Typeck_config.of_compilation_unit ?parent_config cu in

    (* TODO: unit_data_items *)
    let { result = unit_data; diags = unit_data_diags }
      = Typeck_data_items.of_compilation_unit unit_config cu in

    let unit =
      {
        unit_name = name_of_compilation_unit ~&cu;
        unit_parent_name = parent_name;
        unit_loc = ~@cu;
        unit_config;
        unit_data;
        unit_proc_paragraphs;
      }
    in
    let acc =
      DIAGS.with_more_diags ~diags:unit_config_diags @@
      DIAGS.with_more_diags ~diags:unit_data_diags @@
      DIAGS.map_result acc ~f:begin fun acc ->
        { parent_name = Some unit.unit_name;
          parent_config = Some unit_config;
          cus = CUs.add unit acc.cus }
      end
    in

    (* Proceed with nested progs, if any, and then restore parent's name and
       config: *)
    Visitor.do_children_and_then acc @@
    DIAGS.map_result ~f:(fun acc -> { acc with parent_name; parent_config })

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
  : Cobol_config.t -> Cobol_ptree.compilation_group -> CUs.t with_diags =
  fun config compilation_group ->
  DIAGS.result init |>
  Cobol_ptree.Visitor.fold_compilation_group
    (build_units config) compilation_group |>
  DIAGS.map_result ~f:result
