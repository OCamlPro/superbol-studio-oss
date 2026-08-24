(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Cobol_common.Srcloc.TYPES
open Lsp.Types

module Positions = Set.Make (struct
    type t = Position.t
    let compare (p1: t) (p2: t) =
      let c = p2.line - p1.line in
      if c <> 0 then c else p2.character - p1.character
  end)

type context =
  | Procedure_division
  | Data_division
  | Global_context

let enter_context context (prev_context, acc) =
  Cobol_common.Visitor.do_children_and_then (context, acc)
    (fun (_, acc) -> prev_context, acc)

let positions ~uri group artifacts =
  let filename = Lsp.Uri.to_path uri in
  let take_when_in context { loc; _ } (current_context, acc) =
    if context <> current_context then
      Cobol_common.Visitor.skip (current_context, acc)
    else
      let range = Lsp_position.range_of_srcloc_in ~filename loc in
      Cobol_common.Visitor.skip (context, Positions.add range.start acc)
  in
  Cobol_unit.Visitor.fold_unit_group object (v)
    inherit [_] Cobol_unit.Visitor.folder
    method! fold_procedure _ =
      enter_context Procedure_division
    method! fold_data_definitions _ =
      enter_context Data_division
    method! fold_paragraph' _ =
      Cobol_common.Visitor.skip
    method! fold_procedure_name' =
      take_when_in Procedure_division
    method! fold_qualname' =
      take_when_in Data_division
    method! fold_record_renaming { renaming_name; _ } =
      take_when_in Data_division renaming_name
    method! fold_field_definition { field_qualname; field_redefines;
                                    field_leading_ranges;
                                    field_offset; field_size; field_layout;
                                    field_conditions; field_redefinitions;
                                    field_length_variability = _;
                                    field_has_definition_issues = _ } acc =
      ignore(field_redefines, field_leading_ranges, field_offset, field_size);
      Cobol_common.Visitor.skip @@ begin
        acc
        |> Cobol_ptree.Visitor.fold_qualname'_opt v field_qualname
        |> Cobol_data.Visitor.fold_field_layout v field_layout
        |> Cobol_data.Visitor.fold_condition_names v field_conditions
        |> Cobol_data.Visitor.fold_item_redefinitions v field_redefinitions
      end
    method! fold_table_definition { table_field; table_offset; table_size;
                                    table_range; table_init_values;
                                    table_redefines; table_redefinitions;
                                    table_has_definition_issues } acc =
      ignore(table_offset, table_size, table_init_values, table_redefines,
             table_has_definition_issues);
      Cobol_common.Visitor.skip @@ begin
        acc
        |> Cobol_data.Visitor.fold_field_definition' v table_field
        |> Cobol_data.Visitor.fold_table_range v table_range
        |> Cobol_data.Visitor.fold_item_redefinitions v table_redefinitions
      end
  end group (Global_context, Positions.empty) |>
  snd |>
  Cobol_preproc.Trace.fold artifacts.Cobol_parser.Outputs.pplog
    ~f:begin fun event positions ->
      try match event with    (* Some locations in the pre-processor log may not
                                 involve [filename], so we need to catch those
                                 cases. *)
        | Cobol_preproc.Trace.Variable_definition { loc; _ } ->
            let range = Lsp_position.range_of_srcloc_in ~filename loc in
            Positions.add range.start positions
        | _ ->
            positions
      with Invalid_argument _ -> positions
    end |>
  Positions.elements
