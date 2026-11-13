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
open Cobol_common.Srcloc.INFIX
open Lsp.Types

module Visitor = Cobol_common.Visitor

module Positions = Map.Make (struct
    type t = Position.t
    let compare (p1: t) (p2: t) =
      let c = p1.line - p2.line in
      if c <> 0 then c else p1.character - p2.character
  end)

(** A code lens, at [position], for the given [element].  [element] and
    [cu_name] are those {!Lsp_lookup.element_at_position} would give at
    [position]. *)
type lens =
  {
    position: Position.t;
    element: Lsp_lookup.TYPES.element_in_context;
    cu_name: string option;            (* name of enclosing compilation unit *)
  }

type context =
  | Procedure_division
  | Data_division
  | Global_context

type acc =
  {
    context: context;
    cu_name: string option;
    section: Cobol_unit.Types.section option;
    lenses: lens Positions.t;
  }

(** [lenses ~uri group artifacts] computes the code lenses of the document
    [uri], along with the element each lens is about.  This is done in a single
    traversal of [group], so that code lenses do not need to lookup elements by
    position.  Lenses are given in reverse order of their positions. *)
let lenses ~uri group artifacts =
  let filename = Lsp_utils.path_of_uri uri in
  let add_lens ~cu_name position element lenses =
    Positions.add position { position; element; cu_name } lenses
  in
  let with_children_and_then f acc =
    Visitor.do_children_and_then (f acc)
      (fun acc' -> { acc' with context = acc.context; cu_name = acc.cu_name;
                               section = acc.section })
  in
  let enter_context context =
    with_children_and_then (fun acc -> { acc with context; section = None })
  in
  let take_when_in context ~element ({ loc; _ } as x) acc =
    if context <> acc.context then
      Visitor.skip acc
    else
      let range = Lsp_position.range_of_srcloc_in ~filename loc in
      Visitor.skip
        { acc with lenses = add_lens ~cu_name:acc.cu_name range.start
                       (element acc x) acc.lenses }
  in
  let take_data_name =
    take_when_in Data_division
      ~element:(fun _ qn -> Lsp_lookup.TYPES.Data_full_name ~&qn)
  and take_proc_name =
    take_when_in Procedure_division
      ~element:begin fun { section; _ } qn ->
        Lsp_lookup.TYPES.Proc_name { qn = ~&qn; enclosing_section = section }
      end
  in
  Cobol_unit.Visitor.fold_unit_group object (v)
    inherit [_] Cobol_unit.Visitor.folder
    inherit! [_] Lsp_position.shallow_sieve ~filename
    method! fold_cobol_unit' cu =
      with_children_and_then
        (fun acc -> { acc with cu_name = Some ~&(~&cu.unit_name) })
    method! fold_procedure _ =
      enter_context Procedure_division
    method! fold_section s =
      with_children_and_then (fun acc -> { acc with section = Some s })
    method! fold_data_definitions _ =
      enter_context Data_division
    method! fold_statements' _ =
      Visitor.skip
    method! fold_procedure_name' =
      take_proc_name
    method! fold_qualname' =
      take_data_name
    method! fold_record_renaming { renaming_name; _ } =
      take_data_name renaming_name
    method! fold_field_definition { field_qualname; field_redefines;
                                    field_leading_ranges;
                                    field_offset; field_size; field_size_max;
                                    field_layout;
                                    field_conditions; field_redefinitions;
                                    field_length_variability = _;
                                    field_has_definition_issues = _ } acc =
      ignore(field_redefines, field_leading_ranges, field_offset, field_size,
             field_size_max);
      Cobol_common.Visitor.skip @@ begin
        acc
        |> Cobol_ptree.Visitor.fold_qualname'_opt v field_qualname
        |> Cobol_data.Visitor.fold_field_layout v field_layout
        |> Cobol_data.Visitor.fold_condition_names v field_conditions
        |> Cobol_data.Visitor.fold_item_redefinitions v field_redefinitions
      end
    method! fold_table_definition { table_field; table_offset; table_size;
                                    table_size_max; table_range;
                                    table_init_values; table_redefines;
                                    table_redefinitions;
                                    table_has_definition_issues } acc =
      ignore(table_offset, table_size, table_size_max, table_init_values,
             table_redefines, table_has_definition_issues);
      Cobol_common.Visitor.skip @@ begin
        acc
        |> Cobol_data.Visitor.fold_field_definition' v table_field
        |> Cobol_data.Visitor.fold_table_range v table_range
        |> Cobol_data.Visitor.fold_item_redefinitions v table_redefinitions
      end
  end group { context = Global_context; cu_name = None; section = None;
              lenses = Positions.empty } |>
  (fun { lenses; _ } -> lenses) |>
  (* Note: pre-processor elements take priority over those of the parse-tree,
     as in {!Lsp_lookup.element_at_position}. *)
  Cobol_preproc.Trace.fold artifacts.Cobol_parser.Outputs.pplog
    ~f:begin fun event lenses ->
      try match event with    (* Some locations in the pre-processor log may not
                                 involve [filename], so we need to catch those
                                 cases. *)
        | Cobol_preproc.Trace.Variable_definition { def; loc; _ } ->
            (match Lsp_position.shallow_start_position_in ~filename loc with
             | None -> lenses
             | Some pos ->
                 add_lens ~cu_name:None pos
                   (Lsp_lookup.TYPES.Preproc_or_compilation_variable_ref
                      { def; use = `Definition; loc }) lenses)
        | _ ->
            lenses
      with Invalid_argument _ -> lenses
    end |>
  fun lenses ->
  (* Note: gives lenses in reverse order of positions; avoids non-tail-recursive
     [List.map] on potentially long lists. *)
  Positions.fold (fun _ lens acc -> lens :: acc) lenses []
