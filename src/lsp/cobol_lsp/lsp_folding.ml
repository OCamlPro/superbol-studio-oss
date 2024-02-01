(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2023 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

open Lsp.Types

open Cobol_common                                                  (* Visitor *)
open Cobol_common.Srcloc.TYPES

type range = FoldingRange.t

let range_of_loc_in ~filename ?kind loc =
  try
    let p1, p2 = Srcloc.lexloc_in ~filename loc in
    Option.some @@ FoldingRange.create ()
      ~startLine:(p1.pos_lnum - 1)
      ~startCharacter:(p1.pos_cnum - p1.pos_bol)
      ~endLine:(p2.pos_lnum - 1)
      ~endCharacter:(p2.pos_cnum - p2.pos_bol)
      ?kind
  with Invalid_argument _ ->
    (* Filename did not take part in the construction of loc.  This may happen
       on dummy locations inserted during recovery. *)
    Option.none

let acc_range = function
  | None -> Fun.id
  | Some r -> List.cons r

(* let extend_range (range: range option as 's) (new_range: 's) = *)
(*   match range, new_range with *)
(*   | None, _ | _, None -> *)
(*       None *)
(*   | Some range, Some new_range -> *)
(*       Some { range with *)
(*              endLine = new_range.endLine; *)
(*              endCharacter = new_range.endCharacter } *)

let ranges_in ~filename ptree group =

  let register_range ?kind { loc; _ } acc =
    acc_range (range_of_loc_in ~filename ?kind loc) acc
  in

  let with_subranges ?kind n acc =
    Visitor.do_children @@ register_range ?kind n acc
  and leaf_range ?kind n acc =
    Visitor.skip_children @@ register_range ?kind n acc
  in

  let wide_region n = with_subranges ~kind:FoldingRangeKind.Region n
  and leaf_region n = leaf_range     ~kind:FoldingRangeKind.Region n in

  let ptree_ranges =
    Cobol_ptree.Visitor.fold_compilation_group object
      inherit [range list] Cobol_ptree.Visitor.folder

      method! fold_compilation_unit' = wide_region
      method! fold_options_paragraph' = leaf_region

      method! fold_data_division' = wide_region
      method! fold_file_section' = leaf_region
      method! fold_working_storage_section' = leaf_region
      method! fold_linkage_section' = leaf_region
      method! fold_communication_section' = wide_region               (* TODO *)
      method! fold_local_storage_section' = leaf_region
      method! fold_report_section' = wide_region                      (* TODO *)
      method! fold_screen_section' = wide_region                      (* TODO *)

      method! fold_environment_division' = wide_region
      method! fold_configuration_section' = wide_region
      (* method! fold_source_computer_paragraph' = region *)
      (* method! fold_object_computer_paragraph' = region *)
      (* method! fold_special_names_paragraph' = region *)
      method! fold_repository_paragraph' = leaf_region
      method! fold_input_output_section' = leaf_region
      method! fold_file_control_paragraph' = leaf_region
      method! fold_io_control_paragraph' = leaf_region

      (* Stop at the procedure division, as we fold over the unit group
         representaton below to grab ranges of sections and paragraphs. *)
      method! fold_procedure_division' = leaf_region
      (* method! fold_statement' = wide_region *)

      (* method! fold_paragraph' {payload = { paragraph_is_section; _ }; loc} acc = *)
      (*   let range = range_of_loc_in ~filename loc in *)
      (*   Visitor.do_children @@ *)
      (*   if paragraph_is_section *)
      (*   then { section_range = range; *)
      (*          ranges = acc_range acc.section_range acc.ranges } *)
      (*   else { section_range = extend_range acc.section_range range; *)
      (*          ranges = acc_range range acc.ranges } *)

    end ptree []
  in

  let all_ranges =
    Cobol_unit.Visitor.fold_unit_group object
      inherit [range list] Cobol_unit.Visitor.folder
      method! fold_unit_config _ = Visitor.skip
      method! fold_item_definition' = wide_region
      method! fold_procedure_section' = wide_region
      method! fold_procedure_paragraph' = wide_region
      method! fold_statement' = wide_region
    end group ptree_ranges
  in

  all_ranges
