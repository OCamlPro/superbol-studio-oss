(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2023 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

module Cobol_data = Cobol_data.OLD

open Lsp.Types

open Cobol_common                                                  (* Visitor *)
open Cobol_common.Srcloc.INFIX

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

let extend_range (range: range option as 's) (new_range: 's) =
  match range, new_range with
  | None, _ | _, None ->
      None
  | Some range, Some new_range ->
      Some { range with
             endLine = new_range.endLine;
             endCharacter = new_range.endCharacter }

let acc_ranges_in ~filename ptree acc =
  let open struct
    type acc =
      {
        section_range: range option;
        ranges: range list;
      }
  end in

  let register_range ?kind { loc; _ } acc =
    let range = range_of_loc_in ~filename ?kind loc in
    { acc with ranges = acc_range range acc.ranges }
  in

  let with_subranges ?kind n acc =
    Visitor.do_children @@ register_range ?kind n acc
  and leaf_range ?kind n acc =
    Visitor.skip_children @@ register_range ?kind n acc
  in

  let wide_region n = with_subranges ~kind:FoldingRangeKind.Region n
  and leaf_region n = leaf_range     ~kind:FoldingRangeKind.Region n in

  let { section_range; ranges } =
    Cobol_ptree.Visitor.fold_compilation_group (object
      inherit [acc] Cobol_ptree.Visitor.folder

      method! fold_compilation_unit' = wide_region
      method! fold_options_paragraph' = leaf_region

      method! fold_data_division' = wide_region
      method! fold_file_section' = wide_region
      method! fold_working_storage_section' = wide_region
      method! fold_linkage_section' = wide_region
      method! fold_communication_section' = wide_region
      method! fold_local_storage_section' = wide_region
      method! fold_report_section' = wide_region
      method! fold_screen_section' = wide_region

      method! fold_environment_division' = wide_region
      method! fold_configuration_section' = wide_region
      (* method! fold_source_computer_paragraph' = region *)
      (* method! fold_object_computer_paragraph' = region *)
      (* method! fold_special_names_paragraph' = region *)
      method! fold_repository_paragraph' = leaf_region
      method! fold_input_output_section' = leaf_region
      method! fold_file_control_paragraph' = leaf_region
      method! fold_io_control_paragraph' = leaf_region

      method! fold_procedure_division' = wide_region
      method! fold_statement' = wide_region

      (*TODO:
        - add location for some nodes in the ast
          so that we can define folding_range for
          environment division, file section... (predefined section)

        - it is possible to add folding_range for
        - branch of statement(else_branch, evaluate_branch...)
        - handler(on_size_error)
        - inline_call

        - add folding_range for other type of compilation_unit (not program) *)

      method! fold_paragraph' {payload = { paragraph_is_section; _ }; loc} acc =
        let range = range_of_loc_in ~filename loc in
        Visitor.do_children @@
        if paragraph_is_section
        then { section_range = range;
               ranges = acc_range acc.section_range acc.ranges }
        else { section_range = extend_range acc.section_range range;
               ranges = acc_range range acc.ranges }

    end) ptree { section_range = None; ranges = acc }
  in

  acc_range section_range ranges


(*TODO:
  Now we use the type Group.t (need to be rewritten),
  which does not work for renames-item, condition-item ... *)
let folding_range_data_in ~filename ({ cu_wss; _ }: Cobol_data.Types.compilation_unit) =
  (*add the folding_range of grouped item *)
  let rec add group l =
    let r = range_of_loc_in ~filename ~@group in
    match ~&group with
    | Cobol_data.Group.Elementary _
    | Constant _ | Renames _ | ConditionName _ -> None, l
    | Group {elements; _} ->
        let r, l =
          List.fold_left
            (fun (r, l) group -> aux group (r, l))
            (r, l) elements
        in
        match r with
        | None -> None, l
        | Some r -> Some r, r :: l
  (*traverse the elements, update the folding_range of grouped item*)
  and aux group (r, l) =
    match ~&group with
    | Cobol_data.Group.Elementary _
    | Constant _ | Renames _ | ConditionName _ ->
        extend_range r (range_of_loc_in ~filename ~@group), l
    | Group _ ->
        let r', l = add group l in
        extend_range r r', l
  in
  List.fold_left
    (fun acc group -> snd @@ add group acc) [] cu_wss


let ranges_in ~filename ptree cus =
  Cobol_data.Compilation_unit.SET.to_seq cus
  |> Seq.map (fun cu -> folding_range_data_in ~filename cu)
  |> List.of_seq
  |> List.flatten
  |> acc_ranges_in ~filename ptree
