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

module Qualmap = Cobol_unit.Qualmap
module Visitor = Cobol_common.Visitor

type output =
  {
    procedure: Cobol_unit.Types.procedure;
    references: Typeck_outputs.references_in_unit;
  }

let procedure_of_compilation_unit cu' =

  let open struct
    type acc =
      {
        blocks: procedure_block Qualmap.t;
        block_list: procedure_block list;
        current_section: section_under_construction option;
      }
    and section_under_construction = procedure_section with_loc

    let init =
      {
        blocks = Qualmap.empty;
        block_list = [];
        current_section = None;
      }

    let procedure acc =
      {
        named = acc.blocks;
        list = List.rev acc.block_list;
      }

    let name n : Cobol_ptree.qualname = Name n
    let qual n q : Cobol_ptree.qualname = Qual (n, q)

    let section_block ({ payload = suc; loc }: section_under_construction) =
      Section ({ suc with
                 section_paragraphs = List.rev suc.section_paragraphs } &@ loc)

    let simple_paragraph (p: Cobol_ptree.paragraph with_loc) acc =
      match acc.current_section, ~&p.paragraph_name with
      | None, Some n ->
          { paragraph_name = Some (name n &@<- n);
            paragraph = p } &@<- p
      | Some { payload = { section_name = qn; _ }; _ }, Some n ->
          { paragraph_name = Some (qual n ~&qn &@<- n);
            paragraph = p } &@<- p
      | _, None ->
          { paragraph_name = None;
            paragraph = p } &@<- p

    let paragraph_block p =
      Paragraph p

    let commit_section acc =
      match acc.current_section with
      | Some ({ payload = { section_name = qn; _ }; _ } as section) ->
          let section = section_block section in
          { blocks = Qualmap.add ~&qn section acc.blocks;
            block_list = section :: acc.block_list;
            current_section = None }
      | None ->
          acc

    let start_new_section n s acc =
      let acc = commit_section acc in
      let section_paragraphs = [simple_paragraph s acc] in
      { acc with
        current_section = Some ({ section_name = name n &@<- n;
                                  section_paragraphs } &@<- s) }

    let named_paragraph n p acc =
      let p = simple_paragraph p acc in
      let qn, block_list, current_section =
        match acc.current_section with
        | None ->
            name n,
            paragraph_block p :: acc.block_list,
            None
        | Some s ->
            let loc = Cobol_common.Srcloc.concat ~@s ~@p in
            let section_paragraphs = p :: ~&s.section_paragraphs in
            qual n ~&(~&s.section_name),
            acc.block_list,
            Some ({ ~&s with section_paragraphs } &@ loc)
      in
      { blocks = Qualmap.add qn (paragraph_block p) acc.blocks;
        block_list;
        current_section }

    let anonymous_paragraph p acc =         (* only at beginning of procedure *)
      { acc with
        block_list = paragraph_block (simple_paragraph p acc) :: acc.block_list }

  end in

  let visitor = object
    inherit [acc] Cobol_ptree.Visitor.folder

    method! fold_informational_paragraphs _ = Visitor.skip
    method! fold_options_paragraph' _ = Visitor.skip
    method! fold_environment_division _ = Visitor.skip
    method! fold_data_division' _ = Visitor.skip
    method! fold_procedure_division _ acc =
      Visitor.do_children_and_then acc
        commit_section                  (* be sure to handle the last section *)

    method! fold_paragraph' p acc =
      Visitor.skip_children @@ match ~&p with
      | { paragraph_is_section = true;
          paragraph_name = Some name; _ } -> start_new_section name p acc
      | { paragraph_name = Some name; _ } -> named_paragraph name p acc
      | { paragraph_name = None; _ }      -> anonymous_paragraph p acc

    (* TODO: nested programs... *)
    method! fold_nested_programs _  = Visitor.skip

  end in

  Cobol_ptree.Visitor.fold_compilation_unit' visitor
    cu' init |> procedure

(* --- *)

let references ~(data_definitions: Cobol_unit.Types.data_definitions) procedure =

  let open struct
    (* TODO: add a context, and gather references to procedures, etc. *)
    type acc =
      {
        refs: Typeck_outputs.references_in_unit;
        diags: Typeck_diagnostics.t;
      }
    let init =
      {
        refs = Typeck_outputs.no_refs;
        diags = Typeck_diagnostics.none;
      }
    let references { refs; diags } = refs, diags
  end in

  let baseloc_of_qualname: Cobol_ptree.qualname -> srcloc = function
    | Name name
    | Qual (name, _) -> ~@name
  in

  let error acc err =
    { acc with diags = Proc_error err :: acc.diags }
  in

  let visitor = object
    inherit [acc] Cobol_unit.Visitor.folder

    method! fold_qualname qn acc =                         (* TODO: qualname' *)
      let loc = baseloc_of_qualname qn in
      Visitor.skip_children @@
      match Cobol_unit.Qualmap.find qn data_definitions.data_items.named with
      | Data_item { def; _ } ->
          { acc with
            refs = Typeck_outputs.register_data_item_ref ~loc def acc.refs }
      | Data_renaming { def; _ } ->
          { acc with
            refs = Typeck_outputs.register_data_renaming_ref ~loc def acc.refs }
      | exception Not_found ->
          acc  (* ignored for now, as we don't process all the DATA DIV. yet. *)
      | exception Cobol_unit.Qualmap.Ambiguous matching_qns ->
          error acc @@
          Ambiguous_data_name { given_qualname = qn &@ loc;
                                matching_qualnames = Lazy.force matching_qns }

    method! fold_procedure_name' ({ loc; _ } as qn) acc =
      Visitor.skip_children @@
      match Cobol_unit.Qualmap.find ~&qn procedure.named with
      | block ->
          { acc with
            refs = Typeck_outputs.register_procedure_ref ~loc block acc.refs }
      | exception Not_found ->
          error acc @@ Unknown_proc_name qn
      | exception Cobol_unit.Qualmap.Ambiguous matching_qns ->
          error acc @@
          Ambiguous_proc_name { given_qualname = qn;
                                matching_qualnames = Lazy.force matching_qns }

  end in

  Cobol_unit.Visitor.fold_procedure visitor procedure init |> references

(* --- *)

let of_compilation_unit ~data_definitions cu' =
  let procedure = procedure_of_compilation_unit cu' in
  let references, diags = references ~data_definitions procedure in
  {
    procedure;
    references;
  },
  diags
