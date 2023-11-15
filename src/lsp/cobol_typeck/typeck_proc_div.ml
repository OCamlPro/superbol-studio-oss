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
(* open Cobol_common.Srcloc.TYPES *)
open Cobol_common.Srcloc.INFIX
(* open Cobol_common.Diagnostics.TYPES *)

module Qualmap = Cobol_unit.Qualmap
module Visitor = Cobol_common.Visitor
(* module DIAGS = Cobol_common.Diagnostics *)
(* module CUs = Cobol_unit.Collections.SET *)

let of_compilation_unit cu' =

  let open struct
    type acc =
      {
        paragraphs: procedure_paragraph Qualmap.t;
        paragraph_list: procedure_paragraph list;
        current_section: section_under_construction option;
      }
    and section_under_construction = procedure_section

    let init =
      {
        paragraphs = Qualmap.empty;
        paragraph_list = [];
        current_section = None;
      }

    let result acc =
      {
        named = acc.paragraphs;
        list = List.rev acc.paragraph_list;
      }

    let name n : Cobol_ptree.qualname = Name n
    let qual n q : Cobol_ptree.qualname = Qual (n, q)

    let section_paragraph (suc: section_under_construction) =
      Section { suc with section_paragraphs = List.rev suc.section_paragraphs }

    let simple_paragraph p =
      Paragraph p

    let commit_section acc =
      match acc.current_section with
      | Some ({ section_name = n; _ } as section) ->
          let section = section_paragraph section in
          { paragraphs = Qualmap.add (Name n) section acc.paragraphs;
            paragraph_list = section :: acc.paragraph_list;
            current_section = None }
      | None ->
          acc

    let start_new_section n s acc =
      let acc = commit_section acc in
      { acc with current_section = Some { section_name = n;
                                          section_paragraphs = [s] } }

    let start_new_paragraph n p acc =
      let paragraph = simple_paragraph p in
      let qn, paragraph_list, current_section =
        match acc.current_section with
        | None ->
            name n,
            paragraph :: acc.paragraph_list,
            None
        | Some s ->
            qual n @@ name s.section_name,
            acc.paragraph_list,
            Some { s with section_paragraphs = p :: s.section_paragraphs }
      in
      { paragraphs = Qualmap.add qn paragraph acc.paragraphs;
        paragraph_list;
        current_section }

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
      | { paragraph_name = None; _ } -> acc
      | { paragraph_is_section = true;
          paragraph_name = Some name; _ } -> start_new_section name p acc
      | { paragraph_name = Some name; _ } -> start_new_paragraph name p acc

    (* TODO: nested programs... *)

  end in

  Cobol_ptree.Visitor.fold_compilation_unit' visitor
    cu' init |> result
