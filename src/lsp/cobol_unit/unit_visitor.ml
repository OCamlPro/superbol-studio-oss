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

open Unit_types

open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)
open Cobol_common.Srcloc.TYPES

(* --- *)

class ['a] folder = object
  inherit ['a] Cobol_data.Visitor.folder
  inherit!['a] Cobol_ptree.Proc_division_visitor.folder
  method fold_unit_group: (Unit_group.t, 'a) fold = default
  method fold_cobol_unit: (cobol_unit, 'a) fold = default
  method fold_cobol_unit': (cobol_unit with_loc, 'a) fold = default
  method fold_unit_config: (unit_config, 'a) fold = default
  method fold_data_definitions: (data_definitions, 'a) fold = default
  method fold_procedure_section: (procedure_section, 'a) fold = default
  method fold_procedure_section': (procedure_section with_loc, 'a) fold = default
  method fold_procedure_paragraph: (procedure_paragraph, 'a) fold = default
  method fold_procedure_paragraph': (procedure_paragraph with_loc, 'a) fold = default
  method fold_procedure_block: (procedure_block, 'a) fold = default
  method fold_procedure: (procedure, 'a) fold = default
end

(* --- *)

let fold_unit_config (v: _ #folder) = leaf v#fold_unit_config

let fold_data_definitions (v: _ #folder) =
  handle v#fold_data_definitions
    ~continue:begin fun { data_records; data_items = _ } x -> x
      (* traverse via full records, skip individual items view *)
      >> fold_list ~fold:Cobol_data.Visitor.fold_record v data_records
    end

let fold_procedure_paragraph (v: _ #folder) =
  handle v#fold_procedure_paragraph
    ~continue:begin fun { paragraph_name; paragraph } x -> x
      >> Cobol_ptree.Terms_visitor.fold_procedure_name'_opt v paragraph_name
      >> Cobol_ptree.Proc_division_visitor.fold_paragraph' v paragraph
    end

let fold_procedure_paragraph' (v: _ #folder) =
  handle' v#fold_procedure_paragraph' ~fold:fold_procedure_paragraph v

let fold_procedure_section (v: _ #folder) =
  handle v#fold_procedure_section
    ~continue:begin fun { section_name; section_paragraphs } x -> x
      >> Cobol_ptree.Terms_visitor.fold_procedure_name' v section_name
      >> fold_list v section_paragraphs.list ~fold:fold_procedure_paragraph'
    end

let fold_procedure_section' (v: _ #folder) =
  handle' v#fold_procedure_section' ~fold:fold_procedure_section v

let fold_procedure_block (v: _ #folder) =
  handle v#fold_procedure_block
    ~continue:begin function
      | Paragraph p -> fold_procedure_paragraph' v p
      | Section s -> fold_procedure_section' v s
    end

let fold_procedure (v: _ #folder) =
  handle v#fold_procedure                                     (* skip `named` *)
    ~continue:(fun s -> fold_list v ~fold:fold_procedure_block s.list)

let fold_cobol_unit (v: _ #folder) =
  handle v#fold_cobol_unit
    ~continue:begin fun { unit_name; unit_parent_name; unit_config;
                          unit_data; unit_procedure } x -> x
      >> fold_string' v unit_name
      >> fold_string'_opt v unit_parent_name
      >> fold_unit_config v unit_config
      >> fold_data_definitions v unit_data
      >> fold_procedure v unit_procedure
    end

let fold_cobol_unit' (v: _ #folder) =
  handle' v#fold_cobol_unit' ~fold:fold_cobol_unit v

let fold_unit_group (v: _ #folder) =
  handle v#fold_unit_group
    ~continue:(Unit_group.fold (fold_cobol_unit' v))
