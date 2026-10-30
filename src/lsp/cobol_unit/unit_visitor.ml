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
  inherit! ['a] Cobol_procedure.Visitor.folder
  method fold_unit_group: (Unit_group.t, 'a) fold = default
  method fold_cobol_unit: (cobol_unit, 'a) fold = default
  method fold_cobol_unit': (cobol_unit with_loc, 'a) fold = default
  method fold_unit_config: (unit_config, 'a) fold = default
  method fold_unit_env: (unit_env, 'a) fold = default
  method fold_data_definitions: (data_definitions, 'a) fold = default
  method fold_section: (section, 'a) fold = default
  method fold_section': (section with_loc, 'a) fold = default
  method fold_paragraph: (paragraph, 'a) fold = default
  method fold_paragraph': (paragraph with_loc, 'a) fold = default
  method fold_procedure_block: (procedure_block, 'a) fold = default
  method fold_procedure_using: (procedure_using, 'a) fold = default
  method fold_procedure_using': (procedure_using with_loc, 'a) fold = default
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

let fold_paragraph (v: _ #folder) =
  handle v#fold_paragraph
    ~continue:begin fun { paragraph_name; paragraph_sentences } x -> x
      >> Cobol_ptree.Terms_visitor.fold_procedure_name'_opt v paragraph_name
      >> fold_list ~fold:Cobol_procedure.Visitor.fold_statements' v paragraph_sentences
    end

let fold_paragraph' (v: _ #folder) =
  handle' v#fold_paragraph' ~fold:fold_paragraph v

let fold_section (v: _ #folder) =
  handle v#fold_section
    ~continue:begin fun { section_name; section_segment;
                          section_paragraphs } x -> x
      >> Cobol_ptree.Terms_visitor.fold_procedure_name'_opt v
        (Option.map (fun sn ->
             Cobol_ptree.{ payload = Name sn; loc = sn.loc }) section_name)
      >> fold_option ~fold:Cobol_procedure.Visitor.fold_integer_value v
        section_segment
      >> fold_list v section_paragraphs.list ~fold:fold_paragraph'
    end

let fold_section' (v: _ #folder) =
  handle' v#fold_section' ~fold:fold_section v

let fold_procedure_block (v: _ #folder) =
  handle v#fold_procedure_block
    ~continue:begin function
      | Paragraph p -> fold_paragraph' v p
      | Section s -> fold_section' v s
    end

let fold_procedure_using (v: _ #folder) =
  handle v#fold_procedure_using
    ~continue:(fold_with_loc_list v
                 ~fold:Cobol_procedure.Visitor.fold_procedure_arg)

let fold_procedure_using' (v: _ #folder) =
  handle' v#fold_procedure_using' ~fold:fold_procedure_using v

let fold_procedure (v: _ #folder) =
  handle v#fold_procedure                                     (* skip `named` *)
    ~continue:begin fun { procedure_sections; procedure_using; _ } x -> x
      >> fold_option ~fold:fold_procedure_using' v procedure_using
      >> fold_list v ~fold:fold_section' procedure_sections
    end

let fold_unit_env (v: _ #folder) =
  handle v#fold_unit_env
    ~continue:begin fun { env_config } x ->
      fold_unit_config v env_config x
    end

let fold_cobol_unit (v: _ #folder) =
  handle v#fold_cobol_unit
    ~continue:begin fun { unit_name; unit_parent_name; unit_env;
                          unit_data; unit_procedure } x -> x
      >> fold_string' v unit_name
      >> fold_string'_opt v unit_parent_name
      >> fold_unit_env v unit_env
      >> fold_data_definitions v unit_data
      >> fold_procedure v unit_procedure
    end

let fold_cobol_unit' (v: _ #folder) =
  handle' v#fold_cobol_unit' ~fold:fold_cobol_unit v

let fold_unit_group (v: _ #folder) =
  handle v#fold_unit_group
    ~continue:(Unit_group.fold (fold_cobol_unit' v))
