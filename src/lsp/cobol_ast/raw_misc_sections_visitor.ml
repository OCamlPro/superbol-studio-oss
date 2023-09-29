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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)
open Terms_visitor

let todo    x = Cobol_common.Visitor.todo    __FILE__ x
let partial x = Cobol_common.Visitor.partial __FILE__ x

(* --- *)

module Make = struct

  module Misc_sections_visitor =
    Abstract_visitor.For_misc_sections (Raw.Misc_sections)

  open Misc_descr

  class virtual ['a] folder = object
    inherit ['a] Terms_visitor.folder
    inherit ['a] Misc_sections_visitor.folder
    method fold_informational_paragraph': (informational_paragraph with_loc, 'a) fold = default
    method fold_options_clause: (options_clause, 'a) fold = default
    method fold_configuration_section: (configuration_section, 'a) fold = default
    method fold_configuration_section': (configuration_section with_loc, 'a) fold = default
    method fold_special_names_paragraph: (special_names_paragraph, 'a) fold = default
    method fold_special_names_clause: (special_names_clause, 'a) fold = default
    method fold_special_names_clause': (special_names_clause with_loc, 'a) fold = default
    method fold_repository_paragraph: (repository_paragraph, 'a) fold = default
    method fold_repository_paragraph': (repository_paragraph with_loc, 'a) fold = default
    method fold_specifier: (specifier, 'a) fold = default
    method fold_expands: (expands, 'a) fold = default
    method fold_select: (select, 'a) fold = default
    method fold_select_clause: (select_clause, 'a) fold = default
    method fold_file_control_paragraph: (file_control_paragraph, 'a) fold = default
    method fold_file_control_paragraph': (file_control_paragraph with_loc, 'a) fold = default
    method fold_io_control_paragraph: (io_control_paragraph, 'a) fold = default
    method fold_io_control_paragraph': (io_control_paragraph with_loc, 'a) fold = default
    method fold_io_control_entry: (io_control_entry, 'a) fold = default
    method fold_rerun_clause: (rerun_clause, 'a) fold = default
    method fold_rerun_frequency: (rerun_frequency, 'a) fold = default
    method fold_same_area_clause: (same_area_clause, 'a) fold = default
    method fold_area_source: (area_source, 'a) fold = default
    method fold_multiple_file_clause: (multiple_file_clause, 'a) fold = default
    method fold_file_portion: (file_portion, 'a) fold = default
    method fold_input_output_section: (input_output_section, 'a) fold = default
    method fold_input_output_section': (input_output_section with_loc, 'a) fold = default
    method fold_alphabet_specification: (alphabet_specification, 'a) fold = default
  end

  let todo    x = todo    __MODULE__ x
  and partial x = partial __MODULE__ x

  let fold_options_clause (v: _ #folder) =
    handle v#fold_options_clause
      ~continue:(todo __LINE__ "fold_options_clause")

  let fold_select_clause (v: _ #folder) =
    handle v#fold_select_clause
      ~continue:(todo __LINE__ "fold_select_clause")

  let fold_select (v: _ #folder) =
    handle v#fold_select
      ~continue:begin fun { select_optional; select_name; select_clauses } x -> x
        >> fold_bool v select_optional
        >> fold_name' v select_name
        >> fold_with_loc_list ~fold:fold_select_clause v select_clauses
      end

  let fold_file_control_paragraph (v: _ #folder) =
    handle v#fold_file_control_paragraph
      ~continue:(fold_list ~fold:fold_select v)

  let fold_file_control_paragraph' (v: _ #folder) =
    handle' v#fold_file_control_paragraph' v
      ~fold:fold_file_control_paragraph

  let fold_rerun_frequency (v: _ #folder) =
    handle v#fold_rerun_frequency
      ~continue:begin function
        | RerunEndOf n | RerunCond n -> fold_name' v n
        | RerunRecords (i, n) -> fun x -> x >> fold_integer v i >> fold_name' v n
        | RerunClockUnits i -> fold_integer v i
      end

  let fold_rerun_clause (v: _ #folder) =
    handle v#fold_rerun_clause
      ~continue:begin fun { rerun_on; rerun_every } x -> x
        >> fold_name'_opt v rerun_on
        >> fold_rerun_frequency v rerun_every
      end

  let fold_area_source (v: _ #folder) =
    leaf v#fold_area_source

  let fold_same_area_clause (v: _ #folder) =
    handle v#fold_same_area_clause
      ~continue:begin fun { same_area_source;
                            same_area_file_name;
                            same_area_file_names } x -> x
        >> fold_area_source v same_area_source
        >> fold_name' v same_area_file_name
        >> fold_name'_list v same_area_file_names
      end

  let fold_file_portion (v: _ #folder) =
    handle v#fold_file_portion
      ~continue:begin fun { file_portion_name; file_portion_position } x -> x
        >> fold_name' v file_portion_name
        >> fold_integer_opt v file_portion_position
      end

  let fold_multiple_file_clause (v: _ #folder) =
    handle v#fold_multiple_file_clause
      ~continue:(fold_list ~fold:fold_file_portion v)

  let fold_io_control_entry (v: _ #folder) =
    handle v#fold_io_control_entry
      ~continue:begin fun { io_control_rerun_clauses;
                            io_control_same_area_clauses;
                            io_control_multiple_file_clauses } x -> x
        >> fold_with_loc_list v io_control_rerun_clauses
          ~fold:fold_rerun_clause
        >> fold_with_loc_list v io_control_same_area_clauses
          ~fold:fold_same_area_clause
        >> fold_with_loc_list v io_control_multiple_file_clauses
          ~fold:fold_multiple_file_clause
      end

  let fold_io_control_paragraph (v: _ #folder) =
    handle v#fold_io_control_paragraph
      ~continue:(fold_option ~fold:fold_io_control_entry v)

  let fold_io_control_paragraph' (v: _ #folder) =
    handle' v#fold_io_control_paragraph' v
      ~fold:fold_io_control_paragraph

  let fold_input_output_section (v: _ #folder) =
    handle v#fold_input_output_section
      ~continue:begin fun { file_control_paragraph; io_control_paragraph } x -> x
        >> fold_option v file_control_paragraph
          ~fold:fold_file_control_paragraph'
        >> fold_option v io_control_paragraph
          ~fold:fold_io_control_paragraph'
      end

  let fold_input_output_section' (v: _ #folder) =
    handle' v#fold_input_output_section' v
      ~fold:fold_input_output_section

  (* --- *)

  let fold_informational_paragraph' (v: _ #folder) =
    leaf' v#fold_informational_paragraph' v

  let fold_informational_paragraphs (v: _ #folder) =
    handle v#fold_informational_paragraphs
      ~continue:(fold_list ~fold:fold_informational_paragraph' v)

  let fold_options_paragraph (v: _ #folder) =
    handle v#fold_options_paragraph
      ~continue:(fold_with_loc_list ~fold:fold_options_clause v)

  let fold_expands (v: _ #folder) =
    handle v#fold_expands
      ~continue:begin fun { expands_name; expands_using } x -> x
        >> fold_name' v expands_name
        >> fold_name'_list v expands_using
      end

  let fold_specifier (v: _ #folder) =
    handle v#fold_specifier
      ~continue:begin fun s x -> match s with
        | ClassSpecifier { name; external_name; expands }
        | InterfaceSpecifier { name; external_name; expands } -> x
            >> fold_name' v name
            >> fold_strlit_opt v external_name
            >> fold_option ~fold:fold_expands v expands
        | UserFunctionSpecifier { name; external_name }
        | ProgramSpecifier { name; external_name }
        | PropertySpecifier { name; external_name } -> x
            >> fold_name' v name
            >> fold_strlit_opt v external_name
        | IntrinsicFunctionSpecifier names -> x
            >> fold_name'_list v names
        | IntrinsicFunctionAllSpecifier ->
            x
      end

  let fold_repository_paragraph (v: _ #folder) =
    handle v#fold_repository_paragraph
      ~continue:(fold_list ~fold:fold_specifier v)

  let fold_repository_paragraph' (v: _ #folder) =
    handle' v#fold_repository_paragraph' v
      ~fold:fold_repository_paragraph

  let fold_special_names_clause (v: _ #folder) =
    handle v#fold_special_names_clause
      ~continue:begin fun c x -> match c with
        | DecimalPointIsComma -> x
        | CurrencySign { sign; picture_symbol } -> x
            >> fold_strlit v sign
            >> fold_strlit_opt v picture_symbol
        | _ -> partial __LINE__ "fold_special_names_clause" x
      end

  let fold_special_names_clause' (v: _ #folder) =
    handle' v#fold_special_names_clause' ~fold:fold_special_names_clause v

  let fold_special_names_paragraph (v: _ #folder) =
    handle v#fold_special_names_paragraph
      ~continue:(fold_list ~fold:fold_special_names_clause' v)

  let fold_configuration_section (v: _ #folder) =
    handle v#fold_configuration_section
      ~continue:begin fun { source_computer_paragraph;
                            object_computer_paragraph;
                            special_names_paragraph;
                            repository_paragraph } x ->
        ignore source_computer_paragraph;
        ignore object_computer_paragraph;
        ignore special_names_paragraph;
        x
        >> partial __LINE__ "fold_configuration_section"
        >> fold_option ~fold:fold_repository_paragraph' v repository_paragraph
      end

  let fold_configuration_section' (v: _ #folder) =
    handle' v#fold_configuration_section' v
      ~fold:fold_configuration_section

  let fold_environment_division (v: _ #folder) =
    handle v#fold_environment_division
      ~continue:begin fun { env_configuration; env_input_output } x -> x
        >> fold_option ~fold:fold_configuration_section' v env_configuration
        >> fold_option ~fold:fold_input_output_section' v env_input_output
      end

  let fold_alphabet_specification (v: _ #folder) =
    handle v#fold_alphabet_specification
      ~continue:begin fun { alphanumeric; national } x -> x
        >> fold_option ~fold:fold_name' v alphanumeric
        >> fold_option ~fold:fold_name' v national
      end
end
