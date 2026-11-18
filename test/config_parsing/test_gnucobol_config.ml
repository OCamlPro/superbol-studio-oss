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

open EzCompat

let srcdir = Testsuite_utils.srcdir     (* ../output-tests/testsuite_utils.ml *)
let confdir = Testsuite_utils.confdir
let platform = Testsuite_utils.platform
let search_path = Cobol_config.default_search_path ~platform

module Default_conf =
  (val Cobol_config.default)

module Parsed_conf =
  (val (Cobol_common.Diagnostics.show_n_forget @@
        Cobol_config.from_file ~platform ~search_path @@
        Filename.concat confdir "default.conf"))

module MF_conf =
  (val (Cobol_common.Diagnostics.show_n_forget @@
        Cobol_config.from_dialect ~platform ~search_path
          Cobol_dialect.Id.mf_strict))

let both_diff s1 s2 =
  StringSet.union
    (StringSet.diff s1 s2)
    (StringSet.diff s2 s1)

let%test_module _ = (module struct
  let intrinsics = both_diff Parsed_conf.intrinsic_functions Default_conf.intrinsic_functions
  let system_names = both_diff Parsed_conf.system_names Default_conf.system_names
  let registers = both_diff Parsed_conf.registers Default_conf.registers

  let diff_words =
    let parsed_words = List.to_seq Parsed_conf.words |> StringMap.of_seq
    and default_words = List.to_seq Default_conf.words |> StringMap.of_seq in
    StringMap.merge begin fun _ p d -> match d, p with
      | Some _, None -> Some "-"
      | None, Some _ -> Some "+"
      | _ -> None
    end parsed_words default_words;;
  StringMap.iter (Pretty.error "%s: %s@.") diff_words;;

  let%test "words" = StringMap.is_empty diff_words
  let%test "intrinsic_functions" = StringSet.is_empty intrinsics
  let%test "system_names" = StringSet.is_empty system_names
  let%test "registers" = StringSet.is_empty registers
end)

let%test "int values" = true
  && Default_conf.tab_stops = Parsed_conf.tab_stops
  && Default_conf.text_column = Parsed_conf.text_column
  && Default_conf.pic_length = Parsed_conf.pic_length
  && Default_conf.word_length = Parsed_conf.word_length
  && Default_conf.literal_length = Parsed_conf.literal_length
  && Default_conf.numeric_literal_length = Parsed_conf.numeric_literal_length

let%test "any values" = true
  && Default_conf.defaultbyte = Parsed_conf.defaultbyte
  && Default_conf.standard_define = Parsed_conf.standard_define
  && Default_conf.format = Parsed_conf.format
  && Default_conf.binary_size = Parsed_conf.binary_size
  && Default_conf.binary_byteorder = Parsed_conf.binary_byteorder
  && Default_conf.assign_clause = Parsed_conf.assign_clause
  && Default_conf.screen_section_rules = Parsed_conf.screen_section_rules
  && Default_conf.dpc_in_data = Parsed_conf.dpc_in_data

let%test "boolean values" = true
  && Default_conf.filename_mapping = Parsed_conf.filename_mapping
  && Default_conf.pretty_display = Parsed_conf.pretty_display
  && Default_conf.binary_truncate = Parsed_conf.binary_truncate
  && Default_conf.complex_odo = Parsed_conf.complex_odo
  && Default_conf.odoslide = Parsed_conf.odoslide
  && Default_conf.indirect_redefines = Parsed_conf.indirect_redefines
  && Default_conf.relax_syntax_checks = Parsed_conf.relax_syntax_checks
  && Default_conf.ref_mod_zero_length = Parsed_conf.ref_mod_zero_length
  && Default_conf.relax_level_hierarchy = Parsed_conf.relax_level_hierarchy
  && Default_conf.select_working = Parsed_conf.select_working
  && Default_conf.local_implies_recursive = Parsed_conf.local_implies_recursive
  && Default_conf.sticky_linkage = Parsed_conf.sticky_linkage
  && Default_conf.move_ibm = Parsed_conf.move_ibm
  && Default_conf.perform_osvs = Parsed_conf.perform_osvs
  && Default_conf.arithmetic_osvs = Parsed_conf.arithmetic_osvs
  && Default_conf.hostsign = Parsed_conf.hostsign
  && Default_conf.program_name_redefinition = Parsed_conf.program_name_redefinition
  && Default_conf.accept_update = Parsed_conf.accept_update
  && Default_conf.accept_auto = Parsed_conf.accept_auto
  && Default_conf.console_is_crt = Parsed_conf.console_is_crt
  && Default_conf.no_echo_means_secure = Parsed_conf.no_echo_means_secure
  && Default_conf.line_col_zero_default = Parsed_conf.line_col_zero_default
  && Default_conf.display_special_fig_consts = Parsed_conf.display_special_fig_consts
  && Default_conf.binary_comp_1 = Parsed_conf.binary_comp_1
  && Default_conf.numeric_pointer = Parsed_conf.numeric_pointer
  && Default_conf.move_non_numeric_lit_to_numeric_is_zero = Parsed_conf.move_non_numeric_lit_to_numeric_is_zero
  && Default_conf.implicit_assign_dynamic_var = Parsed_conf.implicit_assign_dynamic_var
  && Default_conf.device_mnemonics = Parsed_conf.device_mnemonics
  && Default_conf.xml_parse_xmlss = Parsed_conf.xml_parse_xmlss
  && Default_conf.areacheck = Parsed_conf.areacheck

let%test "support options" = true
  && Default_conf.control_division.level = Parsed_conf.control_division.level
  && Default_conf.memory_size_clause.level = Parsed_conf.memory_size_clause.level
  && Default_conf.multiple_file_tape_clause.level = Parsed_conf.multiple_file_tape_clause.level
  && Default_conf.label_records_clause.level = Parsed_conf.label_records_clause.level
  && Default_conf.value_of_clause.level = Parsed_conf.value_of_clause.level
  && Default_conf.data_records_clause.level = Parsed_conf.data_records_clause.level
  && Default_conf.top_level_occurs_clause.level = Parsed_conf.top_level_occurs_clause.level
  && Default_conf.same_as_clause.level = Parsed_conf.same_as_clause.level
  && Default_conf.type_to_clause.level = Parsed_conf.type_to_clause.level
  && Default_conf.usage_type.level = Parsed_conf.usage_type.level
  && Default_conf.synchronized_clause.level = Parsed_conf.synchronized_clause.level
  && Default_conf.sync_left_right.level = Parsed_conf.sync_left_right.level
  && Default_conf.special_names_clause.level = Parsed_conf.special_names_clause.level
  && Default_conf.goto_statement_without_name.level = Parsed_conf.goto_statement_without_name.level
  && Default_conf.stop_literal_statement.level = Parsed_conf.stop_literal_statement.level
  && Default_conf.stop_identifier_statement.level = Parsed_conf.stop_identifier_statement.level
  && Default_conf.stop_error_statement.level = Parsed_conf.stop_error_statement.level
  && Default_conf.debugging_mode.level = Parsed_conf.debugging_mode.level
  && Default_conf.use_for_debugging.level = Parsed_conf.use_for_debugging.level
  && Default_conf.padding_character_clause.level = Parsed_conf.padding_character_clause.level
  && Default_conf.next_sentence_phrase.level = Parsed_conf.next_sentence_phrase.level
  && Default_conf.listing_statements.level = Parsed_conf.listing_statements.level
  && Default_conf.title_statement.level = Parsed_conf.title_statement.level
  && Default_conf.entry_statement.level = Parsed_conf.entry_statement.level
  && Default_conf.move_noninteger_to_alphanumeric.level = Parsed_conf.move_noninteger_to_alphanumeric.level
  && Default_conf.move_figurative_constant_to_numeric.level = Parsed_conf.move_figurative_constant_to_numeric.level
  && Default_conf.move_figurative_space_to_numeric.level = Parsed_conf.move_figurative_space_to_numeric.level
  && Default_conf.move_figurative_quote_to_numeric.level = Parsed_conf.move_figurative_quote_to_numeric.level
  && Default_conf.odo_without_to.level = Parsed_conf.odo_without_to.level
  && Default_conf.section_segments.level = Parsed_conf.section_segments.level
  && Default_conf.alter_statement.level = Parsed_conf.alter_statement.level
  && Default_conf.call_overflow.level = Parsed_conf.call_overflow.level
  && Default_conf.numeric_boolean.level = Parsed_conf.numeric_boolean.level
  && Default_conf.hexadecimal_boolean.level = Parsed_conf.hexadecimal_boolean.level
  && Default_conf.national_literals.level = Parsed_conf.national_literals.level
  && Default_conf.hexadecimal_national_literals.level = Parsed_conf.hexadecimal_national_literals.level
  && Default_conf.national_character_literals.level = Parsed_conf.national_character_literals.level
  && Default_conf.hp_octal_literals.level = Parsed_conf.hp_octal_literals.level
  && Default_conf.acu_literals.level = Parsed_conf.acu_literals.level
  && Default_conf.word_continuation.level = Parsed_conf.word_continuation.level
  && Default_conf.not_exception_before_exception.level = Parsed_conf.not_exception_before_exception.level
  && Default_conf.accept_display_extensions.level = Parsed_conf.accept_display_extensions.level
  && Default_conf.larger_redefines.level = Parsed_conf.larger_redefines.level
  && Default_conf.symbolic_constant.level = Parsed_conf.symbolic_constant.level
  && Default_conf.constant_78.level = Parsed_conf.constant_78.level
  && Default_conf.constant_01.level = Parsed_conf.constant_01.level
  && Default_conf.perform_varying_without_by.level = Parsed_conf.perform_varying_without_by.level
  && Default_conf.reference_out_of_declaratives.level = Parsed_conf.reference_out_of_declaratives.level
  && Default_conf.program_prototypes.level = Parsed_conf.program_prototypes.level
  && Default_conf.call_convention_mnemonic.level = Parsed_conf.call_convention_mnemonic.level
  && Default_conf.call_convention_linkage.level = Parsed_conf.call_convention_linkage.level
  && Default_conf.numeric_value_for_edited_item.level = Parsed_conf.numeric_value_for_edited_item.level
  && Default_conf.incorrect_conf_sec_order.level = Parsed_conf.incorrect_conf_sec_order.level
  && Default_conf.define_constant_directive.level = Parsed_conf.define_constant_directive.level
  && Default_conf.free_redefines_position.level = Parsed_conf.free_redefines_position.level
  && Default_conf.records_mismatch_record_clause.level = Parsed_conf.records_mismatch_record_clause.level
  && Default_conf.record_delimiter.level = Parsed_conf.record_delimiter.level
  && Default_conf.sequential_delimiters.level = Parsed_conf.sequential_delimiters.level
  && Default_conf.record_delim_with_fixed_recs.level = Parsed_conf.record_delim_with_fixed_recs.level
  && Default_conf.missing_statement.level = Parsed_conf.missing_statement.level
  && Default_conf.missing_period.level = Parsed_conf.missing_period.level
  && Default_conf.zero_length_literals.level = Parsed_conf.zero_length_literals.level
  && Default_conf.xml_generate_extra_phrases.level = Parsed_conf.xml_generate_extra_phrases.level
  && Default_conf.continue_after.level = Parsed_conf.continue_after.level
  && Default_conf.goto_entry.level = Parsed_conf.goto_entry.level
  && Default_conf.assign_variable.level = Parsed_conf.assign_variable.level
  && Default_conf.assign_using_variable.level = Parsed_conf.assign_using_variable.level
  && Default_conf.assign_ext_dyn.level = Parsed_conf.assign_ext_dyn.level
  && Default_conf.assign_disk_from.level = Parsed_conf.assign_disk_from.level
  && Default_conf.vsam_status.level = Parsed_conf.vsam_status.level
  && Default_conf.self_call_recursive.level = Parsed_conf.self_call_recursive.level
  && Default_conf.record_contains_depending_clause.level = Parsed_conf.record_contains_depending_clause.level
  && Default_conf.picture_l.level = Parsed_conf.picture_l.level

module Tab_stops = Cobol_common.Tab_stops

let%test "tab width values" = true
  && Tab_stops.of_string "8" = [8]
  && Tab_stops.of_string "6,1,8" = [6;1;8]
  && Tab_stops.of_string " 4 , 2 " = [4;2]

let%test "tab width validity" = true
  && Tab_stops.is_valid [8]
  && Tab_stops.is_valid [6;1;8]
  && Tab_stops.is_valid [4;2]
  && not (Tab_stops.is_valid [])
  && not (Tab_stops.is_valid [0])
  && not (Tab_stops.is_valid [-4])
  && not (Tab_stops.is_valid [4;0])

let%test "tab stops" = true
  && Tab_stops.next_stop ~tab_stops:[8] 1 = 9
  && Tab_stops.next_stop ~tab_stops:[4] 1 = 5
  && Tab_stops.next_stop ~tab_stops:[4] 5 = 9
  && Tab_stops.next_stop ~tab_stops:[4;2] 1 = 5
  && Tab_stops.next_stop ~tab_stops:[4;2] 5 = 7
  && Tab_stops.next_stop ~tab_stops:[6;1;8] 1 = 7
  && Tab_stops.next_stop ~tab_stops:[6;1;8] 7 = 8
  && Tab_stops.next_stop ~tab_stops:[6;1;8] 8 = 16

let load_conf contents =
  let file = Filename.temp_file "superbol_tab_stops" ".conf" in
  let oc = open_out file in
  output_string oc contents;
  close_out oc;
  let search_path = Filename.dirname file :: confdir :: search_path in
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  Cobol_common.Diagnostics.show_n_forget @@
  Cobol_config.from_file ~platform ~search_path (Filename.basename file)

let tab_stops_of_conf value =
  let module Conf =
    (val load_conf (Printf.sprintf "include \"default.conf\"\ntab-width: %s\n"
                      value))
  in
  Conf.tab_stops

let rejected value =
  match tab_stops_of_conf value with
  | _ -> false
  | exception
      Cobol_config.ERROR (Invalid_key_value_pair ("tab-width", _)) -> true

let%test "tab width from configuration" = true
  && tab_stops_of_conf "4" = [4]
  && tab_stops_of_conf "6,1,8" = [6;1;8]
  && tab_stops_of_conf "4, 2" = [4;2]
  && tab_stops_of_conf "\"6,1,8\"" = [6;1;8]      (* former only spelling *)

let%test "invalid tab width from configuration" = true
  && rejected "0"
  && rejected "-4"
  && rejected "4,0"
