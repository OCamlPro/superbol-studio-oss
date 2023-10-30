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
let search_path = Lazy.force Cobol_config.default_search_path

module Default_conf =
  (val Cobol_config.default)

module Parsed_conf =
  (val (Cobol_common.Diagnostics.show_n_forget @@
        Cobol_config.from_file ~search_path @@
        Filename.concat confdir "default.conf"))

module MF_conf =
  (val (Cobol_common.Diagnostics.show_n_forget @@
        Cobol_config.(from_dialect ~search_path DIALECT.mf_strict)))

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
  && Default_conf.tab_width#value = Parsed_conf.tab_width#value
  && Default_conf.text_column#value = Parsed_conf.text_column#value
  && Default_conf.pic_length#value = Parsed_conf.pic_length#value
  && Default_conf.word_length#value = Parsed_conf.word_length#value
  && Default_conf.literal_length#value = Parsed_conf.literal_length#value
  && Default_conf.numeric_literal_length#value = Parsed_conf.numeric_literal_length#value

let%test "any values" = true
  && Default_conf.defaultbyte#value = Parsed_conf.defaultbyte#value
  && Default_conf.standard_define#value = Parsed_conf.standard_define#value
  && Default_conf.format#value = Parsed_conf.format#value
  && Default_conf.binary_size#value = Parsed_conf.binary_size#value
  && Default_conf.binary_byteorder#value = Parsed_conf.binary_byteorder#value
  && Default_conf.assign_clause#value = Parsed_conf.assign_clause#value
  && Default_conf.screen_section_rules#value = Parsed_conf.screen_section_rules#value
  && Default_conf.dpc_in_data#value = Parsed_conf.dpc_in_data#value

let%test "boolean values" = true
  && Default_conf.filename_mapping#value = Parsed_conf.filename_mapping#value
  && Default_conf.pretty_display#value = Parsed_conf.pretty_display#value
  && Default_conf.binary_truncate#value = Parsed_conf.binary_truncate#value
  && Default_conf.complex_odo#value = Parsed_conf.complex_odo#value
  && Default_conf.odoslide#value = Parsed_conf.odoslide#value
  && Default_conf.indirect_redefines#value = Parsed_conf.indirect_redefines#value
  && Default_conf.relax_syntax_checks#value = Parsed_conf.relax_syntax_checks#value
  && Default_conf.ref_mod_zero_length#value = Parsed_conf.ref_mod_zero_length#value
  && Default_conf.relax_level_hierarchy#value = Parsed_conf.relax_level_hierarchy#value
  && Default_conf.select_working#value = Parsed_conf.select_working#value
  && Default_conf.local_implies_recursive#value = Parsed_conf.local_implies_recursive#value
  && Default_conf.sticky_linkage#value = Parsed_conf.sticky_linkage#value
  && Default_conf.move_ibm#value = Parsed_conf.move_ibm#value
  && Default_conf.perform_osvs#value = Parsed_conf.perform_osvs#value
  && Default_conf.arithmetic_osvs#value = Parsed_conf.arithmetic_osvs#value
  && Default_conf.hostsign#value = Parsed_conf.hostsign#value
  && Default_conf.program_name_redefinition#value = Parsed_conf.program_name_redefinition#value
  && Default_conf.accept_update#value = Parsed_conf.accept_update#value
  && Default_conf.accept_auto#value = Parsed_conf.accept_auto#value
  && Default_conf.console_is_crt#value = Parsed_conf.console_is_crt#value
  && Default_conf.no_echo_means_secure#value = Parsed_conf.no_echo_means_secure#value
  && Default_conf.line_col_zero_default#value = Parsed_conf.line_col_zero_default#value
  && Default_conf.display_special_fig_consts#value = Parsed_conf.display_special_fig_consts#value
  && Default_conf.binary_comp_1#value = Parsed_conf.binary_comp_1#value
  && Default_conf.numeric_pointer#value = Parsed_conf.numeric_pointer#value
  && Default_conf.move_non_numeric_lit_to_numeric_is_zero#value = Parsed_conf.move_non_numeric_lit_to_numeric_is_zero#value
  && Default_conf.implicit_assign_dynamic_var#value = Parsed_conf.implicit_assign_dynamic_var#value
  && Default_conf.device_mnemonics#value = Parsed_conf.device_mnemonics#value
  && Default_conf.xml_parse_xmlss#value = Parsed_conf.xml_parse_xmlss#value
  && Default_conf.areacheck#value = Parsed_conf.areacheck#value

let%test "support options" = true
  && Default_conf.control_division#level = Parsed_conf.control_division#level
  && Default_conf.memory_size_clause#level = Parsed_conf.memory_size_clause#level
  && Default_conf.multiple_file_tape_clause#level = Parsed_conf.multiple_file_tape_clause#level
  && Default_conf.label_records_clause#level = Parsed_conf.label_records_clause#level
  && Default_conf.value_of_clause#level = Parsed_conf.value_of_clause#level
  && Default_conf.data_records_clause#level = Parsed_conf.data_records_clause#level
  && Default_conf.top_level_occurs_clause#level = Parsed_conf.top_level_occurs_clause#level
  && Default_conf.same_as_clause#level = Parsed_conf.same_as_clause#level
  && Default_conf.type_to_clause#level = Parsed_conf.type_to_clause#level
  && Default_conf.usage_type#level = Parsed_conf.usage_type#level
  && Default_conf.synchronized_clause#level = Parsed_conf.synchronized_clause#level
  && Default_conf.sync_left_right#level = Parsed_conf.sync_left_right#level
  && Default_conf.special_names_clause#level = Parsed_conf.special_names_clause#level
  && Default_conf.goto_statement_without_name#level = Parsed_conf.goto_statement_without_name#level
  && Default_conf.stop_literal_statement#level = Parsed_conf.stop_literal_statement#level
  && Default_conf.stop_identifier_statement#level = Parsed_conf.stop_identifier_statement#level
  && Default_conf.stop_error_statement#level = Parsed_conf.stop_error_statement#level
  && Default_conf.debugging_mode#level = Parsed_conf.debugging_mode#level
  && Default_conf.use_for_debugging#level = Parsed_conf.use_for_debugging#level
  && Default_conf.padding_character_clause#level = Parsed_conf.padding_character_clause#level
  && Default_conf.next_sentence_phrase#level = Parsed_conf.next_sentence_phrase#level
  && Default_conf.listing_statements#level = Parsed_conf.listing_statements#level
  && Default_conf.title_statement#level = Parsed_conf.title_statement#level
  && Default_conf.entry_statement#level = Parsed_conf.entry_statement#level
  && Default_conf.move_noninteger_to_alphanumeric#level = Parsed_conf.move_noninteger_to_alphanumeric#level
  && Default_conf.move_figurative_constant_to_numeric#level = Parsed_conf.move_figurative_constant_to_numeric#level
  && Default_conf.move_figurative_space_to_numeric#level = Parsed_conf.move_figurative_space_to_numeric#level
  && Default_conf.move_figurative_quote_to_numeric#level = Parsed_conf.move_figurative_quote_to_numeric#level
  && Default_conf.odo_without_to#level = Parsed_conf.odo_without_to#level
  && Default_conf.section_segments#level = Parsed_conf.section_segments#level
  && Default_conf.alter_statement#level = Parsed_conf.alter_statement#level
  && Default_conf.call_overflow#level = Parsed_conf.call_overflow#level
  && Default_conf.numeric_boolean#level = Parsed_conf.numeric_boolean#level
  && Default_conf.hexadecimal_boolean#level = Parsed_conf.hexadecimal_boolean#level
  && Default_conf.national_literals#level = Parsed_conf.national_literals#level
  && Default_conf.hexadecimal_national_literals#level = Parsed_conf.hexadecimal_national_literals#level
  && Default_conf.national_character_literals#level = Parsed_conf.national_character_literals#level
  && Default_conf.hp_octal_literals#level = Parsed_conf.hp_octal_literals#level
  && Default_conf.acu_literals#level = Parsed_conf.acu_literals#level
  && Default_conf.word_continuation#level = Parsed_conf.word_continuation#level
  && Default_conf.not_exception_before_exception#level = Parsed_conf.not_exception_before_exception#level
  && Default_conf.accept_display_extensions#level = Parsed_conf.accept_display_extensions#level
  && Default_conf.larger_redefines#level = Parsed_conf.larger_redefines#level
  && Default_conf.symbolic_constant#level = Parsed_conf.symbolic_constant#level
  && Default_conf.constant_78#level = Parsed_conf.constant_78#level
  && Default_conf.constant_01#level = Parsed_conf.constant_01#level
  && Default_conf.perform_varying_without_by#level = Parsed_conf.perform_varying_without_by#level
  && Default_conf.reference_out_of_declaratives#level = Parsed_conf.reference_out_of_declaratives#level
  && Default_conf.program_prototypes#level = Parsed_conf.program_prototypes#level
  && Default_conf.call_convention_mnemonic#level = Parsed_conf.call_convention_mnemonic#level
  && Default_conf.call_convention_linkage#level = Parsed_conf.call_convention_linkage#level
  && Default_conf.numeric_value_for_edited_item#level = Parsed_conf.numeric_value_for_edited_item#level
  && Default_conf.incorrect_conf_sec_order#level = Parsed_conf.incorrect_conf_sec_order#level
  && Default_conf.define_constant_directive#level = Parsed_conf.define_constant_directive#level
  && Default_conf.free_redefines_position#level = Parsed_conf.free_redefines_position#level
  && Default_conf.records_mismatch_record_clause#level = Parsed_conf.records_mismatch_record_clause#level
  && Default_conf.record_delimiter#level = Parsed_conf.record_delimiter#level
  && Default_conf.sequential_delimiters#level = Parsed_conf.sequential_delimiters#level
  && Default_conf.record_delim_with_fixed_recs#level = Parsed_conf.record_delim_with_fixed_recs#level
  && Default_conf.missing_statement#level = Parsed_conf.missing_statement#level
  && Default_conf.missing_period#level = Parsed_conf.missing_period#level
  && Default_conf.zero_length_literals#level = Parsed_conf.zero_length_literals#level
  && Default_conf.xml_generate_extra_phrases#level = Parsed_conf.xml_generate_extra_phrases#level
  && Default_conf.continue_after#level = Parsed_conf.continue_after#level
  && Default_conf.goto_entry#level = Parsed_conf.goto_entry#level
  && Default_conf.assign_variable#level = Parsed_conf.assign_variable#level
  && Default_conf.assign_using_variable#level = Parsed_conf.assign_using_variable#level
  && Default_conf.assign_ext_dyn#level = Parsed_conf.assign_ext_dyn#level
  && Default_conf.assign_disk_from#level = Parsed_conf.assign_disk_from#level
  && Default_conf.vsam_status#level = Parsed_conf.vsam_status#level
  && Default_conf.self_call_recursive#level = Parsed_conf.self_call_recursive#level
  && Default_conf.record_contains_depending_clause#level = Parsed_conf.record_contains_depending_clause#level
  && Default_conf.picture_l#level = Parsed_conf.picture_l#level
