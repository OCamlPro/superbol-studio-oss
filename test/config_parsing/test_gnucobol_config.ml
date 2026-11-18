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

let default_conf = Cobol_config.default

let parsed_conf =
  Cobol_common.Diagnostics.show_n_forget @@
  Cobol_config.from_file ~platform ~search_path @@
  Filename.concat confdir "default.conf"

let mf_conf =
  Cobol_common.Diagnostics.show_n_forget @@
  Cobol_config.from_dialect ~platform ~search_path Cobol_dialect.Id.mf_strict

let both_diff s1 s2 =
  StringSet.union
    (StringSet.diff s1 s2)
    (StringSet.diff s2 s1)

let%test_module _ = (module struct
  let intrinsics = both_diff parsed_conf.intrinsic_functions default_conf.intrinsic_functions
  let system_names = both_diff parsed_conf.system_names default_conf.system_names
  let registers = both_diff parsed_conf.registers default_conf.registers

  let diff_words =
    let parsed_words = List.to_seq parsed_conf.words |> StringMap.of_seq
    and default_words = List.to_seq default_conf.words |> StringMap.of_seq in
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
  && default_conf.tab_stops = parsed_conf.tab_stops
  && default_conf.text_column = parsed_conf.text_column
  && default_conf.pic_length = parsed_conf.pic_length
  && default_conf.word_length = parsed_conf.word_length
  && default_conf.literal_length = parsed_conf.literal_length
  && default_conf.numeric_literal_length = parsed_conf.numeric_literal_length

let%test "any values" = true
  && default_conf.defaultbyte = parsed_conf.defaultbyte
  && default_conf.standard_define = parsed_conf.standard_define
  && default_conf.format = parsed_conf.format
  && default_conf.binary_size = parsed_conf.binary_size
  && default_conf.binary_byteorder = parsed_conf.binary_byteorder
  && default_conf.assign_clause = parsed_conf.assign_clause
  && default_conf.screen_section_rules = parsed_conf.screen_section_rules
  && default_conf.dpc_in_data = parsed_conf.dpc_in_data

let%test "boolean values" = true
  && default_conf.filename_mapping = parsed_conf.filename_mapping
  && default_conf.pretty_display = parsed_conf.pretty_display
  && default_conf.binary_truncate = parsed_conf.binary_truncate
  && default_conf.complex_odo = parsed_conf.complex_odo
  && default_conf.odoslide = parsed_conf.odoslide
  && default_conf.indirect_redefines = parsed_conf.indirect_redefines
  && default_conf.relax_syntax_checks = parsed_conf.relax_syntax_checks
  && default_conf.ref_mod_zero_length = parsed_conf.ref_mod_zero_length
  && default_conf.relax_level_hierarchy = parsed_conf.relax_level_hierarchy
  && default_conf.select_working = parsed_conf.select_working
  && default_conf.local_implies_recursive = parsed_conf.local_implies_recursive
  && default_conf.sticky_linkage = parsed_conf.sticky_linkage
  && default_conf.move_ibm = parsed_conf.move_ibm
  && default_conf.perform_osvs = parsed_conf.perform_osvs
  && default_conf.arithmetic_osvs = parsed_conf.arithmetic_osvs
  && default_conf.hostsign = parsed_conf.hostsign
  && default_conf.program_name_redefinition = parsed_conf.program_name_redefinition
  && default_conf.accept_update = parsed_conf.accept_update
  && default_conf.accept_auto = parsed_conf.accept_auto
  && default_conf.console_is_crt = parsed_conf.console_is_crt
  && default_conf.no_echo_means_secure = parsed_conf.no_echo_means_secure
  && default_conf.line_col_zero_default = parsed_conf.line_col_zero_default
  && default_conf.display_special_fig_consts = parsed_conf.display_special_fig_consts
  && default_conf.binary_comp_1 = parsed_conf.binary_comp_1
  && default_conf.numeric_pointer = parsed_conf.numeric_pointer
  && default_conf.move_non_numeric_lit_to_numeric_is_zero = parsed_conf.move_non_numeric_lit_to_numeric_is_zero
  && default_conf.implicit_assign_dynamic_var = parsed_conf.implicit_assign_dynamic_var
  && default_conf.device_mnemonics = parsed_conf.device_mnemonics
  && default_conf.xml_parse_xmlss = parsed_conf.xml_parse_xmlss
  && default_conf.areacheck = parsed_conf.areacheck

let%test "support options" = true
  && default_conf.control_division.level = parsed_conf.control_division.level
  && default_conf.memory_size_clause.level = parsed_conf.memory_size_clause.level
  && default_conf.multiple_file_tape_clause.level = parsed_conf.multiple_file_tape_clause.level
  && default_conf.label_records_clause.level = parsed_conf.label_records_clause.level
  && default_conf.value_of_clause.level = parsed_conf.value_of_clause.level
  && default_conf.data_records_clause.level = parsed_conf.data_records_clause.level
  && default_conf.top_level_occurs_clause.level = parsed_conf.top_level_occurs_clause.level
  && default_conf.same_as_clause.level = parsed_conf.same_as_clause.level
  && default_conf.type_to_clause.level = parsed_conf.type_to_clause.level
  && default_conf.usage_type.level = parsed_conf.usage_type.level
  && default_conf.synchronized_clause.level = parsed_conf.synchronized_clause.level
  && default_conf.sync_left_right.level = parsed_conf.sync_left_right.level
  && default_conf.special_names_clause.level = parsed_conf.special_names_clause.level
  && default_conf.goto_statement_without_name.level = parsed_conf.goto_statement_without_name.level
  && default_conf.stop_literal_statement.level = parsed_conf.stop_literal_statement.level
  && default_conf.stop_identifier_statement.level = parsed_conf.stop_identifier_statement.level
  && default_conf.stop_error_statement.level = parsed_conf.stop_error_statement.level
  && default_conf.debugging_mode.level = parsed_conf.debugging_mode.level
  && default_conf.use_for_debugging.level = parsed_conf.use_for_debugging.level
  && default_conf.padding_character_clause.level = parsed_conf.padding_character_clause.level
  && default_conf.next_sentence_phrase.level = parsed_conf.next_sentence_phrase.level
  && default_conf.listing_statements.level = parsed_conf.listing_statements.level
  && default_conf.title_statement.level = parsed_conf.title_statement.level
  && default_conf.entry_statement.level = parsed_conf.entry_statement.level
  && default_conf.move_noninteger_to_alphanumeric.level = parsed_conf.move_noninteger_to_alphanumeric.level
  && default_conf.move_figurative_constant_to_numeric.level = parsed_conf.move_figurative_constant_to_numeric.level
  && default_conf.move_figurative_space_to_numeric.level = parsed_conf.move_figurative_space_to_numeric.level
  && default_conf.move_figurative_quote_to_numeric.level = parsed_conf.move_figurative_quote_to_numeric.level
  && default_conf.odo_without_to.level = parsed_conf.odo_without_to.level
  && default_conf.section_segments.level = parsed_conf.section_segments.level
  && default_conf.alter_statement.level = parsed_conf.alter_statement.level
  && default_conf.call_overflow.level = parsed_conf.call_overflow.level
  && default_conf.numeric_boolean.level = parsed_conf.numeric_boolean.level
  && default_conf.hexadecimal_boolean.level = parsed_conf.hexadecimal_boolean.level
  && default_conf.national_literals.level = parsed_conf.national_literals.level
  && default_conf.hexadecimal_national_literals.level = parsed_conf.hexadecimal_national_literals.level
  && default_conf.national_character_literals.level = parsed_conf.national_character_literals.level
  && default_conf.hp_octal_literals.level = parsed_conf.hp_octal_literals.level
  && default_conf.acu_literals.level = parsed_conf.acu_literals.level
  && default_conf.word_continuation.level = parsed_conf.word_continuation.level
  && default_conf.not_exception_before_exception.level = parsed_conf.not_exception_before_exception.level
  && default_conf.accept_display_extensions.level = parsed_conf.accept_display_extensions.level
  && default_conf.larger_redefines.level = parsed_conf.larger_redefines.level
  && default_conf.symbolic_constant.level = parsed_conf.symbolic_constant.level
  && default_conf.constant_78.level = parsed_conf.constant_78.level
  && default_conf.constant_01.level = parsed_conf.constant_01.level
  && default_conf.perform_varying_without_by.level = parsed_conf.perform_varying_without_by.level
  && default_conf.reference_out_of_declaratives.level = parsed_conf.reference_out_of_declaratives.level
  && default_conf.program_prototypes.level = parsed_conf.program_prototypes.level
  && default_conf.call_convention_mnemonic.level = parsed_conf.call_convention_mnemonic.level
  && default_conf.call_convention_linkage.level = parsed_conf.call_convention_linkage.level
  && default_conf.numeric_value_for_edited_item.level = parsed_conf.numeric_value_for_edited_item.level
  && default_conf.incorrect_conf_sec_order.level = parsed_conf.incorrect_conf_sec_order.level
  && default_conf.define_constant_directive.level = parsed_conf.define_constant_directive.level
  && default_conf.free_redefines_position.level = parsed_conf.free_redefines_position.level
  && default_conf.records_mismatch_record_clause.level = parsed_conf.records_mismatch_record_clause.level
  && default_conf.record_delimiter.level = parsed_conf.record_delimiter.level
  && default_conf.sequential_delimiters.level = parsed_conf.sequential_delimiters.level
  && default_conf.record_delim_with_fixed_recs.level = parsed_conf.record_delim_with_fixed_recs.level
  && default_conf.missing_statement.level = parsed_conf.missing_statement.level
  && default_conf.missing_period.level = parsed_conf.missing_period.level
  && default_conf.zero_length_literals.level = parsed_conf.zero_length_literals.level
  && default_conf.xml_generate_extra_phrases.level = parsed_conf.xml_generate_extra_phrases.level
  && default_conf.continue_after.level = parsed_conf.continue_after.level
  && default_conf.goto_entry.level = parsed_conf.goto_entry.level
  && default_conf.assign_variable.level = parsed_conf.assign_variable.level
  && default_conf.assign_using_variable.level = parsed_conf.assign_using_variable.level
  && default_conf.assign_ext_dyn.level = parsed_conf.assign_ext_dyn.level
  && default_conf.assign_disk_from.level = parsed_conf.assign_disk_from.level
  && default_conf.vsam_status.level = parsed_conf.vsam_status.level
  && default_conf.self_call_recursive.level = parsed_conf.self_call_recursive.level
  && default_conf.record_contains_depending_clause.level = parsed_conf.record_contains_depending_clause.level
  && default_conf.picture_l.level = parsed_conf.picture_l.level

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
  (load_conf (Printf.sprintf "include \"default.conf\"\ntab-width: %s\n"
                value)).tab_stops

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
