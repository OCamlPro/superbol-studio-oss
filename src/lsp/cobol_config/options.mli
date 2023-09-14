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

(** Definition for all options *)
open Types

val tab_width: int value

val text_column: int value

val pic_length: int value

val word_length: int value

val literal_length: int value

val numeric_literal_length: int value

(* any options *)
val defaultbyte: defaultbyte value

val standard_define: standard value

val format: source_format_spec value

val binary_size: binary_size value

val binary_byteorder: binary_byteorder value

val assign_clause: assign_clause value

val screen_section_rules: screen_section_rules value

val dpc_in_data: dpc_in_data value

(* boolean options *)
val filename_mapping: bool value

val pretty_display: bool value

val binary_truncate: bool value

val complex_odo: bool value

val odoslide: bool value

val indirect_redefines: bool value

val relax_syntax_checks: bool value

val ref_mod_zero_length: bool value

val relax_level_hierarchy: bool value

val select_working: bool value

val local_implies_recursive: bool value

val sticky_linkage: bool value

val move_ibm: bool value

val perform_osvs: bool value

val arithmetic_osvs: bool value

val hostsign: bool value

val program_name_redefinition: bool value

val accept_update: bool value

val accept_auto: bool value

val console_is_crt: bool value

val no_echo_means_secure: bool value

val line_col_zero_default: bool value

val display_special_fig_consts: bool value

val binary_comp_1: bool value

val numeric_pointer: bool value

val move_non_numeric_lit_to_numeric_is_zero: bool value

val implicit_assign_dynamic_var: bool value

val device_mnemonics: bool value

val xml_parse_xmlss: bool value

val areacheck: bool value

val ebcdic_symbolic_characters: bool value

(* support options *)
val safe_partial_replacing_when_src_literal: [`Safe | `Unsafe] feature

val comment_paragraphs: unit feature

val control_division: unit feature

val memory_size_clause: unit feature

val multiple_file_tape_clause: unit feature

val label_records_clause: unit feature

val value_of_clause: unit feature

val data_records_clause: unit feature

val top_level_occurs_clause: unit feature

val same_as_clause: unit feature

val type_to_clause: unit feature

val usage_type: unit feature

val synchronized_clause: unit feature

val sync_left_right: unit feature

val special_names_clause: unit feature

val goto_statement_without_name: unit feature

val stop_literal_statement: unit feature

val stop_identifier_statement: unit feature

val stop_error_statement: unit feature

val debugging_mode: unit feature

val use_for_debugging: unit feature

val padding_character_clause: unit feature

val next_sentence_phrase: unit feature

val listing_statements: unit feature

val title_statement: unit feature

val entry_statement: unit feature

val move_noninteger_to_alphanumeric: unit feature

val move_figurative_constant_to_numeric: unit feature

val move_figurative_space_to_numeric: unit feature

val move_figurative_quote_to_numeric: unit feature

val odo_without_to: unit feature

val section_segments: unit feature

val alter_statement: unit feature

val call_overflow: unit feature

val numeric_boolean: unit feature

val hexadecimal_boolean: unit feature

val national_literals: unit feature

val hexadecimal_national_literals: unit feature

val national_character_literals: unit feature

val hp_octal_literals: unit feature

val acu_literals: unit feature

val word_continuation: unit feature

val not_exception_before_exception: unit feature

val accept_display_extensions: unit feature

val larger_redefines: unit feature

val symbolic_constant: unit feature

val constant_78: unit feature

val constant_01: unit feature

val perform_varying_without_by: unit feature

val reference_out_of_declaratives: unit feature

val program_prototypes: unit feature

val call_convention_mnemonic: unit feature

val call_convention_linkage: unit feature

val numeric_value_for_edited_item: unit feature

val incorrect_conf_sec_order: unit feature

val define_constant_directive: unit feature

val free_redefines_position: unit feature

val records_mismatch_record_clause: unit feature

val record_delimiter: unit feature

val sequential_delimiters: unit feature

val record_delim_with_fixed_recs: unit feature

val missing_statement: unit feature

val missing_period: unit feature

val zero_length_literals: unit feature

val xml_generate_extra_phrases: unit feature

val continue_after: unit feature

val goto_entry: unit feature

val assign_variable: unit feature

val assign_using_variable: unit feature

val assign_ext_dyn: unit feature

val assign_disk_from: unit feature

val vsam_status: unit feature

val self_call_recursive: unit feature

val record_contains_depending_clause: unit feature

val picture_l: unit feature
