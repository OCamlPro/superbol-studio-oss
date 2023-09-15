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

(** Module containing all the default options *)
open EzCompat

let not_reserved =
  ["TERMINAL"; "EXAMINE"]

let default_aliases =
  [ "AUTO-SKIP", "AUTO";
    "AUTOTERMINATE", "AUTO";
    "BACKGROUND-COLOUR", "BACKGROUND-COLOR";
    "BEEP", "BELL";
    "BINARY-INT", "BINARY-LONG";
    "BINARY-LONG-LONG", "BINARY-DOUBLE";
    "CELLS", "CELL";
    "COLOURS", "COLORS";
    "EMPTY-CHECK", "REQUIRED";
    "EQUALS", "EQUAL";
    "FOREGROUND-COLOUR", "FOREGROUND-COLOR";
    "HIGH-VALUES", "HIGH-VALUE";
    "INITIALISE", "INITIALIZE";
    "INITIALISED", "INITIALIZED";
    "LENGTH-CHECK", "FULL";
    "LOW-VALUES", "LOW-VALUE";
    "ORGANISATION", "ORGANIZATION";
    "PIXELS", "PIXEL";
    "SYNCHRONISED", "SYNCHRONIZED";
    "TIMEOUT", "TIME-OUT";
    "ZEROES", "ZERO";
    "ZEROS", "ZERO"]

module Default: Types.T = struct
  open Types
  let dialect = DIALECT.Default
  let config = { name = DIALECT.name dialect }

  let words =
    let alias_for b = ReserveAlias { alias_for = b;
                                     preserve_context_sensitivity = false } in
    Reserved_words.words @
    List.map (fun w -> w, NotReserved) not_reserved @
    List.map (fun (a, b) -> a, alias_for b) default_aliases

  let not_reserved = StringSet.of_list not_reserved
  let intrinsic_functions = StringSet.diff Reserved_words.default_intrinsics not_reserved
  let system_names = StringSet.diff Reserved_words.default_system_names not_reserved
  let registers = StringSet.diff Reserved_words.default_registers not_reserved

  (* int options *)
  open Options
  let tab_width
    = tab_width#from_val ~config 8
  let text_column
    = text_column#from_val ~config 72
  let pic_length
    = pic_length#from_val ~config 255
  let word_length
    = word_length#from_val ~config 63
  let literal_length
    = literal_length#from_val ~config 8191
  let numeric_literal_length
    = numeric_literal_length#from_val ~config 38

  (* any options *)
  let defaultbyte
    = defaultbyte#from_val ~config Init
  let standard_define
    = standard_define#from_val ~config GnuCOBOL
  let format
    = format#from_val ~config Auto
  let binary_size
    = binary_size#from_val ~config B_1_2_4_8
  let binary_byteorder
    = binary_byteorder#from_val ~config Big_endian
  let assign_clause
    = assign_clause#from_val ~config Dynamic
  let screen_section_rules
    = screen_section_rules#from_val ~config GC
  let dpc_in_data
    = dpc_in_data#from_val ~config XML

  (* boolean options *)
  let filename_mapping
    = filename_mapping#from_val ~config true
  let pretty_display
    = pretty_display#from_val ~config true
  let binary_truncate
    = binary_truncate#from_val ~config true
  let complex_odo
    = complex_odo#from_val ~config false
  let odoslide
    = odoslide#from_val ~config false
  let indirect_redefines
    = indirect_redefines#from_val ~config false
  let relax_syntax_checks
    = relax_syntax_checks#from_val ~config false
  let ref_mod_zero_length
    = ref_mod_zero_length#from_val ~config true
  let relax_level_hierarchy
    = relax_level_hierarchy#from_val ~config false
  let select_working
    = select_working#from_val ~config false
  let local_implies_recursive
    = local_implies_recursive#from_val ~config false
  let sticky_linkage
    = sticky_linkage#from_val ~config false
  let move_ibm
    = move_ibm#from_val ~config false
  let perform_osvs
    = perform_osvs#from_val ~config false
  let arithmetic_osvs
    = arithmetic_osvs#from_val ~config false
  let hostsign
    = hostsign#from_val ~config false
  let program_name_redefinition
    = program_name_redefinition#from_val ~config true
  let accept_update
    = accept_update#from_val ~config false
  let accept_auto
    = accept_auto#from_val ~config false
  let console_is_crt
    = console_is_crt#from_val ~config  false
  let no_echo_means_secure
    = no_echo_means_secure#from_val ~config false
  let line_col_zero_default
    = line_col_zero_default#from_val ~config true
  let display_special_fig_consts
    = display_special_fig_consts#from_val ~config false
  let binary_comp_1
    = binary_comp_1#from_val ~config false
  let numeric_pointer
    = numeric_pointer#from_val ~config false
  let move_non_numeric_lit_to_numeric_is_zero
    = move_non_numeric_lit_to_numeric_is_zero#from_val ~config false
  let implicit_assign_dynamic_var
    = implicit_assign_dynamic_var#from_val ~config true
  let device_mnemonics
    = device_mnemonics#from_val ~config false
  let xml_parse_xmlss
    = xml_parse_xmlss#from_val ~config true
  let areacheck
    = areacheck#from_val ~config false
  let ebcdic_symbolic_characters
    = ebcdic_symbolic_characters#from_val ~config false

  (* support options *)
  let comment_paragraphs
    = comment_paragraphs#from_level ~config @@ Obsolete ()
  let safe_partial_replacing_when_src_literal
    = safe_partial_replacing_when_src_literal#from_level ~config @@ Obsolete `Unsafe
  let control_division
    = control_division#from_level ~config Unconformable
  let memory_size_clause
    = memory_size_clause#from_level ~config @@ Obsolete ()
  let multiple_file_tape_clause
    = multiple_file_tape_clause#from_level ~config @@ Obsolete ()
  let label_records_clause
    = label_records_clause#from_level ~config @@ Obsolete ()
  let value_of_clause
    = value_of_clause#from_level ~config @@ Obsolete ()
  let data_records_clause
    = data_records_clause#from_level ~config @@ Obsolete ()
  let top_level_occurs_clause
    = top_level_occurs_clause#from_level ~config @@ Ok ()
  let same_as_clause
    = same_as_clause#from_level ~config @@ Ok ()
  let type_to_clause
    = type_to_clause#from_level ~config @@ Ok ()
  let usage_type
    = usage_type#from_level ~config @@ Ok ()
  let synchronized_clause
    = synchronized_clause#from_level ~config @@ Ok ()
  let sync_left_right
    = sync_left_right#from_level ~config @@ Ok ()
  let special_names_clause
    = special_names_clause#from_level ~config @@ Ok ()
  let goto_statement_without_name
    = goto_statement_without_name#from_level ~config @@ Obsolete ()
  let stop_literal_statement
    = stop_literal_statement#from_level ~config @@ Obsolete ()
  let stop_identifier_statement
    = stop_identifier_statement#from_level ~config @@ Obsolete ()
  let stop_error_statement
    = stop_error_statement#from_level ~config Unconformable
  let debugging_mode
    = debugging_mode#from_level ~config @@ Ok ()
  let use_for_debugging
    = use_for_debugging#from_level ~config @@ Ok ()
  let padding_character_clause
    = padding_character_clause#from_level ~config  @@ Obsolete ()
  let next_sentence_phrase
    = next_sentence_phrase#from_level ~config @@ Archaic ()
  let listing_statements
    = listing_statements#from_level ~config Skip
  let title_statement
    = title_statement#from_level ~config Skip
  let entry_statement
    = entry_statement#from_level ~config @@ Ok ()
  let move_noninteger_to_alphanumeric
    = move_noninteger_to_alphanumeric#from_level ~config Error
  let move_figurative_constant_to_numeric
    = move_figurative_constant_to_numeric#from_level ~config @@ Archaic ()
  let move_figurative_space_to_numeric
    = move_figurative_space_to_numeric#from_level ~config Error
  let move_figurative_quote_to_numeric
    = move_figurative_quote_to_numeric#from_level ~config @@ Obsolete ()
  let odo_without_to
    = odo_without_to#from_level ~config @@ Warning ()
  let section_segments
    = section_segments#from_level ~config Ignore
  let alter_statement
    = alter_statement#from_level ~config @@ Obsolete ()
  let call_overflow
    = call_overflow#from_level ~config @@ Archaic ()
  let numeric_boolean
    = numeric_boolean#from_level ~config @@ Ok ()
  let hexadecimal_boolean
    = hexadecimal_boolean#from_level ~config @@ Ok ()
  let national_literals
    = national_literals#from_level ~config @@ Ok ()
  let hexadecimal_national_literals
    = hexadecimal_national_literals#from_level ~config @@ Ok ()
  let national_character_literals
    = national_character_literals#from_level ~config @@ Warning ()
  let hp_octal_literals
    = hp_octal_literals#from_level ~config Unconformable
  let acu_literals
    = acu_literals#from_level ~config Unconformable
  let word_continuation
    = word_continuation#from_level ~config @@ Warning ()
  let not_exception_before_exception
    = not_exception_before_exception#from_level ~config @@ Ok ()
  let accept_display_extensions
    = accept_display_extensions#from_level ~config @@ Ok ()
  let larger_redefines
    = larger_redefines#from_level ~config Error
  let symbolic_constant
    = symbolic_constant#from_level ~config @@ Ok ()
  let constant_78
    = constant_78#from_level ~config @@ Ok ()
  let constant_01
    = constant_01#from_level ~config @@ Ok ()
  let perform_varying_without_by
    = perform_varying_without_by#from_level ~config @@ Ok ()
  let reference_out_of_declaratives
    = reference_out_of_declaratives#from_level ~config @@ Warning ()
  let program_prototypes
    = program_prototypes#from_level ~config @@ Ok ()
  let call_convention_mnemonic
    = call_convention_mnemonic#from_level ~config @@ Ok ()
  let call_convention_linkage
    = call_convention_linkage#from_level ~config @@ Ok ()
  let numeric_value_for_edited_item
    = numeric_value_for_edited_item#from_level ~config @@ Ok ()
  let incorrect_conf_sec_order
    = incorrect_conf_sec_order#from_level ~config @@ Ok ()
  let define_constant_directive
    = define_constant_directive#from_level ~config @@ Archaic ()
  let free_redefines_position
    = free_redefines_position#from_level ~config @@ Warning ()
  let records_mismatch_record_clause
    = records_mismatch_record_clause#from_level ~config @@ Warning ()
  let record_delimiter
    = record_delimiter#from_level ~config @@ Ok ()
  let sequential_delimiters
    = sequential_delimiters#from_level ~config @@ Ok ()
  let record_delim_with_fixed_recs
    = record_delim_with_fixed_recs#from_level ~config @@ Ok ()
  let missing_statement
    = missing_statement#from_level ~config @@ Warning ()
  let missing_period
    = missing_period#from_level ~config @@ Warning ()
  let zero_length_literals
    = zero_length_literals#from_level ~config @@ Ok ()
  let xml_generate_extra_phrases
    = xml_generate_extra_phrases#from_level ~config @@ Ok ()
  let continue_after
    = continue_after#from_level ~config @@ Ok ()
  let goto_entry
    = goto_entry#from_level ~config @@ Warning ()
  let assign_variable
    = assign_variable#from_level ~config @@ Ok ()
  let assign_using_variable
    = assign_using_variable#from_level ~config @@ Ok ()
  let assign_ext_dyn
    = assign_ext_dyn#from_level ~config @@ Ok ()
  let assign_disk_from
    = assign_disk_from#from_level ~config @@ Ok ()
  let vsam_status
    = vsam_status#from_level ~config Ignore
  let self_call_recursive
    = self_call_recursive#from_level ~config @@ Warning ()
  let record_contains_depending_clause
    = record_contains_depending_clause#from_level ~config Unconformable
  let picture_l
    = picture_l#from_level ~config @@ Ok ()
end

include Default
