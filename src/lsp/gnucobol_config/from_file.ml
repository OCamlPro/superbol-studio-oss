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

open Types
open Options

open EzCompat

module Make
    (Diags: Cobol_common.Diagnostics.STATEFUL)
    (Conf: sig
       include Types.CONFIG
       val options: Conf_ast.value StringMap.t
       val words: Cobol_common.Reserved.TYPES.words_spec
       val intrinsic_functions: StringSet.t
       val system_names: StringSet.t
       val registers: StringSet.t
     end): T = struct
  open Conf

  exception LookupError

  let warn t n v =
    Diags.warn "`%s`@ expected@ a@ value@ of@ type@ %s,@ but@ got@ %a;@ \
                resorting@ to@ the@ default@ value." n t Conf_ast.pp_value v

  let not_found n =
    Diags.warn "Could@ not@ find@ a@ configuration@ for@ `%s';@ using@ the@ \
                default@ value." n

  let find_any name =
    match StringMap.find_opt name options with
    | Some (Any v | String v) -> v
    | Some (Int i) -> string_of_int i
    | Some v ->
        warn "any" name v;
        raise LookupError
    | None -> not_found name; raise LookupError

  let find_bool name =
    match StringMap.find_opt name options with
    | Some Bool v -> v
    | Some v ->
        warn "boolean" name v;
        raise LookupError
    | None -> not_found name; raise LookupError

  let find_int name =
    match StringMap.find_opt name options with
    | Some Int v -> v
    | Some v ->
        warn "int" name v;
        raise LookupError
    | None -> not_found name; raise LookupError

  let find_support name =
    match StringMap.find_opt name options with
    | Some Support (Normal v| Additional v) -> v
    | Some v ->
        warn "support" name v;
        raise LookupError
    | None -> not_found name; raise LookupError


  let config = config
  let dialect = dialect

  (* reserved_words *)
  let words = words
  let intrinsic_functions = intrinsic_functions
  let system_names = system_names
  let registers = registers


  let parse_value finder v default =
    try v#from_val ~config (finder v#name)
    with LookupError -> default
  let parse_int_value = parse_value find_int
  let parse_bool_value = parse_value find_bool
  let parse_string_value v default =
    try v#from_string ~config (find_any v#name)
    with LookupError -> default
  let parse_support_value v default =
    try v#from_ast ~config (find_support v#name)
    with LookupError -> default

  (* int values *)
  let tab_width = parse_int_value tab_width Default.tab_width
  let text_column = parse_int_value text_column Default.text_column
  let pic_length = parse_int_value pic_length Default.pic_length
  let word_length = parse_int_value word_length Default.word_length
  let literal_length = parse_int_value literal_length Default.literal_length
  let numeric_literal_length = parse_int_value numeric_literal_length
      Default.numeric_literal_length

  (* any values *)
  let defaultbyte = parse_string_value defaultbyte Default.defaultbyte
  let standard_define = parse_string_value standard_define Default.standard_define
  let format = parse_string_value format Default.format
  let binary_size = parse_string_value binary_size Default.binary_size
  let binary_byteorder = parse_string_value binary_byteorder Default.binary_byteorder
  let assign_clause = parse_string_value assign_clause Default.assign_clause
  let screen_section_rules = parse_string_value screen_section_rules Default.screen_section_rules
  let dpc_in_data = parse_string_value dpc_in_data Default.dpc_in_data

  (*  boolean values *)
  let filename_mapping = parse_bool_value filename_mapping Default.filename_mapping
  let pretty_display = parse_bool_value pretty_display Default.pretty_display
  let binary_truncate = parse_bool_value binary_truncate Default.binary_truncate
  let complex_odo = parse_bool_value complex_odo Default.complex_odo
  let odoslide = parse_bool_value odoslide Default.odoslide
  let indirect_redefines = parse_bool_value indirect_redefines Default.indirect_redefines
  let relax_syntax_checks = parse_bool_value relax_syntax_checks Default.relax_syntax_checks
  let ref_mod_zero_length = parse_bool_value ref_mod_zero_length Default.ref_mod_zero_length
  let relax_level_hierarchy = parse_bool_value relax_level_hierarchy Default.relax_level_hierarchy
  let select_working = parse_bool_value select_working Default.select_working
  let local_implies_recursive = parse_bool_value local_implies_recursive Default.local_implies_recursive
  let sticky_linkage = parse_bool_value sticky_linkage Default.sticky_linkage
  let move_ibm = parse_bool_value move_ibm Default.move_ibm
  let perform_osvs = parse_bool_value perform_osvs Default.perform_osvs
  let arithmetic_osvs = parse_bool_value arithmetic_osvs Default.arithmetic_osvs
  let hostsign = parse_bool_value hostsign Default.hostsign
  let program_name_redefinition = parse_bool_value program_name_redefinition Default.program_name_redefinition
  let accept_update = parse_bool_value accept_update Default.accept_update
  let accept_auto = parse_bool_value accept_auto Default.accept_auto
  let console_is_crt = parse_bool_value console_is_crt Default.console_is_crt
  let no_echo_means_secure = parse_bool_value no_echo_means_secure Default.no_echo_means_secure
  let line_col_zero_default = parse_bool_value line_col_zero_default Default.line_col_zero_default
  let display_special_fig_consts = parse_bool_value display_special_fig_consts Default.display_special_fig_consts
  let binary_comp_1 = parse_bool_value binary_comp_1 Default.binary_comp_1
  let numeric_pointer = parse_bool_value numeric_pointer Default.numeric_pointer
  let move_non_numeric_lit_to_numeric_is_zero = parse_bool_value move_non_numeric_lit_to_numeric_is_zero Default.move_non_numeric_lit_to_numeric_is_zero
  let implicit_assign_dynamic_var = parse_bool_value implicit_assign_dynamic_var Default.implicit_assign_dynamic_var
  let device_mnemonics = parse_bool_value device_mnemonics Default.device_mnemonics
  let xml_parse_xmlss = parse_bool_value xml_parse_xmlss Default.xml_parse_xmlss
  let areacheck = parse_bool_value areacheck Default.areacheck
  let ebcdic_symbolic_characters = parse_bool_value ebcdic_symbolic_characters Default.ebcdic_symbolic_characters

  (* support values *)
  let comment_paragraphs = parse_support_value comment_paragraphs Default.comment_paragraphs
  let safe_partial_replacing_when_src_literal = parse_support_value safe_partial_replacing_when_src_literal Default.safe_partial_replacing_when_src_literal
  let control_division = parse_support_value control_division Default.control_division
  let memory_size_clause = parse_support_value memory_size_clause Default.memory_size_clause
  let multiple_file_tape_clause = parse_support_value multiple_file_tape_clause Default.multiple_file_tape_clause
  let label_records_clause = parse_support_value label_records_clause Default.label_records_clause
  let value_of_clause = parse_support_value value_of_clause Default.value_of_clause
  let data_records_clause = parse_support_value data_records_clause Default.data_records_clause
  let top_level_occurs_clause = parse_support_value top_level_occurs_clause Default.top_level_occurs_clause
  let same_as_clause = parse_support_value same_as_clause Default.same_as_clause
  let type_to_clause = parse_support_value type_to_clause Default.type_to_clause
  let usage_type = parse_support_value usage_type Default.usage_type
  let synchronized_clause = parse_support_value synchronized_clause Default.synchronized_clause
  let sync_left_right = parse_support_value sync_left_right Default.sync_left_right
  let special_names_clause = parse_support_value special_names_clause Default.special_names_clause
  let goto_statement_without_name = parse_support_value goto_statement_without_name Default.goto_statement_without_name
  let stop_literal_statement = parse_support_value stop_literal_statement Default.stop_literal_statement
  let stop_identifier_statement = parse_support_value stop_identifier_statement Default.stop_identifier_statement
  let stop_error_statement = parse_support_value stop_error_statement Default.stop_error_statement
  let debugging_mode = parse_support_value debugging_mode Default.debugging_mode
  let use_for_debugging = parse_support_value use_for_debugging Default.use_for_debugging
  let padding_character_clause = parse_support_value padding_character_clause Default.padding_character_clause
  let next_sentence_phrase = parse_support_value next_sentence_phrase Default.next_sentence_phrase
  let listing_statements = parse_support_value listing_statements Default.listing_statements
  let title_statement = parse_support_value title_statement Default.title_statement
  let entry_statement = parse_support_value entry_statement Default.entry_statement
  let move_noninteger_to_alphanumeric = parse_support_value move_noninteger_to_alphanumeric Default.move_noninteger_to_alphanumeric
  let move_figurative_constant_to_numeric = parse_support_value move_figurative_constant_to_numeric Default.move_figurative_constant_to_numeric
  let move_figurative_space_to_numeric = parse_support_value move_figurative_space_to_numeric Default.move_figurative_space_to_numeric
  let move_figurative_quote_to_numeric = parse_support_value move_figurative_quote_to_numeric Default.move_figurative_quote_to_numeric
  let odo_without_to = parse_support_value odo_without_to Default.odo_without_to
  let section_segments = parse_support_value section_segments Default.section_segments
  let alter_statement = parse_support_value alter_statement Default.alter_statement
  let call_overflow = parse_support_value call_overflow Default.call_overflow
  let numeric_boolean = parse_support_value numeric_boolean Default.numeric_boolean
  let hexadecimal_boolean = parse_support_value hexadecimal_boolean Default.hexadecimal_boolean
  let national_literals = parse_support_value national_literals Default.national_literals
  let hexadecimal_national_literals = parse_support_value hexadecimal_national_literals Default.hexadecimal_national_literals
  let national_character_literals = parse_support_value national_character_literals Default.national_character_literals
  let hp_octal_literals = parse_support_value hp_octal_literals Default.hp_octal_literals
  let acu_literals = parse_support_value acu_literals Default.acu_literals
  let word_continuation = parse_support_value word_continuation Default.word_continuation
  let not_exception_before_exception = parse_support_value not_exception_before_exception Default.not_exception_before_exception
  let accept_display_extensions = parse_support_value accept_display_extensions Default.accept_display_extensions
  let larger_redefines = parse_support_value larger_redefines Default.larger_redefines
  let symbolic_constant = parse_support_value symbolic_constant Default.symbolic_constant
  let constant_78 = parse_support_value constant_78 Default.constant_78
  let constant_01 = parse_support_value constant_01 Default.constant_01
  let perform_varying_without_by = parse_support_value perform_varying_without_by Default.perform_varying_without_by
  let reference_out_of_declaratives = parse_support_value reference_out_of_declaratives Default.reference_out_of_declaratives
  let program_prototypes = parse_support_value program_prototypes Default.program_prototypes
  let call_convention_mnemonic = parse_support_value call_convention_mnemonic Default.call_convention_mnemonic
  let call_convention_linkage = parse_support_value call_convention_linkage Default.call_convention_linkage
  let numeric_value_for_edited_item = parse_support_value numeric_value_for_edited_item Default.numeric_value_for_edited_item
  let incorrect_conf_sec_order = parse_support_value incorrect_conf_sec_order Default.incorrect_conf_sec_order
  let define_constant_directive = parse_support_value define_constant_directive Default.define_constant_directive
  let free_redefines_position = parse_support_value free_redefines_position Default.free_redefines_position
  let records_mismatch_record_clause = parse_support_value records_mismatch_record_clause Default.records_mismatch_record_clause
  let record_delimiter = parse_support_value record_delimiter Default.record_delimiter
  let sequential_delimiters = parse_support_value sequential_delimiters Default.sequential_delimiters
  let record_delim_with_fixed_recs = parse_support_value record_delim_with_fixed_recs Default.record_delim_with_fixed_recs
  let missing_statement = parse_support_value missing_statement Default.missing_statement
  let missing_period = parse_support_value missing_period Default.missing_period
  let zero_length_literals = parse_support_value zero_length_literals Default.zero_length_literals
  let xml_generate_extra_phrases = parse_support_value xml_generate_extra_phrases Default.xml_generate_extra_phrases
  let continue_after = parse_support_value continue_after Default.continue_after
  let goto_entry = parse_support_value goto_entry Default.goto_entry
  let assign_variable = parse_support_value assign_variable Default.assign_variable
  let assign_using_variable = parse_support_value assign_using_variable Default.assign_using_variable
  let assign_ext_dyn = parse_support_value assign_ext_dyn Default.assign_ext_dyn
  let assign_disk_from = parse_support_value assign_disk_from Default.assign_disk_from
  let vsam_status = parse_support_value vsam_status Default.vsam_status
  let self_call_recursive = parse_support_value self_call_recursive Default.self_call_recursive
  let record_contains_depending_clause = parse_support_value record_contains_depending_clause Default.record_contains_depending_clause
  let picture_l = parse_support_value picture_l Default.picture_l
end
