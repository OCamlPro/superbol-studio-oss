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
  let origin = Cobol_dialect.Origin.of_id Default

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
  let tab_stops = [8]
  let text_column = 72
  let pic_length = 255
  let word_length = 63
  let literal_length = 8191
  let numeric_literal_length = 38

  (* any options *)
  let defaultbyte = Init
  let standard_define = GnuCOBOL
  let format = Auto
  let binary_size = B_1_2_4_8
  let binary_byteorder = Big_endian
  let assign_clause = Dynamic
  let screen_section_rules = GC
  let dpc_in_data = XML

  (* boolean options *)
  let filename_mapping = true
  let pretty_display = true
  let binary_truncate = true
  let complex_odo = false
  let odoslide = false
  let indirect_redefines = false
  let relax_syntax_checks = false
  let ref_mod_zero_length = true
  let relax_level_hierarchy = false
  let select_working = false
  let local_implies_recursive = false
  let sticky_linkage = false
  let move_ibm = false
  let perform_osvs = false
  let arithmetic_osvs = false
  let hostsign = false
  let program_name_redefinition = true
  let accept_update = false
  let accept_auto = false
  let console_is_crt = false
  let no_echo_means_secure = false
  let line_col_zero_default = true
  let display_special_fig_consts = false
  let binary_comp_1 = false
  let numeric_pointer = false
  let move_non_numeric_lit_to_numeric_is_zero = false
  let implicit_assign_dynamic_var = true
  let device_mnemonics = false
  let xml_parse_xmlss = true
  let areacheck = false
  let ebcdic_symbolic_characters = false

  (* support options *)
  open Cobol_dialect.Types
  let bind f = Cobol_dialect.Feature.bind ~origin f
  let comment_paragraphs = bind comment_paragraphs @@ Obsolete ()
  let safe_partial_replacing_when_src_literal
    = bind safe_partial_replacing_when_src_literal @@ Obsolete `Unsafe
  let control_division = bind control_division Unconformable
  let memory_size_clause = bind memory_size_clause @@ Obsolete ()
  let multiple_file_tape_clause = bind multiple_file_tape_clause @@ Obsolete ()
  let label_records_clause = bind label_records_clause @@ Obsolete ()
  let value_of_clause = bind value_of_clause @@ Obsolete ()
  let data_records_clause = bind data_records_clause @@ Obsolete ()
  let top_level_occurs_clause = bind top_level_occurs_clause @@ Ok ()
  let same_as_clause = bind same_as_clause @@ Ok ()
  let type_to_clause = bind type_to_clause @@ Ok ()
  let usage_type = bind usage_type @@ Ok ()
  let synchronized_clause = bind synchronized_clause @@ Ok ()
  let sync_left_right = bind sync_left_right @@ Ok ()
  let special_names_clause = bind special_names_clause @@ Ok ()
  let goto_statement_without_name
    = bind goto_statement_without_name @@ Obsolete ()
  let stop_literal_statement = bind stop_literal_statement @@ Obsolete ()
  let stop_identifier_statement = bind stop_identifier_statement @@ Obsolete ()
  let stop_error_statement = bind stop_error_statement Unconformable
  let debugging_mode = bind debugging_mode @@ Ok ()
  let use_for_debugging = bind use_for_debugging @@ Ok ()
  let padding_character_clause = bind padding_character_clause  @@ Obsolete ()
  let next_sentence_phrase = bind next_sentence_phrase @@ Archaic ()
  let listing_statements = bind listing_statements Skip
  let title_statement = bind title_statement Skip
  let entry_statement = bind entry_statement @@ Ok ()
  let move_noninteger_to_alphanumeric
    = bind move_noninteger_to_alphanumeric Error
  let move_figurative_constant_to_numeric
    = bind move_figurative_constant_to_numeric @@ Archaic ()
  let move_figurative_space_to_numeric
    = bind move_figurative_space_to_numeric Error
  let move_figurative_quote_to_numeric
    = bind move_figurative_quote_to_numeric @@ Obsolete ()
  let odo_without_to = bind odo_without_to @@ Warning ()
  let section_segments = bind section_segments Ignore
  let alter_statement = bind alter_statement @@ Obsolete ()
  let call_overflow = bind call_overflow @@ Archaic ()
  let numeric_boolean = bind numeric_boolean @@ Ok ()
  let hexadecimal_boolean = bind hexadecimal_boolean @@ Ok ()
  let national_literals = bind national_literals @@ Ok ()
  let hexadecimal_national_literals
    = bind hexadecimal_national_literals @@ Ok ()
  let national_character_literals
    = bind national_character_literals @@ Warning ()
  let hp_octal_literals = bind hp_octal_literals Unconformable
  let acu_literals = bind acu_literals Unconformable
  let word_continuation = bind word_continuation @@ Warning ()
  let not_exception_before_exception
    = bind not_exception_before_exception @@ Ok ()
  let accept_display_extensions = bind accept_display_extensions @@ Ok ()
  let larger_redefines = bind larger_redefines Error
  let symbolic_constant = bind symbolic_constant @@ Ok ()
  let constant_78 = bind constant_78 @@ Ok ()
  let constant_01 = bind constant_01 @@ Ok ()
  let perform_varying_without_by = bind perform_varying_without_by @@ Ok ()
  let reference_out_of_declaratives
    = bind reference_out_of_declaratives @@ Warning ()
  let program_prototypes = bind program_prototypes @@ Ok ()
  let call_convention_mnemonic = bind call_convention_mnemonic @@ Ok ()
  let call_convention_linkage = bind call_convention_linkage @@ Ok ()
  let numeric_value_for_edited_item
    = bind numeric_value_for_edited_item @@ Ok ()
  let incorrect_conf_sec_order = bind incorrect_conf_sec_order @@ Ok ()
  let define_constant_directive = bind define_constant_directive @@ Archaic ()
  let free_redefines_position = bind free_redefines_position @@ Warning ()
  let records_mismatch_record_clause
    = bind records_mismatch_record_clause @@ Warning ()
  let record_delimiter = bind record_delimiter @@ Ok ()
  let sequential_delimiters = bind sequential_delimiters @@ Ok ()
  let record_delim_with_fixed_recs = bind record_delim_with_fixed_recs @@ Ok ()
  let missing_statement = bind missing_statement @@ Warning ()
  let missing_period = bind missing_period @@ Warning ()
  let zero_length_literals = bind zero_length_literals @@ Ok ()
  let xml_generate_extra_phrases = bind xml_generate_extra_phrases @@ Ok ()
  let continue_after = bind continue_after @@ Ok ()
  let goto_entry = bind goto_entry @@ Warning ()
  let assign_variable = bind assign_variable @@ Ok ()
  let assign_using_variable = bind assign_using_variable @@ Ok ()
  let assign_ext_dyn = bind assign_ext_dyn @@ Ok ()
  let assign_disk_from = bind assign_disk_from @@ Ok ()
  let vsam_status = bind vsam_status Ignore
  let self_call_recursive = bind self_call_recursive @@ Warning ()
  let record_contains_depending_clause
    = bind record_contains_depending_clause Unconformable
  let picture_l = bind picture_l @@ Ok ()
end

include Default
