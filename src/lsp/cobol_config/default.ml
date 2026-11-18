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

let default : Types.t =
  let open Types in
  let origin = Cobol_dialect.Origin.of_id Default in
  let words =
    let alias_for b = ReserveAlias { alias_for = b;
                                     preserve_context_sensitivity = false } in
    Reserved_words.words @
    List.map (fun w -> w, NotReserved) not_reserved @
    List.map (fun (a, b) -> a, alias_for b) default_aliases
  in
  let not_reserved = StringSet.of_list not_reserved in
  let open Options in
  let open Cobol_dialect.Types in
  let bind f = Cobol_dialect.Feature.bind ~origin f in
  {
    origin;

    (* reserved words *)
    words;
    intrinsic_functions =
      StringSet.diff Reserved_words.default_intrinsics not_reserved;
    system_names =
      StringSet.diff Reserved_words.default_system_names not_reserved;
    registers = StringSet.diff Reserved_words.default_registers not_reserved;

    (* int options *)
    tab_stops = [8];
    text_column = 72;
    pic_length = 255;
    word_length = 63;
    literal_length = 8191;
    numeric_literal_length = 38;

    (* any options *)
    defaultbyte = Init;
    standard_define = GnuCOBOL;
    format = Auto;
    binary_size = B_1_2_4_8;
    binary_byteorder = Big_endian;
    assign_clause = Dynamic;
    screen_section_rules = GC;
    dpc_in_data = XML;

    (* boolean options *)
    filename_mapping = true;
    pretty_display = true;
    binary_truncate = true;
    complex_odo = false;
    odoslide = false;
    indirect_redefines = false;
    relax_syntax_checks = false;
    ref_mod_zero_length = true;
    relax_level_hierarchy = false;
    select_working = false;
    local_implies_recursive = false;
    sticky_linkage = false;
    move_ibm = false;
    perform_osvs = false;
    arithmetic_osvs = false;
    hostsign = false;
    program_name_redefinition = true;
    accept_update = false;
    accept_auto = false;
    console_is_crt = false;
    no_echo_means_secure = false;
    line_col_zero_default = true;
    display_special_fig_consts = false;
    binary_comp_1 = false;
    numeric_pointer = false;
    move_non_numeric_lit_to_numeric_is_zero = false;
    implicit_assign_dynamic_var = true;
    device_mnemonics = false;
    xml_parse_xmlss = true;
    areacheck = false;
    ebcdic_symbolic_characters = false;

    (* support options *)
    comment_paragraphs = bind comment_paragraphs @@ Obsolete ();
    safe_partial_replacing_when_src_literal =
      bind safe_partial_replacing_when_src_literal @@ Obsolete `Unsafe;
    control_division = bind control_division Unconformable;
    memory_size_clause = bind memory_size_clause @@ Obsolete ();
    multiple_file_tape_clause = bind multiple_file_tape_clause @@ Obsolete ();
    label_records_clause = bind label_records_clause @@ Obsolete ();
    value_of_clause = bind value_of_clause @@ Obsolete ();
    data_records_clause = bind data_records_clause @@ Obsolete ();
    top_level_occurs_clause = bind top_level_occurs_clause @@ Ok ();
    same_as_clause = bind same_as_clause @@ Ok ();
    type_to_clause = bind type_to_clause @@ Ok ();
    usage_type = bind usage_type @@ Ok ();
    synchronized_clause = bind synchronized_clause @@ Ok ();
    sync_left_right = bind sync_left_right @@ Ok ();
    special_names_clause = bind special_names_clause @@ Ok ();
    goto_statement_without_name =
      bind goto_statement_without_name @@ Obsolete ();
    stop_literal_statement = bind stop_literal_statement @@ Obsolete ();
    stop_identifier_statement = bind stop_identifier_statement @@ Obsolete ();
    stop_error_statement = bind stop_error_statement Unconformable;
    debugging_mode = bind debugging_mode @@ Ok ();
    use_for_debugging = bind use_for_debugging @@ Ok ();
    padding_character_clause = bind padding_character_clause  @@ Obsolete ();
    next_sentence_phrase = bind next_sentence_phrase @@ Archaic ();
    listing_statements = bind listing_statements Skip;
    title_statement = bind title_statement Skip;
    entry_statement = bind entry_statement @@ Ok ();
    move_noninteger_to_alphanumeric =
      bind move_noninteger_to_alphanumeric Error;
    move_figurative_constant_to_numeric =
      bind move_figurative_constant_to_numeric @@ Archaic ();
    move_figurative_space_to_numeric =
      bind move_figurative_space_to_numeric Error;
    move_figurative_quote_to_numeric =
      bind move_figurative_quote_to_numeric @@ Obsolete ();
    odo_without_to = bind odo_without_to @@ Warning ();
    section_segments = bind section_segments Ignore;
    alter_statement = bind alter_statement @@ Obsolete ();
    call_overflow = bind call_overflow @@ Archaic ();
    numeric_boolean = bind numeric_boolean @@ Ok ();
    hexadecimal_boolean = bind hexadecimal_boolean @@ Ok ();
    national_literals = bind national_literals @@ Ok ();
    hexadecimal_national_literals = bind hexadecimal_national_literals @@ Ok ();
    national_character_literals =
      bind national_character_literals @@ Warning ();
    hp_octal_literals = bind hp_octal_literals Unconformable;
    acu_literals = bind acu_literals Unconformable;
    word_continuation = bind word_continuation @@ Warning ();
    not_exception_before_exception =
      bind not_exception_before_exception @@ Ok ();
    accept_display_extensions = bind accept_display_extensions @@ Ok ();
    larger_redefines = bind larger_redefines Error;
    symbolic_constant = bind symbolic_constant @@ Ok ();
    constant_78 = bind constant_78 @@ Ok ();
    constant_01 = bind constant_01 @@ Ok ();
    perform_varying_without_by = bind perform_varying_without_by @@ Ok ();
    reference_out_of_declaratives =
      bind reference_out_of_declaratives @@ Warning ();
    program_prototypes = bind program_prototypes @@ Ok ();
    call_convention_mnemonic = bind call_convention_mnemonic @@ Ok ();
    call_convention_linkage = bind call_convention_linkage @@ Ok ();
    numeric_value_for_edited_item = bind numeric_value_for_edited_item @@ Ok ();
    incorrect_conf_sec_order = bind incorrect_conf_sec_order @@ Ok ();
    define_constant_directive = bind define_constant_directive @@ Archaic ();
    free_redefines_position = bind free_redefines_position @@ Warning ();
    records_mismatch_record_clause =
      bind records_mismatch_record_clause @@ Warning ();
    record_delimiter = bind record_delimiter @@ Ok ();
    sequential_delimiters = bind sequential_delimiters @@ Ok ();
    record_delim_with_fixed_recs = bind record_delim_with_fixed_recs @@ Ok ();
    missing_statement = bind missing_statement @@ Warning ();
    missing_period = bind missing_period @@ Warning ();
    zero_length_literals = bind zero_length_literals @@ Ok ();
    xml_generate_extra_phrases = bind xml_generate_extra_phrases @@ Ok ();
    continue_after = bind continue_after @@ Ok ();
    goto_entry = bind goto_entry @@ Warning ();
    assign_variable = bind assign_variable @@ Ok ();
    assign_using_variable = bind assign_using_variable @@ Ok ();
    assign_ext_dyn = bind assign_ext_dyn @@ Ok ();
    assign_disk_from = bind assign_disk_from @@ Ok ();
    vsam_status = bind vsam_status Ignore;
    self_call_recursive = bind self_call_recursive @@ Warning ();
    record_contains_depending_clause =
      bind record_contains_depending_clause Unconformable;
    picture_l = bind picture_l @@ Ok ();
  }
