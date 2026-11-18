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
       val origin: Cobol_dialect.Types.origin
       val options: Conf_ast.value StringMap.t
       val words: Types.words_spec
       val intrinsic_functions: StringSet.t
       val system_names: StringSet.t
       val registers: StringSet.t
     end): sig val config: t end = struct
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



  let parse_value finder v default =
    try finder v.name
    with LookupError -> default
  let parse_int_value = parse_value find_int
  let parse_bool_value = parse_value find_bool
  let parse_string_value v default =
    try v.parse (find_any v.name)
    with LookupError -> default
  let parse_support_value v default =
    try Cobol_dialect.Feature.bind_level ~origin v (find_support v.info.name)
    with LookupError -> default

  let config : t =
    let default = Default.default in
    {
      origin;

      (* reserved words *)
      words;
      intrinsic_functions;
      system_names;
      registers;

      (* int values *)
      tab_stops = parse_string_value tab_stops default.tab_stops;
      text_column = parse_int_value text_column default.text_column;
      pic_length = parse_int_value pic_length default.pic_length;
      word_length = parse_int_value word_length default.word_length;
      literal_length = parse_int_value literal_length default.literal_length;
      numeric_literal_length =
        parse_int_value numeric_literal_length default.numeric_literal_length;

      (* any values *)
      defaultbyte = parse_string_value defaultbyte default.defaultbyte;
      standard_define =
        parse_string_value standard_define default.standard_define;
      format = parse_string_value format default.format;
      binary_size = parse_string_value binary_size default.binary_size;
      binary_byteorder =
        parse_string_value binary_byteorder default.binary_byteorder;
      assign_clause = parse_string_value assign_clause default.assign_clause;
      screen_section_rules =
        parse_string_value screen_section_rules default.screen_section_rules;
      dpc_in_data = parse_string_value dpc_in_data default.dpc_in_data;

      (* boolean values *)
      filename_mapping =
        parse_bool_value filename_mapping default.filename_mapping;
      pretty_display = parse_bool_value pretty_display default.pretty_display;
      binary_truncate =
        parse_bool_value binary_truncate default.binary_truncate;
      complex_odo = parse_bool_value complex_odo default.complex_odo;
      odoslide = parse_bool_value odoslide default.odoslide;
      indirect_redefines =
        parse_bool_value indirect_redefines default.indirect_redefines;
      relax_syntax_checks =
        parse_bool_value relax_syntax_checks default.relax_syntax_checks;
      ref_mod_zero_length =
        parse_bool_value ref_mod_zero_length default.ref_mod_zero_length;
      relax_level_hierarchy =
        parse_bool_value relax_level_hierarchy default.relax_level_hierarchy;
      select_working = parse_bool_value select_working default.select_working;
      local_implies_recursive =
        parse_bool_value local_implies_recursive
          default.local_implies_recursive;
      sticky_linkage = parse_bool_value sticky_linkage default.sticky_linkage;
      move_ibm = parse_bool_value move_ibm default.move_ibm;
      perform_osvs = parse_bool_value perform_osvs default.perform_osvs;
      arithmetic_osvs =
        parse_bool_value arithmetic_osvs default.arithmetic_osvs;
      hostsign = parse_bool_value hostsign default.hostsign;
      program_name_redefinition =
        parse_bool_value program_name_redefinition
          default.program_name_redefinition;
      accept_update = parse_bool_value accept_update default.accept_update;
      accept_auto = parse_bool_value accept_auto default.accept_auto;
      console_is_crt = parse_bool_value console_is_crt default.console_is_crt;
      no_echo_means_secure =
        parse_bool_value no_echo_means_secure default.no_echo_means_secure;
      line_col_zero_default =
        parse_bool_value line_col_zero_default default.line_col_zero_default;
      display_special_fig_consts =
        parse_bool_value display_special_fig_consts
          default.display_special_fig_consts;
      binary_comp_1 = parse_bool_value binary_comp_1 default.binary_comp_1;
      numeric_pointer =
        parse_bool_value numeric_pointer default.numeric_pointer;
      move_non_numeric_lit_to_numeric_is_zero =
        parse_bool_value move_non_numeric_lit_to_numeric_is_zero
          default.move_non_numeric_lit_to_numeric_is_zero;
      implicit_assign_dynamic_var =
        parse_bool_value implicit_assign_dynamic_var
          default.implicit_assign_dynamic_var;
      device_mnemonics =
        parse_bool_value device_mnemonics default.device_mnemonics;
      xml_parse_xmlss =
        parse_bool_value xml_parse_xmlss default.xml_parse_xmlss;
      areacheck = parse_bool_value areacheck default.areacheck;
      ebcdic_symbolic_characters =
        parse_bool_value ebcdic_symbolic_characters
          default.ebcdic_symbolic_characters;

      (* support values *)
      comment_paragraphs =
        parse_support_value comment_paragraphs default.comment_paragraphs;
      safe_partial_replacing_when_src_literal =
        parse_support_value safe_partial_replacing_when_src_literal
          default.safe_partial_replacing_when_src_literal;
      control_division =
        parse_support_value control_division default.control_division;
      memory_size_clause =
        parse_support_value memory_size_clause default.memory_size_clause;
      multiple_file_tape_clause =
        parse_support_value multiple_file_tape_clause
          default.multiple_file_tape_clause;
      label_records_clause =
        parse_support_value label_records_clause default.label_records_clause;
      value_of_clause =
        parse_support_value value_of_clause default.value_of_clause;
      data_records_clause =
        parse_support_value data_records_clause default.data_records_clause;
      top_level_occurs_clause =
        parse_support_value top_level_occurs_clause
          default.top_level_occurs_clause;
      same_as_clause =
        parse_support_value same_as_clause default.same_as_clause;
      type_to_clause =
        parse_support_value type_to_clause default.type_to_clause;
      usage_type = parse_support_value usage_type default.usage_type;
      synchronized_clause =
        parse_support_value synchronized_clause default.synchronized_clause;
      sync_left_right =
        parse_support_value sync_left_right default.sync_left_right;
      special_names_clause =
        parse_support_value special_names_clause default.special_names_clause;
      goto_statement_without_name =
        parse_support_value goto_statement_without_name
          default.goto_statement_without_name;
      stop_literal_statement =
        parse_support_value stop_literal_statement
          default.stop_literal_statement;
      stop_identifier_statement =
        parse_support_value stop_identifier_statement
          default.stop_identifier_statement;
      stop_error_statement =
        parse_support_value stop_error_statement default.stop_error_statement;
      debugging_mode =
        parse_support_value debugging_mode default.debugging_mode;
      use_for_debugging =
        parse_support_value use_for_debugging default.use_for_debugging;
      padding_character_clause =
        parse_support_value padding_character_clause
          default.padding_character_clause;
      next_sentence_phrase =
        parse_support_value next_sentence_phrase default.next_sentence_phrase;
      listing_statements =
        parse_support_value listing_statements default.listing_statements;
      title_statement =
        parse_support_value title_statement default.title_statement;
      entry_statement =
        parse_support_value entry_statement default.entry_statement;
      move_noninteger_to_alphanumeric =
        parse_support_value move_noninteger_to_alphanumeric
          default.move_noninteger_to_alphanumeric;
      move_figurative_constant_to_numeric =
        parse_support_value move_figurative_constant_to_numeric
          default.move_figurative_constant_to_numeric;
      move_figurative_space_to_numeric =
        parse_support_value move_figurative_space_to_numeric
          default.move_figurative_space_to_numeric;
      move_figurative_quote_to_numeric =
        parse_support_value move_figurative_quote_to_numeric
          default.move_figurative_quote_to_numeric;
      odo_without_to =
        parse_support_value odo_without_to default.odo_without_to;
      section_segments =
        parse_support_value section_segments default.section_segments;
      alter_statement =
        parse_support_value alter_statement default.alter_statement;
      call_overflow = parse_support_value call_overflow default.call_overflow;
      numeric_boolean =
        parse_support_value numeric_boolean default.numeric_boolean;
      hexadecimal_boolean =
        parse_support_value hexadecimal_boolean default.hexadecimal_boolean;
      national_literals =
        parse_support_value national_literals default.national_literals;
      hexadecimal_national_literals =
        parse_support_value hexadecimal_national_literals
          default.hexadecimal_national_literals;
      national_character_literals =
        parse_support_value national_character_literals
          default.national_character_literals;
      hp_octal_literals =
        parse_support_value hp_octal_literals default.hp_octal_literals;
      acu_literals = parse_support_value acu_literals default.acu_literals;
      word_continuation =
        parse_support_value word_continuation default.word_continuation;
      not_exception_before_exception =
        parse_support_value not_exception_before_exception
          default.not_exception_before_exception;
      accept_display_extensions =
        parse_support_value accept_display_extensions
          default.accept_display_extensions;
      larger_redefines =
        parse_support_value larger_redefines default.larger_redefines;
      symbolic_constant =
        parse_support_value symbolic_constant default.symbolic_constant;
      constant_78 = parse_support_value constant_78 default.constant_78;
      constant_01 = parse_support_value constant_01 default.constant_01;
      perform_varying_without_by =
        parse_support_value perform_varying_without_by
          default.perform_varying_without_by;
      reference_out_of_declaratives =
        parse_support_value reference_out_of_declaratives
          default.reference_out_of_declaratives;
      program_prototypes =
        parse_support_value program_prototypes default.program_prototypes;
      call_convention_mnemonic =
        parse_support_value call_convention_mnemonic
          default.call_convention_mnemonic;
      call_convention_linkage =
        parse_support_value call_convention_linkage
          default.call_convention_linkage;
      numeric_value_for_edited_item =
        parse_support_value numeric_value_for_edited_item
          default.numeric_value_for_edited_item;
      incorrect_conf_sec_order =
        parse_support_value incorrect_conf_sec_order
          default.incorrect_conf_sec_order;
      define_constant_directive =
        parse_support_value define_constant_directive
          default.define_constant_directive;
      free_redefines_position =
        parse_support_value free_redefines_position
          default.free_redefines_position;
      records_mismatch_record_clause =
        parse_support_value records_mismatch_record_clause
          default.records_mismatch_record_clause;
      record_delimiter =
        parse_support_value record_delimiter default.record_delimiter;
      sequential_delimiters =
        parse_support_value sequential_delimiters default.sequential_delimiters;
      record_delim_with_fixed_recs =
        parse_support_value record_delim_with_fixed_recs
          default.record_delim_with_fixed_recs;
      missing_statement =
        parse_support_value missing_statement default.missing_statement;
      missing_period =
        parse_support_value missing_period default.missing_period;
      zero_length_literals =
        parse_support_value zero_length_literals default.zero_length_literals;
      xml_generate_extra_phrases =
        parse_support_value xml_generate_extra_phrases
          default.xml_generate_extra_phrases;
      continue_after =
        parse_support_value continue_after default.continue_after;
      goto_entry = parse_support_value goto_entry default.goto_entry;
      assign_variable =
        parse_support_value assign_variable default.assign_variable;
      assign_using_variable =
        parse_support_value assign_using_variable default.assign_using_variable;
      assign_ext_dyn =
        parse_support_value assign_ext_dyn default.assign_ext_dyn;
      assign_disk_from =
        parse_support_value assign_disk_from default.assign_disk_from;
      vsam_status = parse_support_value vsam_status default.vsam_status;
      self_call_recursive =
        parse_support_value self_call_recursive default.self_call_recursive;
      record_contains_depending_clause =
        parse_support_value record_contains_depending_clause
          default.record_contains_depending_clause;
      picture_l = parse_support_value picture_l default.picture_l;

      (* support options derived from the dialect ancestry *)
      alphanumeric_comp_x_over_8_chars =
        if Option.is_some @@ Cobol_dialect.Origin.closest_ancestor
            (function MicroFocus _ -> true | _ -> false) origin
        then Cobol_dialect.Feature.bind ~origin
            alphanumeric_comp_x_over_8_chars Unconformable
        else default.alphanumeric_comp_x_over_8_chars;
    }
end
