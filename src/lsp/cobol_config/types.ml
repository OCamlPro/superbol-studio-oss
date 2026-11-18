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

(** Module containing most of the types definitions used in {!Cobol_config}. *)

open EzCompat

type doc = Pretty.simple

type 'a feature = 'a Cobol_dialect.Types.feature
type 'a feature_support = 'a Cobol_dialect.Types.binding

(** Definition of a valued option. *)
type 'a value =
  {
    name: string;
    doc: doc;
    parse: string -> 'a;
  }

module Value = struct
  let def ~name ~parse doc = { name; doc; parse }
  let int = def ~parse:(fun s -> Scanf.sscanf s "%d" Fun.id)
  let int_list = def ~parse:Cobol_common.Tab_stops.of_string
  let bool = def ~parse:(fun s -> Scanf.sscanf s "%B" Fun.id)
end

type defaultbyte =
  | Char of char (* int values will be translated to char *)
  | Init
  | None

type standard =
  | GnuCOBOL
  | MicroFocus
  | IBM
  | MVS
  | BS2000
  | ACU
  | RM
  | STD85
  | STD2002
  | STD2014

type source_format = Cobol_common.Platform.TYPES.source_format_id =
  | SFFree
  | SFFixed
  | SFCOBOL85
  | SFVariable
  | SFXOpen
  | SFxCard
  | SFCRT
  | SFTrm
  | SFCOBOLX

type source_format_spec =
  | Auto
  | SF of source_format

type binary_size =
  | B_2_4_8
  | B_1_2_4_8
  | B_1__8

type binary_byteorder =
  | Native
  | Big_endian

(* TODO: Check if IBM and MF change the behavior compared with Dynamic and External *)
type assign_clause =
  | Dynamic
  | External
  | IBM
  | MF

type screen_section_rules =
  | ACU
  | GC
  | MF
  | RM
  | STD
  | XOPEN

type dpc_in_data =
  | None
  | XML
  | Json
  | All

type words_spec = (string * word_spec) list
and word_spec =
  | ReserveWord of
      {
        preserve_context_sensitivity: bool;
      }
  | ReserveAlias of
      {
        alias_for: string;
        preserve_context_sensitivity: bool;
      }
  | NotReserved


type t =
  {
    origin: Cobol_dialect.Types.origin;

    (* Preprocessor options *)

    (* int options *)
    tab_stops: int list;

    (* support options *)
    comment_paragraphs: unit feature_support;
    safe_partial_replacing_when_src_literal: [`Safe | `Unsafe] feature_support;

    (* Compiler options *)

    (* reserved words *)
    words: words_spec;
    intrinsic_functions: StringSet.t;
    system_names: StringSet.t;
    registers: StringSet.t;

    (* int options *)
    text_column: int;
    pic_length: int;
    word_length: int;
    literal_length: int;
    numeric_literal_length: int;

    (* any options *)
    defaultbyte: defaultbyte;
    standard_define: standard;
    format: source_format_spec;
    binary_size: binary_size;
    binary_byteorder: binary_byteorder;
    assign_clause: assign_clause;
    screen_section_rules: screen_section_rules;
    dpc_in_data: dpc_in_data;

    (* boolean options *)
    filename_mapping: bool;
    pretty_display: bool;
    binary_truncate: bool;
    complex_odo: bool;
    odoslide: bool;
    indirect_redefines: bool;
    relax_syntax_checks: bool;
    ref_mod_zero_length: bool;
    relax_level_hierarchy: bool;
    select_working: bool;
    local_implies_recursive: bool;
    sticky_linkage: bool;
    move_ibm: bool;
    perform_osvs: bool;
    arithmetic_osvs: bool;
    hostsign: bool;
    program_name_redefinition: bool;
    accept_update: bool;
    accept_auto: bool;
    console_is_crt: bool;
    no_echo_means_secure: bool;
    line_col_zero_default: bool;
    display_special_fig_consts: bool;
    binary_comp_1: bool;
    numeric_pointer: bool;
    move_non_numeric_lit_to_numeric_is_zero: bool;
    implicit_assign_dynamic_var: bool;
    device_mnemonics: bool;
    xml_parse_xmlss: bool;
    areacheck: bool;
    ebcdic_symbolic_characters: bool;

    (* support options *)
    control_division: unit feature_support;
    memory_size_clause: unit feature_support;
    multiple_file_tape_clause: unit feature_support;
    label_records_clause: unit feature_support;
    value_of_clause: unit feature_support;
    data_records_clause: unit feature_support;
    top_level_occurs_clause: unit feature_support;
    same_as_clause: unit feature_support;
    type_to_clause: unit feature_support;
    usage_type: unit feature_support;
    synchronized_clause: unit feature_support;
    sync_left_right: unit feature_support;
    special_names_clause: unit feature_support;
    goto_statement_without_name: unit feature_support;
    stop_literal_statement: unit feature_support;
    stop_identifier_statement: unit feature_support;
    stop_error_statement: unit feature_support;
    debugging_mode: unit feature_support;
    use_for_debugging: unit feature_support;
    padding_character_clause: unit feature_support;
    next_sentence_phrase: unit feature_support;
    listing_statements: unit feature_support;
    title_statement: unit feature_support;
    entry_statement: unit feature_support;
    move_noninteger_to_alphanumeric: unit feature_support;
    move_figurative_constant_to_numeric: unit feature_support;
    move_figurative_space_to_numeric: unit feature_support;
    move_figurative_quote_to_numeric: unit feature_support;
    odo_without_to: unit feature_support;
    section_segments: unit feature_support;
    alter_statement: unit feature_support;
    call_overflow: unit feature_support;
    numeric_boolean: unit feature_support;
    hexadecimal_boolean: unit feature_support;
    national_literals: unit feature_support;
    hexadecimal_national_literals: unit feature_support;
    national_character_literals: unit feature_support;
    hp_octal_literals: unit feature_support;
    acu_literals: unit feature_support;
    word_continuation: unit feature_support;
    not_exception_before_exception: unit feature_support;
    accept_display_extensions: unit feature_support;
    larger_redefines: unit feature_support;
    symbolic_constant: unit feature_support;
    constant_78: unit feature_support;
    constant_01: unit feature_support;
    perform_varying_without_by: unit feature_support;
    reference_out_of_declaratives: unit feature_support;
    program_prototypes: unit feature_support;
    call_convention_mnemonic: unit feature_support;
    call_convention_linkage: unit feature_support;
    numeric_value_for_edited_item: unit feature_support;
    incorrect_conf_sec_order: unit feature_support;
    define_constant_directive: unit feature_support;
    free_redefines_position: unit feature_support;
    records_mismatch_record_clause: unit feature_support;
    record_delimiter: unit feature_support;
    sequential_delimiters: unit feature_support;
    record_delim_with_fixed_recs: unit feature_support;
    missing_statement: unit feature_support;
    missing_period: unit feature_support;
    zero_length_literals: unit feature_support;
    xml_generate_extra_phrases: unit feature_support;
    continue_after: unit feature_support;
    goto_entry: unit feature_support;
    assign_variable: unit feature_support;
    assign_using_variable: unit feature_support;
    assign_ext_dyn: unit feature_support;
    assign_disk_from: unit feature_support;
    vsam_status: unit feature_support;
    self_call_recursive: unit feature_support;
    record_contains_depending_clause: unit feature_support;
    picture_l: unit feature_support;
  }
