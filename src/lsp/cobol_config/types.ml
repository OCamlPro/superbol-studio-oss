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


module type CONFIG = sig
  val origin: Cobol_dialect.Types.origin
end

module type PP_OPTS = sig
  (** Preprocessor options*)

  (* int options *)
  val tab_stops: int list

  (* support options *)
  val comment_paragraphs: unit feature_support
  val safe_partial_replacing_when_src_literal: [`Safe | `Unsafe] feature_support
end

module type COMP_OPTS = sig
  (** Compiler options *)

  (* reserved words *)
  val words: words_spec
  val intrinsic_functions: StringSet.t
  val system_names: StringSet.t
  val registers: StringSet.t

  (* int options *)
  val text_column: int
  val pic_length: int
  val word_length: int
  val literal_length: int
  val numeric_literal_length: int

  (* any options *)
  val defaultbyte: defaultbyte
  val standard_define: standard
  val format: source_format_spec
  val binary_size: binary_size
  val binary_byteorder: binary_byteorder
  val assign_clause: assign_clause
  val screen_section_rules: screen_section_rules
  val dpc_in_data: dpc_in_data

  (* boolean options *)
  val filename_mapping: bool
  val pretty_display: bool
  val binary_truncate: bool
  val complex_odo: bool
  val odoslide: bool
  val indirect_redefines: bool
  val relax_syntax_checks: bool
  val ref_mod_zero_length: bool
  val relax_level_hierarchy: bool
  val select_working: bool
  val local_implies_recursive: bool
  val sticky_linkage: bool
  val move_ibm: bool
  val perform_osvs: bool
  val arithmetic_osvs: bool
  val hostsign: bool
  val program_name_redefinition: bool
  val accept_update: bool
  val accept_auto: bool
  val console_is_crt: bool
  val no_echo_means_secure: bool
  val line_col_zero_default: bool
  val display_special_fig_consts:  bool
  val binary_comp_1: bool
  val numeric_pointer:  bool
  val move_non_numeric_lit_to_numeric_is_zero:  bool
  val implicit_assign_dynamic_var:  bool
  val device_mnemonics: bool
  val xml_parse_xmlss: bool
  val areacheck: bool
  val ebcdic_symbolic_characters: bool

  (* support options *)
  val control_division: unit feature_support
  val memory_size_clause: unit feature_support
  val multiple_file_tape_clause: unit feature_support
  val label_records_clause: unit feature_support
  val value_of_clause: unit feature_support
  val data_records_clause: unit feature_support
  val top_level_occurs_clause: unit feature_support
  val same_as_clause: unit feature_support
  val type_to_clause: unit feature_support
  val usage_type: unit feature_support
  val synchronized_clause: unit feature_support
  val sync_left_right: unit feature_support
  val special_names_clause: unit feature_support
  val goto_statement_without_name: unit feature_support
  val stop_literal_statement: unit feature_support
  val stop_identifier_statement: unit feature_support
  val stop_error_statement: unit feature_support
  val debugging_mode: unit feature_support
  val use_for_debugging: unit feature_support
  val padding_character_clause: unit feature_support
  val next_sentence_phrase: unit feature_support
  val listing_statements: unit feature_support
  val title_statement: unit feature_support
  val entry_statement: unit feature_support
  val move_noninteger_to_alphanumeric: unit feature_support
  val move_figurative_constant_to_numeric: unit feature_support
  val move_figurative_space_to_numeric: unit feature_support
  val move_figurative_quote_to_numeric: unit feature_support
  val odo_without_to: unit feature_support
  val section_segments: unit feature_support
  val alter_statement: unit feature_support
  val call_overflow: unit feature_support
  val numeric_boolean: unit feature_support
  val hexadecimal_boolean: unit feature_support
  val national_literals: unit feature_support
  val hexadecimal_national_literals: unit feature_support
  val national_character_literals: unit feature_support
  val hp_octal_literals: unit feature_support
  val acu_literals: unit feature_support
  val word_continuation: unit feature_support
  val not_exception_before_exception: unit feature_support
  val accept_display_extensions: unit feature_support
  val larger_redefines: unit feature_support
  val symbolic_constant: unit feature_support
  val constant_78: unit feature_support
  val constant_01: unit feature_support
  val perform_varying_without_by: unit feature_support
  val reference_out_of_declaratives: unit feature_support
  val program_prototypes: unit feature_support
  val call_convention_mnemonic: unit feature_support
  val call_convention_linkage: unit feature_support
  val numeric_value_for_edited_item: unit feature_support
  val incorrect_conf_sec_order: unit feature_support
  val define_constant_directive: unit feature_support
  val free_redefines_position: unit feature_support
  val records_mismatch_record_clause: unit feature_support
  val record_delimiter: unit feature_support
  val sequential_delimiters: unit feature_support
  val record_delim_with_fixed_recs: unit feature_support
  val missing_statement: unit feature_support
  val missing_period: unit feature_support
  val zero_length_literals: unit feature_support
  val xml_generate_extra_phrases: unit feature_support
  val continue_after: unit feature_support
  val goto_entry: unit feature_support
  val assign_variable: unit feature_support
  val assign_using_variable: unit feature_support
  val assign_ext_dyn: unit feature_support
  val assign_disk_from: unit feature_support
  val vsam_status: unit feature_support
  val self_call_recursive: unit feature_support
  val record_contains_depending_clause: unit feature_support
  val picture_l: unit feature_support
end

module type T = sig
  include CONFIG
  include PP_OPTS
  include COMP_OPTS
end

type t = (module T)
