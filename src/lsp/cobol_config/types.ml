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

type configuration = Cobol_dialect.Types.origin

(** Any object with a pretty-printing method. *)
class type showable =
  object
    method pp: Pretty.delayed
  end

(** Kind (type) of configuration options. *)
class virtual ['a] kind ~name =
  object
    val name: string = name
    method name = name
    method pp: Pretty.delayed = fun ppf -> Pretty.string ppf name
    method virtual parse: string -> 'a
  end

let all_configs: showable list ref = ref []         (* for listing of options *)
let configurable_name_style = [`Yellow; `Bold]
and configurable_kind_style = [`Cyan]

(** Any object that is configurable. *)
class virtual ['a] configurable ~name ~kind ?short doc =
  let uncapitalize_heuristics s = match s.[1] with
    | 'A' .. 'Z' | '-' -> s
    | _ -> String.uncapitalize_ascii s
    | exception Invalid_argument _ -> String.uncapitalize_ascii s
  in
  object (self)
    val name: string = name
    val kind: 'a kind = kind
    val doc: doc = doc
    method name = name
    method short: doc = match short with
      | Some s -> s
      | None -> Pretty.Simple.map uncapitalize_heuristics doc
    method pp ppf =
      let pp_name = Pretty.styles configurable_name_style @@ Pretty.string
      and pp_kind = Pretty.styles configurable_kind_style @@ Fmt.fmt "%t" in
      Pretty.print ppf "@[<4>%a@ [%a]:@\n%(%)@]"
        pp_name name pp_kind kind#pp doc
    initializer begin
      all_configs := (self :> showable) :: !all_configs;
    end
  end

type 'a feature = 'a Cobol_dialect.Types.feature
type 'a feature_support = 'a Cobol_dialect.Types.binding

(* TODO: add min/max for int values. *)
module Value = struct
  (** Valued (typed) options. *)

  (** Internal representation of typed option binding. *)
  class ['a] v option ~config:_ v =
    object
      val option: 'a configurable = option
      method value: 'a = v
    end

  (** Type of valued options *)
  class ['a] value ~kind ~name ?short doc =
    object (self)
      inherit ['a] configurable ~kind ~name ?short doc
      method from_val: config:configuration -> 'a -> 'a v =
        new v (self :> 'a configurable)
      method from_string ~(config:configuration) str: 'a v =
        let value = kind#parse str in
        new v (self :> 'a configurable) ~config value
    end

  type 'a t = 'a value

  (** Definitions *)
  let def ~name = new value ~name

  let kind_from_fmt ~name fmt =
    object
      inherit [_] kind ~name
      method parse s = Scanf.sscanf s fmt Fun.id
    end

  let int = def ~kind:(kind_from_fmt ~name:"int" "%d")
  let int_list = def ~kind:(object
    inherit [_] kind ~name:"int_list"
    method parse s = Cobol_common.Tab_stops.of_string s
  end)
  let bool = def ~kind:(kind_from_fmt ~name:"bool" "%B")

end
type 'a valued_option = 'a Value.v                                     (* alias *)
type 'a value = 'a Value.t                                             (* alias *)

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

module DIALECT = Cobol_dialect.Id
type dialect = Cobol_dialect.Types.id

module type CONFIG = sig
  val origin: Cobol_dialect.Types.origin
end

module type PP_OPTS = sig
  (** Preprocessor options*)

  (* int options *)
  val tab_stops: int list valued_option

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
  val text_column: int valued_option
  val pic_length: int valued_option
  val word_length: int valued_option
  val literal_length: int valued_option
  val numeric_literal_length: int valued_option

  (* any options *)
  val defaultbyte: defaultbyte valued_option
  val standard_define: standard valued_option
  val format: source_format_spec valued_option
  val binary_size: binary_size valued_option
  val binary_byteorder: binary_byteorder valued_option
  val assign_clause: assign_clause valued_option
  val screen_section_rules: screen_section_rules valued_option
  val dpc_in_data: dpc_in_data valued_option

  (* boolean options *)
  val filename_mapping: bool valued_option
  val pretty_display: bool valued_option
  val binary_truncate: bool valued_option
  val complex_odo: bool valued_option
  val odoslide: bool valued_option
  val indirect_redefines: bool valued_option
  val relax_syntax_checks: bool valued_option
  val ref_mod_zero_length: bool valued_option
  val relax_level_hierarchy: bool valued_option
  val select_working: bool valued_option
  val local_implies_recursive: bool valued_option
  val sticky_linkage: bool valued_option
  val move_ibm: bool valued_option
  val perform_osvs: bool valued_option
  val arithmetic_osvs: bool valued_option
  val hostsign: bool valued_option
  val program_name_redefinition: bool valued_option
  val accept_update: bool valued_option
  val accept_auto: bool valued_option
  val console_is_crt: bool valued_option
  val no_echo_means_secure: bool valued_option
  val line_col_zero_default: bool valued_option
  val display_special_fig_consts:  bool valued_option
  val binary_comp_1: bool valued_option
  val numeric_pointer:  bool valued_option
  val move_non_numeric_lit_to_numeric_is_zero:  bool valued_option
  val implicit_assign_dynamic_var:  bool valued_option
  val device_mnemonics: bool valued_option
  val xml_parse_xmlss: bool valued_option
  val areacheck: bool valued_option
  val ebcdic_symbolic_characters: bool valued_option

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
