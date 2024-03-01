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

open PTree_types

open Cobol_common.Srcloc.TYPES
open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)
open Terms_visitor

let todo    x = Cobol_common.Visitor.todo    __FILE__ x
let partial x = Cobol_common.Visitor.partial __FILE__ x

(* --- *)

class virtual ['a] folder = object
  inherit ['a] Terms_visitor.folder
  inherit! ['a] Operands_visitor.folder
  method fold_access_mode: (access_mode, 'a) fold = default
  method fold_alphabet_specification: (alphabet_specification, 'a) fold = default
  method fold_area_source: (area_source, 'a) fold = default
  method fold_arithmetic_mode: (arithmetic_mode, 'a) fold = default
  method fold_character_category: (character_category, 'a) fold = default
  method fold_character_range: (character_range, 'a) fold = default
  method fold_character_set: (character_set, 'a) fold = default
  method fold_characters_range: (characters_range, 'a) fold = default
  method fold_dyn_len_struct_kind: (dyn_len_struct_kind, 'a) fold = default
  method fold_entry_convention: (entry_convention, 'a) fold = default
  method fold_expands: (expands, 'a) fold = default
  method fold_file_portion: (file_portion, 'a) fold = default
  method fold_io_control_entry: (io_control_entry, 'a) fold = default
  method fold_locale: (locale, 'a) fold = default
  method fold_lock_mode: (lock_mode, 'a) fold = default
  method fold_memory_size_unit: (memory_size_unit, 'a) fold = default
  method fold_multiple_file_clause: (multiple_file_clause, 'a) fold = default
  method fold_object_computer: (object_computer, 'a) fold = default
  method fold_object_computer_clause: (object_computer_clause, 'a) fold = default
  method fold_options_clause: (options_clause, 'a) fold = default
  method fold_organization: (organization, 'a) fold = default
  method fold_range_items: (range_items, 'a) fold = default
  method fold_record_delimiter: (record_delimiter, 'a) fold = default
  method fold_rerun_clause: (rerun_clause, 'a) fold = default
  method fold_rerun_frequency: (rerun_frequency, 'a) fold = default
  method fold_same_area_clause: (same_area_clause, 'a) fold = default
  method fold_select: (select, 'a) fold = default
  method fold_select_clause: (select_clause, 'a) fold = default
  method fold_source_computer: (source_computer, 'a) fold = default
  method fold_special_names_clause': (special_names_clause with_loc, 'a) fold = default
  method fold_special_names_clause: (special_names_clause, 'a) fold = default
  method fold_specifier: (specifier, 'a) fold = default
  method fold_status_switch: (status_switch, 'a) fold = default
  method fold_symbolic_characters_map: (symbolic_characters_map, 'a) fold = default
  method fold_with_lock: (with_lock, 'a) fold = default

  method fold_file_control_paragraph': (file_control_paragraph with_loc, 'a) fold = default
  method fold_file_control_paragraph: (file_control_paragraph, 'a) fold = default
  method fold_informational_paragraph': (informational_paragraph with_loc, 'a) fold = default
  method fold_informational_paragraphs: (informational_paragraphs, 'a) fold = default
  method fold_io_control_paragraph': (io_control_paragraph with_loc, 'a) fold = default
  method fold_io_control_paragraph: (io_control_paragraph, 'a) fold = default
  method fold_object_computer_paragraph': (object_computer_paragraph with_loc, 'a) fold = default
  method fold_object_computer_paragraph: (object_computer_paragraph, 'a) fold = default
  method fold_options_paragraph': (options_paragraph with_loc, 'a) fold = default
  method fold_options_paragraph: (options_paragraph, 'a) fold = default
  method fold_repository_paragraph': (repository_paragraph with_loc, 'a) fold = default
  method fold_repository_paragraph: (repository_paragraph, 'a) fold = default
  method fold_source_computer_paragraph': (source_computer_paragraph with_loc, 'a) fold = default
  method fold_source_computer_paragraph: (source_computer_paragraph, 'a) fold = default
  method fold_special_names_paragraph': (special_names_paragraph with_loc, 'a) fold = default
  method fold_special_names_paragraph: (special_names_paragraph, 'a) fold = default

  method fold_configuration_section': (configuration_section with_loc, 'a) fold = default
  method fold_configuration_section: (configuration_section, 'a) fold = default
  method fold_input_output_section': (input_output_section with_loc, 'a) fold = default
  method fold_input_output_section: (input_output_section, 'a) fold = default

  method fold_environment_division': (environment_division with_loc, 'a) fold = default
  method fold_environment_division: (environment_division, 'a) fold = default
end

let todo    x = todo    __MODULE__ x
and partial x = partial __MODULE__ x

let fold_access_mode (v: _ #folder) =
  leaf v#fold_access_mode

let fold_alphabet_specification (v: _ #folder) =
  handle v#fold_alphabet_specification
    ~continue:begin fun { alphanumeric; national } x -> x
      >> fold_option ~fold:fold_name' v alphanumeric
      >> fold_option ~fold:fold_name' v national
    end

let fold_arithmetic_mode (v: _ #folder) =
  leaf v#fold_arithmetic_mode

let fold_range_items (v: _ #folder) =
  handle v#fold_range_items
    ~continue:begin fun { start_item; end_item } x -> x
      >> fold_strlit_or_intlit v start_item
      >> fold_strlit_or_intlit v end_item
    end

let fold_character_category (v: _ #folder) =
  leaf v#fold_character_category

let fold_character_range (v: _ #folder) =
  handle v#fold_character_range
    ~continue:begin function
      | SingleCharacter s -> fold_strlit_or_intlit v s
      | CharacterRange r -> fold_range_items v r
    end

let fold_characters_range (v: _ #folder) =
  handle v#fold_characters_range
    ~continue:begin function
      | CharactersRange r -> fold_character_range v r
      | CharactersList l -> fold_list ~fold:fold_strlit_or_intlit v l
    end

let fold_character_set (v: _ #folder) =
  handle v#fold_character_set
    ~continue:begin function
      | CharSetLocale n -> fold_name'_opt v n
      | CharSetNative
      | CharSetStandard_1
      | CharSetStandard_2
      | CharSetUCS_4
      | CharSetUTF_8
      | CharSetUTF_16 -> Fun.id
      | CharSetCharacters r -> fold_list ~fold:fold_characters_range v r
    end

let fold_rerun_frequency (v: _ #folder) =
  handle v#fold_rerun_frequency
    ~continue:begin function
      | RerunEndOf n | RerunCond n -> fold_name' v n
      | RerunRecords (i, n) -> fun x -> x >> fold_integer v i >> fold_name' v n
      | RerunClockUnits i -> fold_integer v i
    end

let fold_area_source (v: _ #folder) =
  leaf v#fold_area_source

let fold_file_portion (v: _ #folder) =
  handle v#fold_file_portion
    ~continue:begin fun { file_portion_name; file_portion_position } x -> x
      >> fold_name' v file_portion_name
      >> fold_integer_opt v file_portion_position
    end

let fold_dyn_len_struct_kind (v: _ #folder) =
  handle v#fold_dyn_len_struct_kind
    ~continue:begin fun k x -> match k with
      | DynLenPrefixed { signed; short } -> x
          >> fold_bool v signed
          >> fold_bool v short
      | DynLenDelimited ->
          x
      | DynLenPhysical n -> x
          >> fold_name' v n
    end

let fold_entry_convention (v: _ #folder) =
  leaf v#fold_entry_convention

let fold_expands (v: _ #folder) =
  handle v#fold_expands
    ~continue:begin fun { expands_name; expands_using } x -> x
      >> fold_name' v expands_name
      >> fold_name'_list v expands_using
    end

let fold_locale (v: _ #folder) =
  handle v#fold_locale
    ~continue:begin function
      | CharClassificationName n -> fold_name' v n
      | CharClassificationLocale
      | CharClassificationSystemDefault
      | CharClassificationUserDefault -> Fun.id
    end

let fold_lock_mode (v: _ #folder) =
  leaf v#fold_lock_mode

let fold_memory_size_unit (v: _ #folder) =
  leaf v#fold_memory_size_unit

let fold_organization (v: _ #folder) =
  leaf v#fold_organization

let fold_record_delimiter (v: _ #folder) =
  leaf v#fold_record_delimiter

let fold_source_computer (v: _ #folder) =
  handle v#fold_source_computer
    ~continue:begin fun { source_computer_name;
                          source_computer_with_debugging_mode } x -> x
      >> fold_name' v source_computer_name
      >> fold_bool v source_computer_with_debugging_mode
    end

let fold_specifier (v: _ #folder) =
  handle v#fold_specifier
    ~continue:begin fun s x -> match s with
      | ClassSpecifier { name; external_name; expands }
      | InterfaceSpecifier { name; external_name; expands } -> x
          >> fold_name' v name
          >> fold_strlit_opt v external_name
          >> fold_option ~fold:fold_expands v expands
      | UserFunctionSpecifier { name; external_name }
      | ProgramSpecifier { name; external_name }
      | PropertySpecifier { name; external_name } -> x
          >> fold_name' v name
          >> fold_strlit_opt v external_name
      | IntrinsicFunctionSpecifier names -> x
          >> fold_name'_list v names
      | IntrinsicFunctionAllSpecifier ->
          x
    end

let fold_status_switch (v: _ #folder) =
  handle v#fold_status_switch
    ~continue:begin fun s x -> match s with
      | StatusSwitchOn n
      | StatusSwitchOff n -> x
          >> fold_name' v n
      | StatusSwitch { on_; off } -> x
          >> fold_name' v on_
          >> fold_name' v off
    end

let fold_symbolic_characters_map (v: _ #folder) =
  handle v#fold_symbolic_characters_map
    ~continue:begin fun (names, ints) x -> x
      >> fold_name'_list v names
      >> fold_list ~fold:fold_integer v ints
    end

let fold_with_lock (v: _ #folder) =
  handle v#fold_with_lock
    ~continue:begin function
      | WithLockNone -> Fun.id
      | WithLock { multiple } -> fold_bool v multiple
    end

(* --- *)

let fold_rerun_clause (v: _ #folder) =
  handle v#fold_rerun_clause
    ~continue:begin fun { rerun_on; rerun_every } x -> x
      >> fold_name'_opt v rerun_on
      >> fold_rerun_frequency v rerun_every
    end

let fold_same_area_clause (v: _ #folder) =
  handle v#fold_same_area_clause
    ~continue:begin fun { same_area_source;
                          same_area_file_name;
                          same_area_file_names } x -> x
      >> fold_area_source v same_area_source
      >> fold_name' v same_area_file_name
      >> fold_name'_list v same_area_file_names
    end

let fold_multiple_file_clause (v: _ #folder) =
  handle v#fold_multiple_file_clause
    ~continue:(fold_list ~fold:fold_file_portion v)

let fold_io_control_entry (v: _ #folder) =
  handle v#fold_io_control_entry
    ~continue:begin fun { io_control_rerun_clauses;
                          io_control_same_area_clauses;
                          io_control_multiple_file_clauses } x -> x
      >> fold_with_loc_list v io_control_rerun_clauses
        ~fold:fold_rerun_clause
      >> fold_with_loc_list v io_control_same_area_clauses
        ~fold:fold_same_area_clause
      >> fold_with_loc_list v io_control_multiple_file_clauses
        ~fold:fold_multiple_file_clause
    end

let fold_object_computer_clause (v: _ #folder) =
  handle v#fold_object_computer_clause
    ~continue:begin fun c x -> match c with
      | ComputerMemorySize (i, u) -> x
          >> fold_integer v i
          >> fold_memory_size_unit v u
      | ComputerCharClassification { alphanumeric; national } -> x
          >> fold_option ~fold:fold_locale v alphanumeric
          >> fold_option ~fold:fold_locale v national
      | ComputerProgCollatingSeq a -> x
          >> fold_alphabet_specification v a
      | ComputerSegmentLimit i -> x
          >> fold_integer v i
    end

let fold_object_computer (v: _ #folder) =
  handle v#fold_object_computer
    ~continue:begin fun { object_computer_name;
                          object_computer_clauses } x -> x
      >> fold_name' v object_computer_name
      >> fold_with_loc_list v object_computer_clauses
        ~fold:fold_object_computer_clause
    end

let fold_special_names_clause (v: _ #folder) =
  handle v#fold_special_names_clause
    ~continue:begin fun c x -> match c with
      | AlphabetName { alphabet_name; category; characters } -> x
          >> fold_name' v alphabet_name
          >> fold_character_category v category
          >> fold_character_set v characters
      | ClassName { class_name; category; characters; source_charset } -> x
          >> fold_name' v class_name
          >> fold_character_category v category
          >> fold_list ~fold:fold_character_range v characters
          >> fold_name'_opt v source_charset
      | CRTStatus n
      | Cursor n -> x
          >> fold_name' v n
      | CurrencySign { sign; picture_symbol } -> x
          >> fold_strlit v sign
          >> fold_strlit_opt v picture_symbol
      | DecimalPointIsComma ->
          x
      | DynLenStruct { name; kind } -> x
          >> fold_name' v name
          >> fold_dyn_len_struct_kind v kind
      | SpecialNameLocale { locale_name; external_name } -> x
          >> fold_name' v locale_name
          >> fold_name_or_string v external_name
      | MnemonicName { implementor_name; mnemonic_name; status } -> x
          >> fold_name' v implementor_name
          >> fold_name'_opt v mnemonic_name
          >> fold_option ~fold:fold_status_switch v status
      | SymbolicChars { category; character_maps; source_charset } -> x
          >> fold_character_category v category
          >> fold_list ~fold:fold_symbolic_characters_map v character_maps
          >> fold_name'_opt v source_charset
      | OrderTable { ordering_name; cultural_ordering } -> x
          >> fold_name' v ordering_name
          >> fold_strlit v cultural_ordering
    end

let fold_special_names_clause' (v: _ #folder) =
  handle' v#fold_special_names_clause' ~fold:fold_special_names_clause v

let fold_select_clause (v: _ #folder) =
  handle v#fold_select_clause
    ~continue:begin fun c x -> match c with
      | SelectAssign { to_; using } -> x
          >> fold_list ~fold:fold_name_or_alphanum v to_
          >> fold_name'_opt v using
      | SelectAccessMode a -> x
          >> fold_access_mode v a
      | SelectAlternateRecordKey { key; source; with_duplicates } -> x
          >> fold_qualname v key
          >> fold_name'_list v source
          >> fold_bool v with_duplicates
      | SelectCollatingSequenceOfFile a -> x
          >> fold_alphabet_specification v a
      | SelectCollatingSequenceOfKey { keys; alphabet } -> x
          >> fold_name'_list v keys
          >> fold_name' v alphabet
      | SelectStatus n -> x
          >> fold_qualname v n
      | SelectLockMode { mode; with_lock } -> x
          >> fold_lock_mode v mode
          >> fold_with_lock v with_lock
      | SelectOrganization o -> x
          >> fold_organization v o
      | SelectPaddingCharacter c -> x
          >> fold_qualname_or_alphanum v c
      | SelectRecordDelimiter d -> x
          >> fold_record_delimiter v d
      | SelectRecordKey { key; source } -> x
          >> fold_qualname v key
          >> fold_name'_list v source
      | SelectRelativeKey n -> x
          >> fold_name' v n
      | SelectReserve i -> x
          >> fold_integer v i
      | SelectSharing s -> x
          >> Operands_visitor.fold_sharing_mode v s
    end

let fold_select (v: _ #folder) =
  handle v#fold_select
    ~continue:begin fun { select_optional; select_name; select_clauses } x -> x
      >> fold_bool v select_optional
      >> fold_name' v select_name
      >> fold_with_loc_list ~fold:fold_select_clause v select_clauses
    end

let fold_options_clause (v: _ #folder) =
  handle v#fold_options_clause
    ~continue:begin function
      | Arithmetic a -> fold_arithmetic_mode v a
      | DefaultRoundedMode r
      | IntermediateRounding r -> fold_rounding_mode v r
      | EntryConvention e -> fold_entry_convention v e
      | FloatBinaryDefault e -> Data_descr_visitor.fold_endianness_mode v e
      | FloatDecimalDefault e -> Data_descr_visitor.fold_encoding_endianness v e
    end

(* --- *)

let fold_file_control_paragraph (v: _ #folder) =
  handle v#fold_file_control_paragraph
    ~continue:(fold_list ~fold:fold_select v)

let fold_file_control_paragraph' (v: _ #folder) =
  handle' v#fold_file_control_paragraph' v
    ~fold:fold_file_control_paragraph

let fold_io_control_paragraph (v: _ #folder) =
  handle v#fold_io_control_paragraph
    ~continue:(fold_option ~fold:fold_io_control_entry v)

let fold_io_control_paragraph' (v: _ #folder) =
  handle' v#fold_io_control_paragraph' v
    ~fold:fold_io_control_paragraph

let fold_input_output_section (v: _ #folder) =
  handle v#fold_input_output_section
    ~continue:begin fun { file_control_paragraph; io_control_paragraph } x -> x
      >> fold_option v file_control_paragraph
        ~fold:fold_file_control_paragraph'
      >> fold_option v io_control_paragraph
        ~fold:fold_io_control_paragraph'
    end

let fold_input_output_section' (v: _ #folder) =
  handle' v#fold_input_output_section' v
    ~fold:fold_input_output_section

let fold_informational_paragraph' (v: _ #folder) =
  leaf' v#fold_informational_paragraph' v

let fold_informational_paragraphs (v: _ #folder) =
  handle v#fold_informational_paragraphs
    ~continue:(fold_list ~fold:fold_informational_paragraph' v)

let fold_options_paragraph (v: _ #folder) =
  handle v#fold_options_paragraph
    ~continue:(fold_with_loc_list ~fold:fold_options_clause v)

let fold_options_paragraph' (v: _ #folder) =
  handle' v#fold_options_paragraph' v
    ~fold:fold_options_paragraph

let fold_source_computer_paragraph (v: _ #folder) =
  handle v#fold_source_computer_paragraph
    ~continue:(fold_option ~fold:fold_source_computer v)

let fold_source_computer_paragraph' (v: _ #folder) =
  handle' v#fold_source_computer_paragraph' v
    ~fold:fold_source_computer_paragraph

let fold_object_computer_paragraph (v: _ #folder) =
  handle v#fold_object_computer_paragraph
    ~continue:(fold_option ~fold:fold_object_computer v)

let fold_object_computer_paragraph' (v: _ #folder) =
  handle' v#fold_object_computer_paragraph'
    ~fold:fold_object_computer_paragraph v

let fold_repository_paragraph (v: _ #folder) =
  handle v#fold_repository_paragraph
    ~continue:(fold_list ~fold:fold_specifier v)

let fold_repository_paragraph' (v: _ #folder) =
  handle' v#fold_repository_paragraph' v
    ~fold:fold_repository_paragraph

let fold_special_names_paragraph (v: _ #folder) =
  handle v#fold_special_names_paragraph
    ~continue:(fold_list ~fold:fold_special_names_clause' v)

let fold_special_names_paragraph' (v: _ #folder) =
  handle' v#fold_special_names_paragraph' ~fold:fold_special_names_paragraph v

let fold_configuration_section (v: _ #folder) =
  handle v#fold_configuration_section
    ~continue:begin fun { source_computer_paragraph;
                          object_computer_paragraph;
                          special_names_paragraph;
                          repository_paragraph } x -> x
      >> fold_option v source_computer_paragraph
        ~fold:fold_source_computer_paragraph'
      >> fold_option v object_computer_paragraph
        ~fold:fold_object_computer_paragraph'
      >> fold_option v special_names_paragraph
        ~fold:fold_special_names_paragraph'
      >> fold_option v repository_paragraph
        ~fold:fold_repository_paragraph'
    end

let fold_configuration_section' (v: _ #folder) =
  handle' v#fold_configuration_section' v
    ~fold:fold_configuration_section

let fold_environment_division (v: _ #folder) =
  handle v#fold_environment_division
    ~continue:begin fun { env_configuration; env_input_output } x -> x
      >> fold_option ~fold:fold_configuration_section' v env_configuration
      >> fold_option ~fold:fold_input_output_section' v env_input_output
    end

let fold_environment_division' (v: _ #folder) =
  handle' v#fold_environment_division' v
    ~fold:fold_environment_division
