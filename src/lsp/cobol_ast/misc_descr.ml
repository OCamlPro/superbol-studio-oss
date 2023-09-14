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

open Terms
open Operands

(* -------------------- IDENTIFICATION DIVISION (EXTRA) -------------------- *)

type informational_paragraphs =
  {
    author: string with_loc option;
    installation: string with_loc option;
    date_written: string with_loc option;
    date_compiled: string with_loc option;
    security: string with_loc option;
  }
[@@deriving show, ord]

(* ------------------------- ENVIRONMENT DIVISION -------------------------- *)
type environment_division =
  {
    env_configuration: configuration_section option;
    env_input_output: input_output_section option;
  }
[@@deriving show, ord]

(* ------------- ENVIRONMENT DIVISION / CONFIGURATION SECTION -------------- *)
and configuration_section =
  {
    source_computer_paragraph: source_computer_paragraph option;
    object_computer_paragraph: object_computer_paragraph option;
    special_names_paragraph: special_names_paragraph option;
    repository_paragraph: repository_paragraph option; (* +COB2002 *)
  }
[@@deriving show]

(* ENVIRONMENT DIVISION / CONFIGURATION SECTION / SOURCE-COMPUTER PARAGRAPH *)
and source_computer_paragraph =
  source_computer option
[@@deriving show]

and source_computer =
  {
    source_computer_name: name with_loc;
    source_computer_with_debugging_mode: bool;
  }
[@@deriving show]

(* ENVIRONMENT DIVISION / CONFIGURATION SECTION / OBJECT-COMPUTER PARAGRAPH *)
and object_computer_paragraph =
  object_computer option
[@@deriving show]

and object_computer =
  {
    object_computer_name: name with_loc;
    object_computer_clauses: object_computer_clause with_loc list;
  }
[@@deriving show]

and object_computer_clause =
  | ComputerMemorySize of                                 (* ~COB85, -COB2002 *)
      integer * memory_size_unit
  | ComputerCharClassification of                                 (* +COB2002 *)
      {
        alphanumeric: locale option;              (* at least one must be set *)
        national: locale option
      }
  | ComputerProgCollatingSeq of alphabet_specification    (* COB85 != COB2002 *)
  | ComputerSegmentLimit of integer                       (* -COB2002 *)
[@@deriving show]

and memory_size_unit =
  | MemoryWords
  | MemoryCharacters
  | MemoryModules
[@@deriving show]

and locale =
  | CharClassificationName of name with_loc
  | CharClassificationLocale
  | CharClassificationSystemDefault
  | CharClassificationUserDefault
[@@deriving show]

and alphabet_specification =                      (* At least one is required *)
  {
    alphanumeric: name with_loc option;
    national: name with_loc option;
  }
[@@deriving show]
(* ENVIRONMENT DIVISION / CONFIGURATION SECTION / SPECIAL-NAMES PARAGRAPH *)
and special_names_paragraph =
  special_names_clause with_loc list
[@@deriving show]

and special_names_clause =
  | AlphabetName of                                       (* Multiple allowed *)
      {
        alphabet_name: name with_loc;
        category: alphanumeric_or_national;
        characters: character_set;
      }
  | ClassName of                                          (* Multiple allowed *)
      {
        class_name: name with_loc;
        category: alphanumeric_or_national;
        characters: character_range list;
        source_charset: name with_loc option;
      }
  | CRTStatus of name with_loc                         (* +COB2002 *)
  | CurrencySign of                                    (* Multiple in COB2002 *)
      {
        sign: strlit;
        picture_symbol: strlit option;
      }
  | Cursor of name with_loc                                       (* +COB2002 *)
  | DecimalPointIsComma
  | DynLenStruct of                        (* +COB2002 *) (* Multiple allowed *)
      {
        name: name with_loc;
        kind: dyn_len_struct_kind;
      }
  | SpecialNameLocale of                   (* +COB2002 *) (* Multiple allowed *)
      {
        locale_name: name with_loc;
        external_name: name_or_string;
      }
  | MnemonicName of                        (* Multiple allowed *)
      {
        implementor_name: name with_loc;
        mnemonic_name: name with_loc option;
        status: status_switch option;
      }
  | SymbolicChars of                                      (* Multiple allowed *)
      {
        category: alphanumeric_or_national;
        characters: (name with_loc list * integer list) list; (* same lengths *)
        source_charset: name with_loc option;
      }
  | OrderTable of                                                 (* +COB2002 *)
      {
        ordering_name: name with_loc;
        cultural_ordering: strlit;
      }
[@@deriving show]

and alphanumeric_or_national =
  | Alphanumeric
  | National
[@@deriving show]

and character_range =
  | SingleCharacter of strlit_or_intlit
  | CharacterRange of range_items
[@@deriving show]

and characters_range =
  | CharactersRange of character_range
  | CharactersList of strlit_or_intlit list                    (* non-trivial *)
[@@deriving show]

and range_items =
  {
    start_item: strlit_or_intlit;
    end_item: strlit_or_intlit;
  }
[@@deriving show]

and character_set =
  | CharSetLocale of name with_loc option     (* +COB2002 *)
  | CharSetNative                             (* +COB2002 *)
  | CharSetStandard_1                         (* Alphanum only *)
  | CharSetStandard_2                         (* Alphanum only *)
  | CharSetUCS_4                              (* +COB2002 *) (* National only *)
  | CharSetUTF_8                              (* +COB2002 *) (* National only *)
  | CharSetUTF_16                             (* +COB2002 *) (* National only *)
  | CharSetCharacters of characters_range list                   (* non-empty *)
[@@deriving show]

and dyn_len_struct_kind =
  | DynLenPrefixed of { signed: bool; short: bool }
  | DynLenDelimited
  | DynLenPhysical of name with_loc
[@@deriving show]

and status_switch =
  | StatusSwitchOn of name with_loc
  | StatusSwitchOff of name with_loc
  | StatusSwitch of { on_: name with_loc; off: name with_loc }
[@@deriving show]

(* ENVIRONMENT DIVISION / CONFIGURATION SECTION / REPOSITORY PARAGRAPH *)
and repository_paragraph =
  specifier list
[@@deriving show]

and specifier =                                       (* TODO: inline records *)
  | ClassSpecifier of
      {
        name: name with_loc;
        external_name: strlit option;
        expands: expands option;
      }
  | InterfaceSpecifier of
      {
        name: name with_loc;
        external_name: strlit option;
        expands: expands option;
      }
  | UserFunctionSpecifier of
      {
        name: name with_loc;
        external_name: strlit option;
      }
  | IntrinsicFunctionSpecifier of
      name with_loc list                                         (* non-empty *)
  | IntrinsicFunctionAllSpecifier
  | ProgramSpecifier of
      {
        name: name with_loc;
        external_name: strlit option;
      }
  | PropertySpecifier of
      {
        name: name with_loc;
        external_name: strlit option;
      }
[@@deriving show]

and expands =
  {
    expands_name: name with_loc;
    expands_using: name with_loc list; (* non-empty *)
  }
[@@deriving show]


(* -------------- ENVIRONMENT DIVISION / INPUT-OUTPUT SECTION -------------- *)
and input_output_section =
  {
    file_control_paragraph: file_control_paragraph option; (* COB85: mandatory *)
    io_control_paragraph: io_control_paragraph option;
  }
[@@deriving show]

(* - ENVIRONMENT DIVISION / INPUT-OUTPUT SECTION / FILE-CONTROL PARAGRAPH -- *)
and file_control_paragraph =
  select list
[@@deriving show]

and select =
  {
    select_optional: bool;
    select_name: name with_loc;
    select_clauses: select_clause with_loc list;
  }
[@@deriving show]

and select_clause =
  | SelectAssign of
      {
        to_: name_or_alphanum list;
        using: name with_loc option;
      }
  | SelectAccessMode of access_mode
  | SelectAlternateRecordKey of
      {
        key: qualname;
        source: name with_loc list;
        with_duplicates: bool;
      }
  (* | SelectCollatingSequence of collating_sequence_clause *)
  | SelectCollatingSequenceOfFile of
      alphabet_specification                       (* +COB2002 *) (* Multiple *)
  | SelectCollatingSequenceOfKey of                (* +COB2002 *) (* Multiple *)
      {
        keys: name with_loc list;
        alphabet: name with_loc;
      }
  | SelectStatus of qualname
  | SelectLockMode of                                             (* +COB2002 *)
      {
        mode: lock_mode;
        with_lock: with_lock;
      }
  | SelectOrganization of organization
  | SelectPaddingCharacter of qualname_or_alphanum                (* -COB2002 *)
  | SelectRecordDelimiter of record_delimiter
  | SelectRecordKey of
      {
        key: qualname;
        source: name with_loc list;
      }
  | SelectRelativeKey of name with_loc
  | SelectReserve of integer
  | SelectSharing of sharing_mode                                 (* +COB2002 *)
[@@deriving show]

and access_mode =
  | AccessModeDynamic
  | AccessModeRandom
  | AccessModeSequential
[@@deriving show]

and lock_mode =
  | LockManual
  | LockAutomatic
[@@deriving show]

and with_lock =
  | WithLockNone
  | WithLock of { multiple: bool }
[@@deriving show]

and organization =
  | OrganizationIndexed
  | OrganizationRelative
  | OrganizationSequential
[@@deriving show]

and record_delimiter =
  | Standard_1
[@@deriving show]

(* -- ENVIRONMENT DIVISION / INPUT-OUTPUT SECTION / I-O-CONTROL PARAGRAPH -- *)
and io_control_paragraph =
  io_control_entry option
[@@deriving show]

and io_control_entry =
  {
    io_control_rerun_clauses: rerun_clause with_loc list;         (* -COB2002 *)
    io_control_same_area_clauses: same_area_clause with_loc list;
    io_control_multiple_file_clauses: multiple_file_clause with_loc list; (* -COB2002 *)
  }
[@@deriving show]

and rerun_clause =
  {
    rerun_on: name with_loc option;
    rerun_every: rerun_frequency;
  }
[@@deriving show]

and rerun_frequency =
  | RerunEndOf of name with_loc
  | RerunRecords of integer * name with_loc
  | RerunClockUnits of integer
  | RerunCond of name with_loc
[@@deriving show]

and same_area_clause =
  {
    same_area_source: area_source;
    same_area_file_name: name with_loc;
    same_area_file_names: name with_loc list; (* non-empty *)
  }
[@@deriving show]

and area_source =
  | AreaSourceFile
  | AreaSourceRecord
  | AreaSourceSortMerge
[@@deriving show]

and multiple_file_clause =
  file_portion list
[@@deriving show]

and file_portion =
  {
    file_portion_name: name with_loc;
    file_portion_position: integer option;
  }
[@@deriving show]

type options_paragraph =
  options_clause with_loc list
[@@deriving show, ord]

and options_clause =
  | Arithmetic of arithmetic_mode
  | DefaultRoundedMode of rounding_mode
  | EntryConvention of entry_convention
  | FloatBinaryDefault of Data_descr.endianness_mode
  | FloatDecimalDefault of Data_descr.encoding_endianness               (* 1+ *)
  | IntermediateRounding of rounding_mode (* not all are valid (TODO:
                                             restriction with type param) *)
[@@deriving show]

and arithmetic_mode =
  | Native
  | Standard        (* ~COB2002 *)
  | StandardBinary
  | StandardDecimal
[@@deriving show]

(* Other conventions may be defined by the implementor *)
and entry_convention =
  | COBOL
[@@deriving show]
