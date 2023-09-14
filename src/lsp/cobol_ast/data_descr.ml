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

(* ----------------------------- DATA DIVISION ----------------------------- *)
type format_clause =
 | Bit
 | Character
 | Numeric
[@@deriving show, ord]


type file_block_contents =
  | FileBlockContainsCharacters
  | FileBlockContainsRecords
[@@deriving show, ord]


type record_clause =
  | FixedLength of integer
  | VariableLength of
      {
        min_length: integer option;
        max_length: integer option;
        depending: qualname with_loc option;
      }
  | FixedOrVariableLength of
      {
        min_length: integer;
        max_length: integer;
      }
[@@deriving show, ord]

type label_clause =
  | LabelStandard
  | LabelOmitted
[@@deriving show, ord]

type file_data_clause =
  name with_loc list
[@@deriving show, ord]

type file_linage_clause =                                  (* file descr only *)
  {
    file_linage_lines: qualname_or_intlit;
    file_linage_with_footing_at: qualname_or_intlit option;
    file_linage_lines_at_top: qualname_or_intlit option;
    file_linage_lines_at_bottom: qualname_or_intlit option;
  }
[@@deriving show, ord]

type data_level = int
[@@deriving show, ord]

type rename_item =
  {
    rename_level: data_level with_loc;
    rename_to: name with_loc;
    rename_renamed: qualname;
    rename_through: qualname option;
  }
[@@deriving show, ord]


type condition_name_item =
  {
    condition_name_level: data_level with_loc; (* is always 88 *)
    condition_name: name with_loc;
    condition_name_values: condition_name_value list;
    condition_name_alphabet: name with_loc option;
    condition_name_when_false: literal option;
  }
[@@deriving show, ord]

and condition_name_value =
  {
    condition_name_value: literal;
    condition_name_through: literal option;
  }
[@@deriving show, ord]


type data_name =
  | DataName of name with_loc
  | DataFiller
[@@deriving show, ord]

(* Format seen on IBM: PICTURE ... SIZE ... LOCALE ... *)
type locale_phrase =
  {
    locale_name: name with_loc option;
    locale_size: integer;
  }
[@@deriving show, ord]

type sign_clause =
  {
    sign_position: sign_position;
    sign_separate_character: bool;
  }
[@@deriving show, ord]

and sign_position = LeadingSign | TrailingSign

type report_screen_usage_clause =
  | Display
  | National
[@@deriving show, ord]

type external_clause =
  strlit option
[@@deriving show, ord]

type group_usage_clause =
  | GroupUsageBit
  | GroupUsageNational
[@@deriving show, ord]

type data_occurs_clause =
  | OccursFixed of
      {
        times: integer;
        key_is: sort_spec list;
        indexed_by: name with_loc list;
      }
  | OccursDepending of
      {
        from: integer;
        to_: integer;
        depending: qualname with_loc;
        key_is: sort_spec list;
        indexed_by: name with_loc list;
      }
  | OccursDynamic of
      {
        capacity_in: name with_loc option;
        from: integer option;
        to_: integer option;
        initialized: bool;
        key_is: sort_spec list;
        indexed_by: name with_loc list;
      }
[@@deriving show, ord]

and sort_spec =
  {
    sort_key_direction: sort_direction;
    sort_key_names: qualname list;
  }
[@@deriving show, ord]

and sort_direction =
  | SortAscending
  | SortDescending
[@@deriving show, ord]

type data_varying =
  {
    data_varying: name with_loc;
    data_varying_from: expression option;
    data_varying_by: expression option;
  }
[@@deriving show, ord]

type select_when_clause =
  | SelectWhen of name with_loc
  | SelectWhenOther
[@@deriving show, ord]

type synchronized_clause =
  | SynchronizedLeft
  | SynchronizedRight
  | SynchronizedDefault
[@@deriving show, ord]


type property_clause =
  {
    property_with_no: property_kind option;
    property_is_final: bool;
  }
[@@deriving show, ord]

and property_kind =
  | PropertyGet
  | PropertySet
[@@deriving show, ord]


type usage_clause =
 | Binary
 | BinaryChar of signedness option                                (* +COB2002 *)
 | BinaryShort of signedness option                               (* +COB2002 *)
 | BinaryLong of signedness option                                (* +COB2002 *)
 | BinaryDouble of signedness option                              (* +COB2002 *)
 | Bit                                                            (* +COB2002 *)
 | Display
 | FloatBinary32 of endianness_mode option                        (* +COB2002 *)
 | FloatBinary64 of endianness_mode option                        (* +COB2002 *)
 | FloatBinary128 of endianness_mode option                       (* +COB2002 *)
 | FloatDecimal16 of encoding_endianness                          (* +COB2002 *)
 | FloatDecimal34 of encoding_endianness                          (* +COB2002 *)
 | FloatExtended                                                  (* +COB2002 *)
 | FloatLong                                                      (* +COB2002 *)
 | FloatShort                                                     (* +COB2002 *)
 | Index
 | National                                                       (* +COB2002 *)
 | ObjectReference of object_reference_kind option                (* +COB2002 *)
 | PackedDecimal
 | Pointer of name with_loc option                                (* +COB2002 *)
 | FunctionPointer of name with_loc                               (* +COB2002 *)
 | ProgramPointer of name with_loc option                         (* +COB2002 *)
 | UsagePending of [`Comp0 | `Comp1 | `Comp5 | `Comp6 | `CompX |
                    `CompN | `Comp9 | `Comp10 | `Comp15 ]
[@@deriving show, ord]

and signedness =
 | Signed
 | Unsigned
[@@deriving show]

and endianness_mode =
 | HighOrderLeft
 | HighOrderRight
[@@deriving show]

and encoding_mode =
 | BinaryEncoding
 | DecimalEncoding
[@@deriving show]

and encoding_endianness =
  {
    encoding_mode: encoding_mode option;
    encoding_endianness: endianness_mode option;
  }
[@@deriving show]

and object_reference_kind =
  | ActiveClass of
      {
        factory_of: bool;
      }
  | Name of
      {
        class_or_interface_name: name with_loc;
        factory_of: bool;
        only: bool;
      }
[@@deriving show]

type validation_clause =
  | Class of class_clause
  | Default of ident_or_literal option
  | Destination of ident list (* non-empty *)
  | InvalidWhen of condition list (* non-empty *)
  | PresentWhen of condition
  | Varying of data_varying list
  | ValidateStatus of
      {
        is_: ident_or_literal;
        when_: validate_when;
        on: validation_stage list;
        for_: ident list; (* non-empty *)
      }
[@@deriving show, ord]

and class_clause =
 | Alphabetic
 | AlphabeticLower
 | AlphabeticUpper
 | Boolean
 | Numeric
 | ClassOrAlphabet of name with_loc
[@@deriving show]

and validate_when =
  | ValidateWhenError
  | ValidateWhenNoError
[@@deriving show]

and validation_stage =
  | ValidationStageFormat
  | ValidationStageContent
  | ValidationStageRelation
[@@deriving show]

type data_value_clause =
  | ValueData of literal
  | ValueTable of table_data_value list
[@@deriving show]

and table_data_value =
  {
    table_data_values: literal list; (* non-empty *)
    table_data_from: subscript list; (* non-empty *)
    table_data_to: subscript list;
  }
[@@deriving show]

type report_data_name_or_final =
 | ReportDataName of qualident
 | ReportFinal
[@@deriving show, ord]

type report_type_clause =
  | Detail
  | ReportHeading
  | ReportFooting
  | PageHeading
  | PageFooting
  | ControlHeading of (report_data_name_or_final * bool) option
  | ControlFooting of report_data_name_or_final option
[@@deriving show, ord]

type next_group_clause =
  | ReportNextAbsolute of integer
  | ReportNextRelative of integer
  | ReportNextNextPage of bool
[@@deriving show, ord]

type column_position =
  | ColumnAbsolute of integer
  | ColumnRelative of integer
[@@deriving show, ord]

type alignment =
  | AlignLeft
  | AlignRight
  | AlignCenter
[@@deriving show, ord]

type line_position =
  | LineAbsolute of integer * bool
  | LineRelative of integer
  | LineOnNextPage
[@@deriving show, ord]

type sum_phrase =
  {
    sum_operands: expression list; (* non-empty *)
    sum_upon_items: name with_loc list;
  }
[@@deriving show, ord]

type polarity =
  | Plus
  | Minus
[@@deriving show, ord]

type blank_clause =
  | Line
  | Screen
[@@deriving show, ord]

type erase_clause =
  | EndOfLine
  | EndOfScreen
[@@deriving show, ord]

type screen_attribute_clause =
  | Bell
  | Blink
  | Highlight
  | Lowlight
  | ReverseVideo
  | Underline
  | ForegroundColor of ident_or_intlit
  | BackgroundColor of ident_or_intlit
[@@deriving show, ord]

type screen_line_column_clause =
  | Absolute of ident_or_intlit
  | Relative of polarity * ident_or_intlit
[@@deriving show, ord]

type source_destination_clause =
  | From of ident_or_literal
  | To of ident
  | Using of ident
  | Value of literal
[@@deriving show, ord]

type value_of_clause =
  {
    value_of_valued: name with_loc;
    value_of_value: qualname_or_literal;
  }
[@@deriving show, ord]

type report_clause =
  | Global
  | Code of ident
  | Control of
      {
        final: bool;
        controls: name with_loc list;
      }
  | PageLimit of
      {
        lines: integer option;
        columns: integer option;
        heading: integer option;
        first_detail: integer option;
        last_control_heading: integer option;
        last_detail: integer option;
        footing: integer option;
      }
[@@deriving show, ord]


type constant_item =
  {
    constant_level: data_level with_loc;      (* is a constant *)     (* TODO: check \in {"1", "01"} *)
    constant_name: data_name with_loc option; (* ident only (NB:refine the type???) *)
    constant_global: bool;
    constant_value: constant_value with_loc;
  }
[@@deriving show, ord]

and constant_value =
  | ConstExpr of expression                                 (* or plain ident *)
  | ConstByteLength of name with_loc
  | ConstLength of name with_loc
  | ConstFrom of name with_loc                        (* compilation variable *)
[@@deriving show, ord]


(* --- COMMUNICATION SECTION --- *)

type comm_clause =
  | CommSymbolic of comm_channel * name with_loc
  | CommDestinationCount of name with_loc                      (* OUTPUT only *)
  | CommDestinationTable of integer * name with_loc list       (* OUTPUT only *)
  | CommMessageCount of name with_loc               (* INPUT [@@deriving show]*)
  | CommMessageDate of name with_loc       (* INPUT [@@deriving show]type I-O *)
  | CommMessageTime of name with_loc       (* INPUT type I-O [@@deriving show]*)
  | CommTextLength of name with_loc (* INPUT, OUTPUT [@@deriving show]type I-O *)
  | CommStatusKey of name with_loc (* INPUT,[@@deriving show] OUTPUT type I-O *)
  | CommEndKey of name with_loc    (* INPUT type I-O *)
  | CommErrorKey of name with_loc  (* OUTPUT only *)
[@@deriving show, ord]

and comm_channel =
  | CommQueue       (* INPUT only *)
  | CommSubQueue1   (* INPUT only *)
  | CommSubQueue2   (* INPUT only *)
  | CommSubQueue3   (* INPUT only *)
  | CommSource      (* INPUT only *)
  | CommTerminal    (* I-O only *)
  | CommDestination (* OUTPUT only *)
[@@deriving show, ord]
