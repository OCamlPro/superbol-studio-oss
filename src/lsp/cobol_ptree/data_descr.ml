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

open Common
open Numericals
open Terms

(* ----------------------------- DATA DIVISION ----------------------------- *)
type format_clause =
 | Bit
 | Character
 | Numeric
[@@deriving ord]

let pp_format_clause ppf = function
  | Bit -> Fmt.pf ppf "BIT"
  | Character -> Fmt.pf ppf "CHARACTER"
  | Numeric -> Fmt.pf ppf "NUMERIC"


type file_block_contents =
  | FileBlockContainsCharacters
  | FileBlockContainsRecords
[@@deriving ord]

let pp_file_block_contents ppf = function
  | FileBlockContainsCharacters -> Fmt.pf ppf "CHARACTERS"
  | FileBlockContainsRecords -> Fmt.pf ppf "RECORDS"


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
[@@deriving ord]

let pp_depending_phrase ppf qn =
  Fmt.pf ppf "DEPENDING %a" (pp_with_loc pp_qualname) qn

let pp_record_clause ppf = function
  | FixedLength n -> Fmt.pf ppf "RECORD %a" pp_integer n
  | VariableLength { min_length; max_length; depending } ->
    Fmt.pf ppf "RECORD VARYING%a%a%a"
      Fmt.(option (any " " ++ pp_integer)) min_length
      Fmt.(option (any " TO " ++ pp_integer)) max_length
      Fmt.(option (any " " ++ pp_depending_phrase)) depending
  | FixedOrVariableLength { min_length; max_length } ->
    Fmt.pf ppf "RECORD %a TO %a" pp_integer min_length pp_integer max_length

type label_clause =
  | LabelStandard
  | LabelOmitted
[@@deriving ord]

let pp_label_clause ppf lc =
  Fmt.pf ppf "LABEL RECORD IS ";
  match lc with
  | LabelStandard -> Fmt.pf ppf "STANDARD"
  | LabelOmitted -> Fmt.pf ppf "OMITTED"

type file_data_clause =
  name with_loc list
[@@deriving ord]

let pp_file_data_clause ppf ns =
  Fmt.pf ppf "DATA RECORD IS@ %a" Fmt.(list ~sep:sp pp_name') ns

type file_linage_clause =                                  (* file descr only *)
  {
    file_linage_lines: qualname_or_intlit;
    file_linage_with_footing_at: qualname_or_intlit option;
    file_linage_lines_at_top: qualname_or_intlit option;
    file_linage_lines_at_bottom: qualname_or_intlit option;
  }
[@@deriving ord]

let pp_file_linage_clause ppf
  { file_linage_lines = fll; file_linage_with_footing_at = wfa;
    file_linage_lines_at_top = top; file_linage_lines_at_bottom = bot }
=
  Fmt.pf ppf "LINAGE %a LINES" pp_qualname_or_intlit fll;
  Fmt.(option (any "@ FOOTING " ++ pp_qualname_or_intlit)) ppf wfa;
  Fmt.(option (any "@ TOP " ++ pp_qualname_or_intlit)) ppf top;
  Fmt.(option (any "@ BOTTOM " ++ pp_qualname_or_intlit)) ppf bot;

type data_level = int
[@@deriving ord]

let pp_data_level = Fmt.fmt "%02d"

type rename_item =
  {
    rename_level: data_level with_loc;
    rename_to: name with_loc;
    rename_renamed: qualname with_loc;
    rename_through: qualname with_loc option;
  }
[@@deriving ord]

let pp_rename_item ppf
  { rename_level = rl; rename_to = rto;
    rename_renamed = rr; rename_through = rt }
=
Fmt.pf ppf "%a %a@;<1 2>RENAMES %a%a."
  (pp_with_loc pp_data_level) rl
  pp_name' rto
  pp_qualname' rr
  Fmt.(option (any "@;<1 2>THROUGH " ++ pp_qualname')) rt


type condition_name_item =
  {
    condition_name_level: data_level with_loc; (* is always 88 *)
    condition_name: name with_loc;
    condition_name_values: condition_name_value list;
    condition_name_alphabet: name with_loc option;
    condition_name_when_false: literal option;
  }
[@@deriving ord]

and condition_name_value =
  {
    condition_name_value: literal;
    condition_name_through: literal option;
  }
[@@deriving ord]

let pp_condition_name_value ppf
  { condition_name_value = cnv; condition_name_through = cnt }
=
  match cnt with
  | Some cnt -> Fmt.pf ppf "%a THROUGH %a" pp_literal cnv pp_literal cnt
  | None -> pp_literal ppf cnv

let pp_condition_name_item ppf
  { condition_name_level = cnl; condition_name = cn;
    condition_name_values = cnvl; condition_name_alphabet = cna;
    condition_name_when_false = cnwf }
=
  Fmt.pf ppf "%a %a@ VALUE %a%a%a."
    (pp_with_loc pp_data_level) cnl
    pp_name' cn
    Fmt.(list ~sep:comma pp_condition_name_value) cnvl
    Fmt.(option (any "@ IN " ++ pp_name')) cna
    Fmt.(option (any "@ WHEN FALSE " ++ pp_literal)) cnwf


type data_name =
  | DataName of name with_loc
  | DataFiller
[@@deriving ord]

let pp_data_name ppf = function
  | DataName n -> pp_name' ppf n
  | DataFiller -> Fmt.pf ppf "FILLER"

(* Format seen on IBM: PICTURE ... SIZE ... LOCALE ... *)
type locale_phrase =
  {
    locale_name: name with_loc option;
    locale_size: integer;
  }
[@@deriving ord]

let pp_locale_phrase ppf { locale_name; locale_size } =
  Fmt.pf ppf "LOCALE%a SIZE %a"
    Fmt.(option (any " " ++ pp_name')) locale_name
    pp_integer locale_size

type sign_clause =
  {
    sign_position: sign_position;
    sign_separate_character: bool;
  }
[@@deriving ord]

and sign_position = LeadingSign | TrailingSign

let pp_sign_position ppf = function
  | LeadingSign -> Fmt.pf ppf "LEADING"
  | TrailingSign -> Fmt.pf ppf "TRAILING"

let pp_sign_clause ppf { sign_position; sign_separate_character } =
  Fmt.pf ppf "SIGN IS %a" pp_sign_position sign_position;
  if sign_separate_character then Fmt.pf ppf " SEPARATE"

type report_screen_usage_clause =
  | Display
  | National
[@@deriving ord]

let pp_report_screen_usage_clause ppf = function
  | Display -> Fmt.pf ppf "USAGE DISPLAY"
  | National -> Fmt.pf ppf "USAGE NATIONAL"

type external_clause =
  strlit option
[@@deriving ord]

let pp_external_clause : external_clause Fmt.t =
  Fmt.(const string "EXTERNAL" ++ option (any "@ AS@ " ++ pp_strlit))

type group_usage_clause =
  | GroupUsageBit
  | GroupUsageNational
[@@deriving ord]

let pp_group_usage_clause ppf = function
  | GroupUsageBit -> Fmt.pf ppf "GROUP-USAGE BIT"
  | GroupUsageNational -> Fmt.pf ppf "GROUP-USAGE NATIONAL"

type data_occurs_clause =
  | OccursFixed of
      {
        times: integer with_loc;
        key_is: sort_spec list;
        indexed_by: name with_loc list;
      }
  | OccursDepending of
      {
        from: integer with_loc;
        to_: integer with_loc;
        depending: qualname with_loc;
        key_is: sort_spec list;
        indexed_by: name with_loc list;
      }
  | OccursDynamic of
      {
        capacity_in: name with_loc option;
        from: integer with_loc option;
        to_: integer with_loc option;
        initialized: bool with_loc;
        key_is: sort_spec list;
        indexed_by: name with_loc list;
      }
[@@deriving ord]

and sort_spec =
  {
    sort_key_direction: sort_direction;
    sort_key_names: qualname list;
  }

and sort_direction =
  | SortAscending
  | SortDescending

let pp_sort_direction ppf = function
  | SortAscending -> Fmt.pf ppf "ASCENDING"
  | SortDescending -> Fmt.pf ppf "DESCENDING"

let pp_sort_spec ppf { sort_key_direction = skd; sort_key_names = skns } =
  Fmt.pf ppf "%a %a" pp_sort_direction skd
    Fmt.(list ~sep:(any " ") pp_qualname) skns

let pp_indexed_by = Fmt.(any "INDEXED " ++ list ~sep:(any " ") pp_name')

let pp_indexed_by_opt ppf = function
  | [] -> ()
  | iby -> Fmt.sp ppf (); pp_indexed_by ppf iby

let pp_integer' = pp_with_loc pp_integer

let pp_data_occurs_clause ppf = function
  | OccursFixed { times; key_is; indexed_by } ->
    Fmt.pf ppf "OCCURS %a%a%a"
      pp_integer' times
      Fmt.(list ~sep:nop (sp ++ pp_sort_spec)) key_is
      pp_indexed_by_opt indexed_by
  | OccursDepending { from; to_; depending; key_is; indexed_by } ->
    Fmt.pf ppf "OCCURS %a TO %a %a %a%a"
      pp_integer' from pp_integer' to_
      pp_depending_phrase depending
      Fmt.(list ~sep:nop (sp ++ pp_sort_spec)) key_is
      pp_indexed_by_opt indexed_by
  | OccursDynamic { capacity_in; from; to_; initialized; key_is; indexed_by } ->
    Fmt.pf ppf "OCCURS DYNAMIC%a%a%a%a%a%a"
      Fmt.(option (any " CAPACITY " ++ pp_name')) capacity_in
      Fmt.(option (any " FROM " ++ pp_integer')) from
      Fmt.(option (any " TO " ++ pp_integer')) to_
      Fmt.(if initialized.payload then any " INITIALIZED" else nop) ()
      Fmt.(list ~sep:nop (sp ++ pp_sort_spec)) key_is
      pp_indexed_by_opt indexed_by

type data_varying =
  {
    data_varying: name with_loc;
    data_varying_from: expression option;
    data_varying_by: expression option;
  }
[@@deriving ord]

let pp_data_varying ppf
  { data_varying = v; data_varying_from = f; data_varying_by = b}
=
  Fmt.pf ppf "%a%a%a"
    pp_name' v
    Fmt.(option (any "@ FROM " ++ pp_expression)) f
    Fmt.(option (any "@ BY " ++ pp_expression)) b

let pp_varying_clause ppf vcs =
  Fmt.pf ppf "VARYING %a"
    Fmt.(list ~sep:sp pp_data_varying) vcs

type select_when_clause =
  | SelectWhen of name with_loc
  | SelectWhenOther
[@@deriving ord]

let pp_select_when_clause ppf = function
  | SelectWhen n -> Fmt.pf ppf "SELECT WHEN %a" pp_name' n
  | SelectWhenOther -> Fmt.pf ppf "SELECT WHEN OTHER"

type synchronized_clause =
  | SynchronizedLeft
  | SynchronizedRight
  | SynchronizedDefault
[@@deriving ord]

let pp_synchronized_clause ppf = function
  | SynchronizedLeft -> Fmt.pf ppf "SYNCHRONIZED LEFT"
  | SynchronizedRight -> Fmt.pf ppf "SYNCHRONIZED RIGHT"
  | SynchronizedDefault -> Fmt.pf ppf "SYNCHRONIZED"


type property_clause =
  {
    property_with_no: property_kind option;
    property_is_final: bool;
  }
[@@deriving ord]

and property_kind =
  | PropertyGet
  | PropertySet
[@@deriving ord]

let pp_property_kind ppf = function
  | PropertyGet -> Fmt.pf ppf "GET"
  | PropertySet -> Fmt.pf ppf "SET"

let pp_property_clause ppf { property_with_no; property_is_final } =
  Fmt.pf ppf "PROPERTY%a"
    Fmt.(option (any "@ WITH NO " ++ pp_property_kind)) property_with_no;
  if property_is_final then Fmt.pf ppf "@ FINAL"

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
 | UsagePending of [`Comp0 | `Comp1 | `Comp2 | `Comp3 | `Comp5 | `Comp6 |
                    `CompX | `CompN | `Comp9 | `Comp10 | `Comp15 |
                    `BinaryCLong of signedness option]
[@@deriving ord]

and signedness =
 | Signed
 | Unsigned

and endianness_mode =
 | HighOrderLeft
 | HighOrderRight

and encoding_mode =
 | BinaryEncoding
 | DecimalEncoding

and encoding_endianness =
  {
    encoding_mode: encoding_mode option;
    encoding_endianness: endianness_mode option;
  }

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

let pp_signedness ppf = function
  | Signed -> Fmt.pf ppf "SIGNED"
  | Unsigned -> Fmt.pf ppf "UNSIGNED"

let pp_endianness_mode ppf = function
  | HighOrderLeft -> Fmt.pf ppf "HIGH-ORDER-LEFT"
  | HighOrderRight -> Fmt.pf ppf "HIGH-ORDER-RIGHT"

let pp_encoding_mode ppf = function
  | BinaryEncoding -> Fmt.pf ppf "BINARY-ENCODING"
  | DecimalEncoding -> Fmt.pf ppf "DECIMAL-ENCODING"

let pp_encoding_endianness ppf { encoding_mode; encoding_endianness } =
  match encoding_mode, encoding_endianness with
  | None, None -> ()
  | Some em, None -> pp_encoding_mode ppf em
  | None, Some ee -> pp_endianness_mode ppf ee
  | Some em, Some ee ->
    Fmt.pf ppf "%a@ %a" pp_encoding_mode em pp_endianness_mode ee

let pp_object_reference_kind ppf = function
  | ActiveClass { factory_of } ->
    if factory_of then Fmt.pf ppf "FACTORY OF ";
    Fmt.pf ppf "ACTIVE-CLASS"
  | Name { class_or_interface_name = n; factory_of; only } ->
    if factory_of then Fmt.pf ppf "FACTORY OF ";
    pp_name' ppf n;
    if only then Fmt.pf ppf " ONLY"

let pp_usage_clause ppf = function
  | Binary -> Fmt.pf ppf "BINARY"
  | BinaryChar so ->
      Fmt.pf ppf "BINARY-CHAR%a"
        Fmt.(option (sp ++ pp_signedness)) so
  | BinaryShort so ->
      Fmt.pf ppf "BINARY-SHORT%a"
        Fmt.(option (sp ++ pp_signedness)) so
  | BinaryLong so ->
      Fmt.pf ppf "BINARY-LONG%a"
        Fmt.(option (sp ++ pp_signedness)) so
  | BinaryDouble so ->
      Fmt.pf ppf "BINARY-DOUBLE%a"
        Fmt.(option (sp ++ pp_signedness)) so
  | Bit -> Fmt.pf ppf "BIT"
  | Display -> Fmt.pf ppf "DISPLAY"
  | FloatBinary32 emo ->
      Fmt.pf ppf "FLOAT-BINARY-32%a"
        Fmt.(option (sp ++ pp_endianness_mode)) emo
  | FloatBinary64 emo ->
      Fmt.pf ppf "FLOAT-BINARY-64%a"
        Fmt.(option (sp ++ pp_endianness_mode)) emo
  | FloatBinary128 emo ->
      Fmt.pf ppf "FLOAT-BINARY-128%a"
        Fmt.(option (sp ++ pp_endianness_mode)) emo
  | FloatDecimal16 ee ->
      Fmt.pf ppf "FLOAT-DECIMAL-16 %a" pp_encoding_endianness ee
  | FloatDecimal34 ee ->
      Fmt.pf ppf "FLOAT-DECIMAL-34 %a" pp_encoding_endianness ee
  | FloatExtended -> Fmt.pf ppf "FLOAT-EXTENDED"
  | FloatLong -> Fmt.pf ppf "FLOAT-LONG"
  | FloatShort -> Fmt.pf ppf "FLOAT-SHORT"
  | Index -> Fmt.pf ppf "INDEX"
  | National -> Fmt.pf ppf "NATIONAL"
  | ObjectReference orko ->
      Fmt.pf ppf "OBJECT REFERENCE%a"
        Fmt.(option (sp ++ pp_object_reference_kind)) orko
  | PackedDecimal -> Fmt.pf ppf "PACKED-DECIMAL"
  | Pointer no ->
      Fmt.pf ppf "POINTER%a" Fmt.(option (any " TO " ++ pp_name')) no
  | FunctionPointer n -> Fmt.pf ppf "FUNCTION-POINTER TO %a" pp_name' n
  | ProgramPointer no ->
      Fmt.pf ppf "PROGRAM-POINTER%a" Fmt.(option (any " TO " ++ pp_name')) no
  | UsagePending comp ->
      match comp with
      | `Comp0 -> Fmt.pf ppf "COMP-0"
      | `Comp1 -> Fmt.pf ppf "COMP-1"
      | `Comp2 -> Fmt.pf ppf "COMP-2"
      | `Comp3 -> Fmt.pf ppf "COMP-3"
      | `Comp5 -> Fmt.pf ppf "COMP-5"
      | `Comp6 -> Fmt.pf ppf "COMP-6"
      | `CompX -> Fmt.pf ppf "COMP-X"
      | `CompN -> Fmt.pf ppf "COMP-N"
      | `Comp9 -> Fmt.pf ppf "COMP-9"
      | `Comp10-> Fmt.pf ppf "COMP-10"
      | `Comp15 -> Fmt.pf ppf "COMP-15"
      | `BinaryCLong so ->
          Fmt.pf ppf "BINARY-C-LONG%a" Fmt.(option (sp ++ pp_signedness)) so

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
[@@deriving ord]

and class_clause =
 | Alphabetic
 | AlphabeticLower
 | AlphabeticUpper
 | Boolean
 | Numeric
 | ClassOrAlphabet of name with_loc

and validate_when =
  | ValidateWhenError
  | ValidateWhenNoError

and validation_stage =
  | ValidationStageFormat
  | ValidationStageContent
  | ValidationStageRelation

let pp_class_clause ppf = function
  | Alphabetic -> Fmt.pf ppf "ALPHABETIC"
  | AlphabeticLower -> Fmt.pf ppf "ALPHABETIC-LOWER"
  | AlphabeticUpper -> Fmt.pf ppf "ALPHABETIC-UPPER"
  | Boolean -> Fmt.pf ppf "BOOLEAN"
  | Numeric -> Fmt.pf ppf "NUMERIC"
  | ClassOrAlphabet n -> pp_name' ppf n

let pp_validate_when ppf = function
  | ValidateWhenError -> Fmt.pf ppf "ERROR"
  | ValidateWhenNoError -> Fmt.pf ppf "NO ERROR"

let pp_validation_stage ppf = function
  | ValidationStageFormat -> Fmt.pf ppf "FORMAT"
  | ValidationStageContent -> Fmt.pf ppf "CONTENT"
  | ValidationStageRelation -> Fmt.pf ppf "RELATION"

let pp_default_clause =
  Fmt.(any "DEFAULT " ++ option ~none:(any "NONE") pp_ident_or_literal)

let pp_destination_clause =
  Fmt.(any "DESTINATION " ++ list ~sep:sp pp_ident)

let pp_invalid_when_clause =
  Fmt.(list ~sep:sp (any "INVALID WHEN " ++ pp_condition))

let pp_present_when_clause =
  Fmt.(any "PRESENT WHEN " ++ pp_condition)

let pp_validation_clause ppf = function
  | Class cc -> pp_class_clause ppf cc
  | Default iolo -> pp_default_clause ppf iolo
  | Destination ns -> pp_destination_clause ppf ns
  | InvalidWhen cs -> pp_invalid_when_clause ppf cs
  | PresentWhen c -> pp_present_when_clause ppf c
  | Varying dvs -> pp_varying_clause ppf dvs
  | ValidateStatus { is_; when_; on; for_ } ->
    Fmt.pf ppf "VALIDATE-STATUS %a %a%a FOR %a"
      pp_ident_or_literal is_
      pp_validate_when when_
      Fmt.(
        if on == [] then nop else list ~sep:nop (sp ++ pp_validation_stage)
      ) on
      Fmt.(list ~sep:sp pp_ident) for_

type data_value_clause =
  | ValueData of literal with_loc
  | ValueTable of table_data_value list

and table_data_value =
  {
    table_data_values: literal with_loc list;                    (* non-empty *)
    table_data_from: subscript list;                             (* non-empty *)
    table_data_to: subscript list;
  }
[@@deriving ord]

let pp_table_data_value ppf
    { table_data_values = tdv; table_data_from = tdf; table_data_to = tdt } =
  Fmt.pf ppf "%a@ FROM@ %a%a"
    Fmt.(list ~sep:sp pp_literal') tdv
    Fmt.(list ~sep:sp pp_subscript) tdf
    Fmt.(if tdt == []
         then nop
         else any "@ TO@ " ++ list ~sep:sp pp_subscript) tdt

let pp_data_value_clause ppf = function
  | ValueData lit ->
      Fmt.pf ppf "VALUE %a" pp_literal' lit
  | ValueTable tdv ->
      Fmt.pf ppf "VALUES %a" Fmt.(list ~sep:sp pp_table_data_value) tdv

type report_data_name_or_final =
  | ReportDataName of qualident
  | ReportFinal
[@@deriving ord]

let pp_report_data_name_or_final ppf = function
  | ReportDataName qi -> pp_qualident ppf qi
  | ReportFinal -> Fmt.pf ppf "FINAL"

type report_type_clause =
  | Detail
  | ReportHeading
  | ReportFooting
  | PageHeading
  | PageFooting
  | ControlHeading of (report_data_name_or_final * bool) option
  | ControlFooting of report_data_name_or_final option
[@@deriving ord]

let pp_report_type_clause ppf = function
  | Detail -> Fmt.pf ppf "TYPE DETAIL"
  | ReportHeading -> Fmt.pf ppf "TYPE RH"
  | ReportFooting -> Fmt.pf ppf "TYPE RF"
  | PageHeading -> Fmt.pf ppf "TYPE PH"
  | PageFooting -> Fmt.pf ppf "TYPE PF"
  | ControlHeading o ->
    Fmt.pf ppf "TYPE CH%a"
      Fmt.(option (
        any " FOR " ++ using fst pp_report_data_name_or_final ++
        using snd (fun ppf b -> if b then Fmt.pf ppf " OR PAGE" else ())
      )) o
  | ControlFooting o ->
    Fmt.pf ppf "TYPE CF%a"
    Fmt.(option (any " FOR " ++ pp_report_data_name_or_final)) o

type next_group_clause =
  | ReportNextAbsolute of integer
  | ReportNextRelative of integer
  | ReportNextNextPage of bool
[@@deriving ord]

let pp_next_group_clause ppf = function
  | ReportNextAbsolute i -> Fmt.pf ppf "NEXT GROUP IS %a" pp_integer i
  | ReportNextRelative i -> Fmt.pf ppf "NEXT GROUP IS PLUS %a" pp_integer i
  | ReportNextNextPage wr ->
    Fmt.pf ppf "NEXT GROUP IS NEXT PAGE";
    if wr then Fmt.pf ppf " WITH RESET"

type column_position =
  | ColumnAbsolute of integer
  | ColumnRelative of integer
[@@deriving ord]

let pp_column_position ppf = function
  | ColumnAbsolute i -> pp_integer ppf i
  | ColumnRelative i -> Fmt.pf ppf "PLUS %a" pp_integer i

type alignment =
  | AlignLeft
  | AlignRight
  | AlignCenter
[@@deriving ord]

let pp_alignment ppf = function
  | AlignLeft -> Fmt.pf ppf "LEFT"
  | AlignRight -> Fmt.pf ppf "RIGHT"
  | AlignCenter -> Fmt.pf ppf "CENTER"

type line_position =
  | LineAbsolute of integer * bool
  | LineRelative of integer
  | LineOnNextPage
[@@deriving ord]

let pp_line_position ppf = function
  | LineAbsolute (i, true) -> Fmt.pf ppf "%a NEXT PAGE" pp_integer i
  | LineAbsolute (i, false) -> pp_integer ppf i
  | LineRelative i -> Fmt.pf ppf "PLUS %a" pp_integer i
  | LineOnNextPage -> Fmt.pf ppf "NEXT PAGE"

type sum_phrase =
  {
    sum_operands: expression list; (* non-empty *)
    sum_upon_items: name with_loc list;
  }
[@@deriving ord]

let pp_sum_phrase ppf { sum_operands = ops; sum_upon_items = sui } =
  Fmt.pf ppf "SUM@;<1 2>";
  Fmt.(box (list ~sep:sp pp_expression)) ppf ops;
  if sui != [] then
    Fmt.pf ppf "@ %a"
      Fmt.(box (any "UPON" ++ list ~sep:nop (sp ++ pp_name'))) sui

type polarity =
  | Plus
  | Minus
[@@deriving ord]

let pp_polarity ppf = function
  | Plus -> Fmt.pf ppf "+"
  | Minus -> Fmt.pf ppf "-"

type blank_clause =
  | Line
  | Screen
[@@deriving ord]

let pp_blank_clause ppf = function
  | Line -> Fmt.pf ppf "BLANK LINE"
  | Screen -> Fmt.pf ppf "BLANK SCREEN"

type erase_clause =
  | EndOfLine
  | EndOfScreen
[@@deriving ord]

let pp_erase_clause ppf = function
  | EndOfLine -> Fmt.pf ppf "ERASE EOL"
  | EndOfScreen -> Fmt.pf ppf "ERASE EOS"

type screen_attribute_clause =
  | Bell
  | Blink
  | Highlight
  | Lowlight
  | ReverseVideo
  | Underline
  | ForegroundColor of ident_or_intlit
  | BackgroundColor of ident_or_intlit
[@@deriving ord]

let pp_screen_attribute_clause ppf = function
  | Bell -> Fmt.pf ppf "BELL"
  | Blink -> Fmt.pf ppf "BLINK"
  | Highlight -> Fmt.pf ppf "HIGHLIGHT"
  | Lowlight -> Fmt.pf ppf "LOWLIGHT"
  | ReverseVideo -> Fmt.pf ppf "REVERSE-VIDEO"
  | Underline -> Fmt.pf ppf "UNDERLINE"
  | ForegroundColor c -> Fmt.pf ppf "FOREGROUND-COLOR %a" pp_ident_or_intlit c
  | BackgroundColor c -> Fmt.pf ppf "BACKGROUND-COLOR %a" pp_ident_or_intlit c

type screen_line_column_clause =
  | Absolute of ident_or_intlit
  | Relative of polarity * ident_or_intlit
[@@deriving ord]

let pp_screen_line_column_clause ppf = function
  | Absolute ii -> pp_ident_or_intlit ppf ii
  | Relative (pm, ii) ->
    pp_polarity ppf pm; pp_ident_or_intlit ppf ii

type source_destination_clause =
  | From of ident_or_literal
  | To of ident
  | Using of ident
  | Value of literal
[@@deriving ord]

let pp_source_destination_clause ppf = function
  | From iol -> Fmt.pf ppf "FROM %a" pp_ident_or_literal iol
  | To i -> Fmt.pf ppf "TO %a" pp_ident i
  | Using i -> Fmt.pf ppf "USING %a" pp_ident i
  | Value l -> Fmt.pf ppf "VALUE %a" pp_literal l

type value_of_clause =
  {
    value_of_valued: name with_loc;
    value_of_value: qualname_or_literal;
  }
[@@deriving ord]

let pp_value_of_clause ppf { value_of_valued; value_of_value } =
  Fmt.(
    pair ~sep:sp pp_name' pp_qualname_or_literal ppf
      (value_of_valued, value_of_value)
  )

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
[@@deriving ord]

let pp_report_clause ppf = function
  | Global -> Fmt.pf ppf "GLOBAL"
  | Code i -> Fmt.pf ppf "CODE %a" pp_ident i
  | Control { final; controls } ->
    Fmt.pf ppf "CONTROL ";
    if final then Fmt.pf ppf "FINAL ";
    Fmt.(list ~sep:sp pp_name') ppf controls
  | PageLimit { lines; columns; heading; first_detail; last_control_heading; last_detail; footing } ->
    Fmt.pf ppf "PAGE LIMIT IS%a%a%a%a%a%a%a"
      Fmt.(option (any " " ++ pp_integer ++ any " LINES")) lines
      Fmt.(option (any " " ++ pp_integer ++ any " COLUMNS")) columns
      Fmt.(option (any "@ HEADING " ++ pp_integer)) heading
      Fmt.(option (any "@ FIRST DETAIL " ++ pp_integer)) first_detail
      Fmt.(option (any "@ LAST CH " ++ pp_integer)) last_control_heading
      Fmt.(option (any "@ LAST DETAIL " ++ pp_integer)) last_detail
      Fmt.(option (any "@ FOOTING " ++ pp_integer)) footing


type constant_item =
  {
    constant_level: data_level with_loc;      (* is a constant *)     (* TODO: check \in {"1", "01"} *)
    constant_name: name with_loc;
    constant_global: bool;
    constant_value: constant_value with_loc;
  }
[@@deriving ord]

and constant_value =
  | ConstExpr of expression                                 (* or plain ident *)
  | ConstByteLength of name with_loc
  | ConstLength of name with_loc
  | ConstFrom of name with_loc                        (* compilation variable *)
[@@deriving ord]

let pp_constant_value ppf = function
  | ConstExpr e -> Fmt.pf ppf "AS@ %a" pp_expression e
  | ConstByteLength n -> Fmt.pf ppf "AS BYTE LENGTH %a" pp_name' n
  | ConstLength n -> Fmt.pf ppf "AS LENGTH %a" pp_name' n
  | ConstFrom n -> Fmt.pf ppf "FROM %a" pp_name' n

let pp_constant_item ppf {
  constant_level; constant_name; constant_global; constant_value
} =
  Fmt.pf ppf "%a%a@ CONSTANT@ %a@ %a."
    (pp_with_loc pp_data_level) constant_level
    (pp_with_loc pp_name) constant_name
    Fmt.(if constant_global then any "@ IS GLOBAL" else nop) ()
    (pp_with_loc pp_constant_value) constant_value

(* --- COMMUNICATION SECTION --- *)

type comm_clause =
  | CommSymbolic of comm_channel * name with_loc
  | CommDestinationCount of name with_loc                      (* OUTPUT only *)
  | CommDestinationTable of integer * name with_loc list       (* OUTPUT only *)
  | CommMessageCount of name with_loc               (* INPUT *)
  | CommMessageDate of name with_loc       (* INPUT type I-O *)
  | CommMessageTime of name with_loc       (* INPUT type I-O *)
  | CommTextLength of name with_loc (* INPUT, OUTPUT type I-O *)
  | CommStatusKey of name with_loc (* INPUT, OUTPUT type I-O *)
  | CommEndKey of name with_loc    (* INPUT type I-O *)
  | CommErrorKey of name with_loc  (* OUTPUT only *)
[@@deriving ord]

and comm_channel =
  | CommQueue       (* INPUT only *)
  | CommSubQueue1   (* INPUT only *)
  | CommSubQueue2   (* INPUT only *)
  | CommSubQueue3   (* INPUT only *)
  | CommSource      (* INPUT only *)
  | CommTerminal    (* I-O only *)
  | CommDestination (* OUTPUT only *)
[@@deriving ord]

let pp_comm_channel ppf = function
  | CommQueue -> Fmt.pf ppf "QUEUE"
  | CommSubQueue1 -> Fmt.pf ppf "SUB-QUEUE-1"
  | CommSubQueue2 -> Fmt.pf ppf "SUB-QUEUE-2"
  | CommSubQueue3 -> Fmt.pf ppf "SUB-QUEUE-3"
  | CommSource -> Fmt.pf ppf "SOURCE"
  | CommTerminal -> Fmt.pf ppf "TERMINAL"
  | CommDestination -> Fmt.pf ppf "DESTINATION"

let pp_comm_clause ppf = function
  | CommSymbolic (cc, n) -> Fmt.pf ppf "%a IS %a" pp_comm_channel cc pp_name' n
  | CommDestinationCount n -> Fmt.pf ppf "DESTINATION COUNT IS %a" pp_name' n
  | CommDestinationTable (i, il) ->
    Fmt.pf ppf "DESTINATION TABLE OCCURS %a TIMES %a"
      pp_integer i Fmt.(list ~sep:sp (any "INDEXED BY " ++ pp_name')) il
  | CommMessageCount n -> Fmt.pf ppf "COUNT IS %a" pp_name' n
  | CommMessageDate n -> Fmt.pf ppf "MESSAGE DATE IS %a" pp_name' n
  | CommMessageTime n -> Fmt.pf ppf "MESSAGE TIME IS %a" pp_name' n
  | CommTextLength n -> Fmt.pf ppf "TEXT LENGTH IS %a" pp_name' n
  | CommStatusKey n -> Fmt.pf ppf "STATUS KEY IS %a" pp_name' n
  | CommEndKey n -> Fmt.pf ppf "END KEY IS %a" pp_name' n
  | CommErrorKey n -> Fmt.pf ppf "ERROR KEY IS %a" pp_name' n
