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

(* ACCEPT, DISPLAY *)
type position =
  | LinePosition of ident_or_intlit
  | ColumnPosition of ident_or_intlit
  | LineColumnPosition of ident_or_intlit * ident_or_intlit
[@@deriving show, ord]

(* CALL, INVOKE *)

type call_using_clause =
  {
    call_using_by: call_using_by option;
    call_using_expr: expression option;(* [@compare
    fun a b -> Option.compare compare_expression a b] *)                (** OMITTED if [None] *)
  }
[@@deriving show, ord]

and call_using_by =
  | CallUsingByReference
  | CallUsingByContent
  | CallUsingByValue
[@@deriving show, ord]


(* DELETE, OPEN, REWRITE, WRITE, READ (through on_lock_or_retry) *)
type retry_clause =
  | RetryNTimes of expression (* arith *)
  | RetryForNSeconds of expression (* arith *)
  | RetryForever
[@@deriving show, ord]


(* ENABLE, DISABLE *)
(* MCS: Message Control System *)

type mcs_command_operands =
  {
    mcs_command_kind: mcs_kind;
    mcs_command_target: name with_loc;
    mcs_command_key: ident_or_alphanum option;
  }
[@@deriving show, ord]

and mcs_kind =
  | MCSInput
  | MCSOutput
  | MCSInputOutput
[@@deriving show, ord]


(* SEND, WRITE, PERFORM *)
type stage =
  | After
  | Before
[@@deriving show, ord]

(* REWRITE, WRITE *)
type write_target =
  | WriteTargetName of qualname
  | WriteTargetFile of name with_loc
[@@deriving show ,ord]

(*
ACCEPT id END_ACC (device or screen)
ACCEPT id FROM DATE/DAY... END_ACC (temporal)
ACCEPT id MESSAGE? COUNT END_ACC (message count?)
ACCEPT id AT ... ON EXCEPT ... END_ACC (screen)
*)

(* ACCEPT *)
type date_time =
  | Date of bool
  | Day of bool
  | DayOfWeek
  | Time
[@@deriving show, ord]


(* ADD, SUBTRACT *)
type basic_arithmetic_operands =
  | ArithSimple of
      {
        sources: ident_or_numlit list;
        targets: rounded_idents;
      }
  | ArithGiving of
      {
        sources: ident_or_numlit list;
        to_or_from_item: ident_or_numlit;
        targets: rounded_idents;
      }
  | ArithCorresponding of
      {
        source: qualname;
        target: rounded_ident;
      }
[@@deriving show, ord]



(*
DIVIDE id/lit INTO id_rounded+ ON_SIZE...? END_DIV

DIVIDE id/lit INTO id/lit GIVING... ON_SIZE...? END_DIV
DIVIDE id/lit INTO id/lit GIVING... REMAINDER... ON_SIZE...? END_DIV

DIVIDE id/lit BY id/lit GIVING... ON_SIZE...? END_DIV
DIVIDE id/lit BY id/lit GIVING... REMAINDER... ON_SIZE...? END_DIV
*)

(* DIVIDE *)
type divide_operands =
  | DivideInto of
      {
        divisor: ident_or_numlit;
        dividends: rounded_idents;                          (* non-empty *)
      }
  | DivideGiving of
      {
        divisor: ident_or_numlit;
        dividend: ident_or_numlit;
        giving: rounded_idents;
        into: bool;                         (* "INTO" if true, "by" otherwise *)
        remainder: ident option;
      }
[@@deriving show, ord]


(* EVALUATE *)
type selection_subject =
  | Subject of condition
  | SubjectConst of bool
[@@deriving show, ord]

type selection_object =
  | SelCond of condition (* ident / literal / expression *)
  | SelRange of
      {
        negated: bool;
        start: expression;
        stop: expression;
        alphabet: name with_loc option;
      }
  | SelRelation of
      {
        relation: relop;
        expr: expression;
      }
  | SelClassCond of
      {
        negated: bool;
        class_specifier: class_;
      }
  | SelSignCond of
      {
        negated: bool;
        sign_specifier: signz;
      }
  | SelOmitted of
      {
        negated: bool;
      }
  | SelConst of bool
  | SelAny
[@@deriving show, ord]


(* MULTIPLY *)
type multiply_operands =
  | MultiplyBy of
      {
        multiplier: ident_or_numlit;
        multiplicand: rounded_idents;
      }
  | MultiplyGiving of
      {
        multiplier: ident_or_numlit;
        multiplicand: ident_or_numlit;
        targets: rounded_idents;
      }
[@@deriving show, ord]


(* OPEN *)
type open_mode =
  | OpenInput
  | OpenOutput
  | OpenInputOutput
  | OpenExtend
[@@deriving show, ord]

type sharing_mode =
  | SharingAllOther
  | SharingNoOther
  | SharingReadOnly
[@@deriving show, ord]

type file_option =
  | FileOptReversed
  | FileOptWithNoRewind
[@@deriving show, ord]


(* RAISE *)
type raise_operand =
  | RaiseIdent of ident
  | RaiseException of name with_loc
[@@deriving show, ord]


(* EXIT & GO BACK *)
type raising =
  | RaisingIdent of ident                (* CHECKME: Can ident be a subscript?*)
  | RaisingException of name with_loc
  | RaisingLastException
[@@deriving show, ord]


(* READ *)
type read_direction =
  | ReadNext
  | ReadPrevious
[@@deriving show, ord]

type read_lock_behavior =
  | ReadAdvancingOnLock
  | ReadIgnoringLock
  | ReadRetry of retry_clause
[@@deriving show, ord]


(* RECEIVE *)
type mcs_awaiting_item =
  | MCSMessage
  | MCSSegment
[@@deriving show, ord]


(* SEARCH *)
type search_condition =
  | IsEqual of
      {
        data_item: qualident;
        condition: expression;
      }
  | Cond of qualident
[@@deriving show, ord]


(* SEND, WRITE *)
type advancing_phrase =
  | AdvancingLines of
      {
        stage: stage;
        lines: ident_or_intlit;
        ambiguous: bool;
      }
  | AdvancingPage of
      {
        stage: stage;
      }
[@@deriving show, ord]


(* SET *)
type set_attribute_switch =
  {
    set_attribute: screen_attribute;
    set_attribute_switch_value: on_off;
  }
[@@deriving show, ord]

and screen_attribute =
  | ScreenBell
  | ScreenBlink
  | ScreenHighlight
  | ScreenLowlight
  | ScreenReverseVideo
  | ScreenUnderline
[@@deriving show, ord]

and set_ambiguous_method =
  | SetMethodUp
  | SetMethodDown
  | SetMethodTo
[@@deriving show, ord]

and on_off =
  | On
  | Off
[@@deriving show, ord]

and locale_category =
  | LcAll
  | LcCollate
  | LcCtype
  | LcMessages
  | LcMonetary
  | LcNumeric
  | LcTime
[@@deriving show, ord]

and set_save_locale =
  | SetSaveLocaleLcAll
  | SetSaveLocaleUserDefault
[@@deriving show, ord]

and set_locale_target =
  | SetLocaleTarget of locale_category
  | SetLocaleTargetUserDefault
[@@deriving show, ord]

and set_locale_source =
  | SetLocaleSource of ident
  | SetLocaleSourceUserDefault
  | SetLocaleSourceSystemDefault
[@@deriving show, ord]

and float_content =
  | FarthestFromZero of bool
  | NearestToZero of bool
  | FloatInfinity
  | FloatNotANumber
  | FloatNotANumberSignaling
[@@deriving show, ord]


(* START *)
type start_position =
  | StartPositionFirst
  | StartPositionLast
  | StartPositionKey of
      {
        operator: relop;
        name: qualname;
        length: expression option;
      }
  (* any relop except IS NOT EQUAL TO or IS NOT = *)
[@@deriving show, ord]


(* STRING *)
type string_source =
  {
    string_source: ident_or_nonnum;
    string_delimiter: string_delimiter option;
  }
[@@deriving show, ord]

and string_delimiter =
  | StringDelimiter of ident_or_nonnum
  | StringDelimiterSize
[@@deriving show, ord]


(* UNSTRING *)
type unstring_delimiter =
  {
    unstring_delimiter: ident_or_strlit;
    unstring_delimiter_by_all: bool;
  }
[@@deriving show, ord]

type unstring_target =
  {
    unstring_target: ident;
    unstring_target_delimiter: ident option;
    unstring_target_count: ident option;
  }
[@@deriving show, ord]


(* --- generics --- *)

type 'a procedure_range =
  {
    procedure_start: 'a;
    procedure_end: 'a option;
  }
[@@deriving show, ord]
