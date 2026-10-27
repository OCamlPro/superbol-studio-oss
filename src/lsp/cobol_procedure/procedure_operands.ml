(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Typed operands: mirror of {!Cobol_ptree} operands. Operand types that never
    refer to data items, expressions, or conditions are not mirrored: their
    parse tree definition is used directly. *)

open Cobol_common.Srcloc.TYPES
open Procedure_terms

type rounded_ident = ident Cobol_ptree.rounded [@@deriving ord]

type rounded_idents = ident Cobol_ptree.rounded_list [@@deriving ord]

type position =
  | LinePosition of ident_or_intlit
  | ColumnPosition of ident_or_intlit
  | LineColumnPosition of ident_or_intlit * ident_or_intlit
  | CompoundPosition of ident_or_intlit (* MF *)
[@@deriving ord]

type using_arg_default =
  | ArgDefaultOmitted
  | ArgDefault of expr with_loc
[@@deriving ord]

type using_reference_arg =
  | ArgRefOmitted
  | ArgRef of scalar with_loc
[@@deriving ord]

type call_using_clause =
  | CallUsingDefault of using_arg_default with_loc Cobol_ptree.nel
  | CallUsingByReference of using_reference_arg with_loc Cobol_ptree.nel
  | CallUsingByContent of expr with_loc Cobol_ptree.nel
  | CallUsingByValue of expr with_loc Cobol_ptree.nel
[@@deriving ord]

type retry_clause =
  | RetryNTimes of expr with_loc
  | RetryForNSeconds of expr with_loc
  | RetryForever
[@@deriving ord]

type mcs_command_operands = {
  mcs_command_kind: Cobol_ptree.mcs_kind;
  mcs_command_target: Cobol_ptree.name with_loc;
  mcs_command_key: ident_or_alphanum option;
}
[@@deriving ord]

type write_target =
  | WriteTargetName of data_ref
  | WriteTargetFile of Cobol_ptree.name with_loc
[@@deriving ord]

type basic_arithmetic_operands =
  | ArithSimple of {
      operands: scalar list;
      targets: rounded_idents;
    }
  | ArithGiving of {
      leading_operands: scalar list;
      base_operand: scalar;
      targets: rounded_idents;
    }
  | ArithCorresponding of {
      source: data_ref;
      target: rounded_ident;
    }
[@@deriving ord]

type divide_operands =
  | DivideInto of {
      divisor: scalar;
      dividends: rounded_idents;
    }
  | DivideGiving of {
      divisor: scalar;
      dividend: scalar;
      giving: rounded_idents;
      into: bool; (* "INTO" if true, "BY" otherwise *)
      remainder: ident option;
    }
[@@deriving ord]

type selection_range = {
  sel_range_negated: bool;
  sel_range_start: expr with_loc;
  sel_range_stop: expr with_loc;
  sel_range_alphabet: Cobol_ptree.name with_loc option;
}
[@@deriving ord]

type selection_subject =
  | SubjectValue of expr with_loc
  | SubjectCond of condition
  | SubjectConst of bool
[@@deriving ord]

type selection_object =
  | SelCond of condition
  | SelValue of {
      negated: bool;
      value: expr with_loc;
    }
  | SelRange of selection_range
  | SelConst of bool
  | SelAny
[@@deriving ord]

type multiply_operands =
  | MultiplyBy of {
      multiplier: scalar;
      multiplicand: rounded_idents;
    }
  | MultiplyGiving of {
      multiplier: scalar;
      multiplicand: scalar;
      targets: rounded_idents;
    }
[@@deriving ord]

type raise_operand =
  | RaiseIdent of ident
  | RaiseException of Cobol_ptree.name with_loc
[@@deriving ord]

type raising =
  | RaisingIdent of ident
  | RaisingException of Cobol_ptree.name with_loc
  | RaisingLastException
[@@deriving ord]

type read_lock_behavior =
  | ReadAdvancingOnLock
  | ReadIgnoringLock
  | ReadRetry of retry_clause
[@@deriving ord]

type search_condition =
  | IsEqual of {
      data_item: qualident;
      condition: expr with_loc;
    }
  | Cond of qualident
[@@deriving ord]

type advancing_phrase =
  | AdvancingLines of {
      stage: Cobol_ptree.stage;
      lines: ident_or_intlit;
      ambiguous: bool;
    }
  | AdvancingPage of { stage: Cobol_ptree.stage }
[@@deriving ord]

type set_locale_source =
  | SetLocaleSource of ident
  | SetLocaleSourceUserDefault
  | SetLocaleSourceSystemDefault
[@@deriving ord]

type start_position =
  | StartPositionFirst
  | StartPositionLast
  | StartPositionKey of {
      operator: Cobol_ptree.relop;
      name: data_ref;
      length: expr with_loc option;
    }
[@@deriving ord]

type string_delimiter =
  | StringDelimiter of ident_or_nonnum
  | StringDelimiterSize
[@@deriving ord]

type string_source = {
  string_source: ident_or_nonnum;
  string_delimiter: string_delimiter option;
}
[@@deriving ord]

type unstring_delimiter = {
  unstring_delimiter: ident_or_strlit;
  unstring_delimiter_by_all: bool;
}
[@@deriving ord]

type unstring_target = {
  unstring_target: ident;
  unstring_target_delimiter: ident option;
  unstring_target_count: ident option;
}
[@@deriving ord]
