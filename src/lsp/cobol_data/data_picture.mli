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

open Cobol_common.Srcloc.TYPES

(* --- *)

module TYPES: sig
  type symbol =
    | A
    | B
    | CR
    | CS
    | DB
    | DecimalSep
    | E
    | GroupingSep
    | L
    | Minus
    | N
    | Nine
    | One
    | P
    | Plus
    | S
    | Slant
    | Star
    | V
    | X
    | Z
    | Zero

  type symbols =
    {
      symbol: symbol;
      symbol_occurences: int;
    }

  type category =
    | Alphabetic of
        {
          length: int;
        }
    | Alphanumeric of
        {
          length: int;
          insertions: simple_insertion list;
        }
    | Boolean of
        {
          length: int;
        }
    | National of
        {
          length: int;
          insertions: simple_insertion list;
        }
    | FixedNum of
        {
          digits: int;
          scale: int;
          with_sign: bool;
          editions: editions;
        }
    | FloatNum of
        {
          digits: int;
          scale: int;
          with_sign: bool;
          exponent_digits: int;
          editions: basic_edition list;
        }

  and editions =
    {
      basics: basic_edition list;
      floating: floating_insertion option;
      zerorepl: zero_replacement option;
    }

  and basic_edition =
    | SimpleInsertion of simple_insertion
    | SpecialInsertion of special_insertion
    | FixedInsertion of fixed_insertion

  and simple_insertion =
    {
      simple_insertion_symbols: symbols;
      simple_insertion_offset: int;
    }

  and special_insertion =
    {
      special_insertion_offset: int;
      special_insertion_length: int;
    }

  and fixed_insertion =
    {
      fixed_insertion_symbol: symbol;
      fixed_insertion_offset: int;
    }

  and floating_insertion =
    {
      floating_insertion_symbol: symbol;
      floating_insertion_ranges: floating_range list;
    }

  and zero_replacement =
    {
      zero_replacement_symbol: symbol;
      zero_replacement_ranges: floating_range list;
    }

  and floating_range =
    {
      floating_range_offset: int;
      floating_range_length: int;
    }

  type picture =
    {
      category: category;
      pic: symbols list;
    }
  [@@deriving show, ord]

  type config = {
    max_pic_length : int;
    decimal_char: char;
    currency_signs: Cobol_common.Basics.CharSet.t;
  }

  type error =
    | May_only_appear_once of { symbol_precedence: int;
                                decimal_char: char }
    | May_not_follow of { symbol_precedence: int;
                          prev_precedence: int;
                          decimal_char: char }
    | Parenthesis_must_be_preceded_by_picture_symbol
    | Unexpected_char of char
    | Extraneous_symbol_in_exponent
    | Symbol_may_only_appear_once of symbol
    | Symbol_must_be_at_start of symbol
    | Symbol_must_be_at_end of symbol
    | Symbol_must_be_at_start_or_end of symbol
    | Symbols_are_mutually_exclusive of symbol * symbol
    | Unexpected_symbol of symbol * category option
    | Empty_picture_string
    | Picture_length_exceeds_limit of (* length *) int * (* max_len *) int
    | Missing_symbol_in_exponent
    | Missing_digits_in_exponent
    | Picture_describes_empty_data_item
    | Numeric_item_cannot_exceed_38_digits of int


  (* Nicolas' style interface :-) *)
  module type ENV = sig
    val decimal_char: char
    val currency_signs: Cobol_common.Basics.CharSet.t
  end

end

type t = TYPES.picture
[@@deriving show, ord]

open TYPES

val pp_category: category Pretty.printer

(** [is_edited c] indicates whether the given category represents an edited
    item *)
val is_edited: category -> bool

(** data size (in "characters" --- probably with implemententor specific
    semantics) *)
val data_size: category -> int

(** display size, after editions; corresponds to "size" in standards *)
val size: category -> int

val of_string: config -> string ->
  (TYPES.picture,
   (TYPES.error * (int * int))                        (* = (error, (pos, len)) *)
     list * TYPES.picture) result

val alphanumeric: size:int -> TYPES.picture

module Make (Config: Cobol_config.T) (Env: ENV) : sig

  exception InvalidPicture of
      string with_loc * Cobol_common.Diagnostics.diagnostics * picture

  val of_string: string with_loc -> t with_loc
end

val pp_error: error Pretty.printer

val rev_errors_with_loc: loc:srcloc -> (error * (int * int)) list ->
  error with_loc list

val error_diagnostics: loc:srcloc -> (error * (int * int)) list ->
  Cobol_common.Diagnostics.diagnostics

val pp_meaning_of_precedence_index
  : decimal_char: char -> Format.formatter -> int -> unit

(** Verifies that the picture string is interpreted as `expect`,
   i.e. the result of `pp_picture`. If not, displays the difference on
   stderr and returns `false` *)
val unit_test
  : ?config:TYPES.config -> expect:string -> string -> bool
