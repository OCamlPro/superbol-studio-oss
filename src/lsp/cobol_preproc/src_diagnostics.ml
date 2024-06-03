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

type error =
  | Mismatch_in_alphanum_continuation of
      {
        continued_alphanum_loc: srcloc;
        expected_quotation: Text.quotation;
      }
  | Missing_continuation of
      {
        loc: srcloc;
        prefix: string;
      }
  | Unexpected of
      {
        loc: srcloc;
        item: unexpected_stuff;
      }
  | Unterminated_pseudotext of
      {
        loc: srcloc;
      }

and unexpected_stuff =
  | Character
  | Characters
  | Indicator_char of char
  | Indicator_string of string
  | Non_blank_area_A_on_continuation_line
  | Opening_alphanumeric_literal_delimiter of string
  | Word
  | Word_in_pseudotext of Text.text_word

let pp_unexpected_stuff ppf = function
  | Character ->
      Pretty.print ppf "character"
  | Characters ->
      Pretty.print ppf "characters"
  | Indicator_char c ->
      Pretty.print ppf "indicator:@ `%c'" c
  | Indicator_string s ->
      Pretty.print ppf "indicator:@ `%s'" s
  | Non_blank_area_A_on_continuation_line ->
      Pretty.print ppf "non-blank@ area@ A@ on@ continuation@ line"
  | Opening_alphanumeric_literal_delimiter str ->
      Pretty.print ppf "opening@ delimiter@ for@ alphanumeric@ literal: \
                        `%s'" str
  | Word ->
      Pretty.print ppf "text@ word"
  | Word_in_pseudotext word ->
      Pretty.print ppf "`%a'@ in@ pseudotext" Text.pp_word word

let error_loc = function
  | Mismatch_in_alphanum_continuation { continued_alphanum_loc = loc; _ }
  | Missing_continuation { loc; _ }
  | Unexpected { loc; _ }
  | Unterminated_pseudotext { loc } ->
      loc

let pp_error ppf = function
  | Mismatch_in_alphanum_continuation { expected_quotation; _ } ->
      Pretty.print ppf "Mismatch@ in@ continuation@ of@ alphanumeric@ literal@ \
                        (expected@ `%a'@ quotation@ character)\
                       " Text.pp_quote expected_quotation
  | Missing_continuation { prefix; _ } ->
      Pretty.print ppf "Missing@ continuation@ of@ `%s'" prefix
  | Unexpected { item; _ } ->
      Pretty.print ppf "Unexpected@ %a" pp_unexpected_stuff item
  | Unterminated_pseudotext _ ->
      Pretty.print ppf "Unterminated@ pseudotext"

type warning =
  | Warn_unexpected of
      {
        loc: srcloc;
        item: unexpected_stuff;
      }

let warning_loc = function
  | Warn_unexpected { loc; _ } -> loc

let pp_warning ppf = function
  | Warn_unexpected { item; _ } ->
      Pretty.print ppf "Unexpected@ %a" pp_unexpected_stuff item

(* --- *)

type diagnostics =
  {
    errors: error list;
    warnings: warning list;
  }
type t = diagnostics
let none = { errors = []; warnings = [] }
let add_error e diags = { diags with errors = e :: diags.errors }
let add_warning w diags = { diags with warnings = w :: diags.warnings }
