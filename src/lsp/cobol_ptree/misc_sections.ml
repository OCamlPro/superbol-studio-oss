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
open Terms

(* -------------------- IDENTIFICATION DIVISION (EXTRA) -------------------- *)

type informational_paragraphs =
  informational_paragraph with_loc list
[@@deriving ord]

and informational_paragraph =
  informational_paragraph_header * comment_entry with_loc

and informational_paragraph_header =
  | Author
  | DateCompiled
  | DateModified
  | DateWritten
  | Installation
  | Remarks
  | Security

and comment_entry = string list

type options_paragraph =
  options_clause with_loc list
[@@deriving ord]

and options_clause =
  | Arithmetic of arithmetic_mode
  | DefaultRoundedMode of rounding_mode
  | EntryConvention of entry_convention
  | FloatBinaryDefault of Data_descr.endianness_mode
  | FloatDecimalDefault of Data_descr.encoding_endianness               (* 1+ *)
  | IntermediateRounding of rounding_mode (* not all are valid (TODO:
                                             restriction with type param) *)

and arithmetic_mode =
  | Native
  | Standard        (* ~COB2002 *)
  | StandardBinary
  | StandardDecimal

(* Other conventions may be defined by the implementor *)
and entry_convention =
  | COBOL

(* --- *)

let pp_entry_convention ppf = function
  | COBOL -> Fmt.pf ppf "COBOL"

let pp_arithmetic_mode ppf = function
  | Native -> Fmt.pf ppf "NATIVE"
  | Standard -> Fmt.pf ppf "STANDARD"
  | StandardBinary -> Fmt.pf ppf "STANDARD-BINARY"
  | StandardDecimal -> Fmt.pf ppf "STANDARD-DECIMAL"

let pp_options_clause ppf = function
  | Arithmetic am ->
      Fmt.pf ppf "ARITHMETIC %a" pp_arithmetic_mode am
  | DefaultRoundedMode rm ->
      Fmt.pf ppf "DEFAULT ROUNDED MODE %a" pp_rounding_mode rm
  | EntryConvention ec ->
      Fmt.pf ppf "ENTRY-CONVENTION %a" pp_entry_convention ec
  | FloatBinaryDefault em ->
      Fmt.pf ppf "FLOAT-BINARY %a" Data_descr.pp_endianness_mode em
  | FloatDecimalDefault ee ->
      Fmt.pf ppf "FLOAT-DECIMAL %a" Data_descr.pp_encoding_endianness ee
  | IntermediateRounding rm ->
      Fmt.pf ppf "INTERMEDIATE-ROUNDING %a" pp_rounding_mode rm

let pp_options_paragraph : options_paragraph Fmt.t =
  Fmt.(any "OPTIONS.@ " ++
       box (list ~sep:sp (pp_with_loc pp_options_clause)) ++
       any ".")


(* --- *)

let pp_informational_paragraph_header ppf = function
  | Author -> Fmt.pf ppf "AUTHOR"
  | DateCompiled -> Fmt.pf ppf "DATE-COMPILED"
  | DateModified -> Fmt.pf ppf "DATE-MODIFIED"
  | DateWritten -> Fmt.pf ppf "DATE-WRITTEN"
  | Installation -> Fmt.pf ppf "INSTALLATION"
  | Remarks -> Fmt.pf ppf "REMARKS"
  | Security -> Fmt.pf ppf "SECURITY"

let pp_comment_entry: comment_entry Pretty.printer =
  Fmt.(list ~sep:sp string)

let pp_informational_paragraph: informational_paragraph Pretty.printer =
  Fmt.(box ~indent:4 @@                     (* <- indent by 4 to avoid Area A *)
       pair ~sep:(any ".@ ")
         pp_informational_paragraph_header
         (pp_with_loc pp_comment_entry))

let pp_informational_paragraphs: informational_paragraphs Pretty.printer =
  Fmt.(list ~sep:(any "@\n")                                (* force newlines *)
         (pp_with_loc pp_informational_paragraph))
