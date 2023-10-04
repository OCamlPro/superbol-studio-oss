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

(* Paging *)

type free = UnlimitedLines
type fixed = LimitedLines
type _ paging =
  | FreePaging: free paging
  | FixedWidth: fixed_paging_params -> fixed paging
and fixed_paging_params =
  {
    cut_at_col: int;
    alphanum_padding: char option;
  }

(* Actual format and indicator positioning *)

type 'k source_format = 'k indicator_position * 'k paging
and _ indicator_position =
  |    NoIndic:  free indicator_position
  | FixedIndic: fixed indicator_position
  | XOpenIndic: fixed indicator_position
  |   CRTIndic: fixed indicator_position
  |   TrmIndic: fixed indicator_position
  |  CBLXIndic: fixed indicator_position
and any =
  | SF: 'k source_format -> any                                        [@@unboxed]

type comment_entry_termination =                  (* skip until... *)
  | Newline                                       (* ... newline *)
  | Period                                        (* ... next period (unused) *)
  | AreaB of { first_area_b_column: int }         (* ... next word in area A *)

(* --- *)

val equal: 'k source_format -> 'r source_format -> bool

val from_config: Cobol_config.source_format -> any
(* val to_config: 'k source_format -> Cobol_config.source_format *)
val decypher
  : dialect: Cobol_config.dialect
  -> string
  -> (any, [> `SFUnknown of string ]) result

val comment_entry_termination: _ source_format -> comment_entry_termination
val first_area_b_column: _ source_format -> int option
