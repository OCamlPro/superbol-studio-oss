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

type error =
  | Caught_exception of { msg: string }
  | Malformed of { loc: srcloc; stuff: malformed_stuff }
  | Missing of { loc: srcloc; stuff: missing_stuff }
  | Unexpected of { loc: srcloc; stuff: unexpected_stuff }
  | Unsupported of { loc: srcloc; stuff: unsupported_stuff }
  | Unterminated of { loc: srcloc; stuff: unterminated_stuff }

and malformed_stuff =
  | Alphanumeric_literal
  | Data_item_at_level_78

and missing_stuff =
  | Continuation_of of string
  | Value_for_78_level_item of Cobol_ptree.data_name with_loc

and unexpected_stuff =
  | Pseudotext
  | Character_in_symbolic_EBCDIC of char
  | Clause_for_78_level_item of Cobol_ptree.data_clause with_loc
  | Multiple_values_for_78_level_item of Cobol_ptree.data_name with_loc
  | Symbolic_EBCDIC_orginal of int

and unsupported_stuff =
  | Global_clause_for_78_level_item

and unterminated_stuff =
  | Comment_entry
