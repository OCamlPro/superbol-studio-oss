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

open Indent_type

val check_pos:
  Cobol_preproc.Src_format.any ->
  Cobol_common.Srcloc.srcloc ->
  int ->
  indent_record list ->
  bool ->
  indent_record list

val failure_msg: Cobol_common.Srcloc.srcloc -> string

val offset_of_keyword: context_kind -> int

val offset_of_context: context -> int

val push_context: context_kind -> context -> context

val is_data_decl: string -> bool

val reduce_level: int -> context -> context

val handle_period: context -> context

val exp_scope_termination: context_kind -> context -> context

val pop_until: context_kind -> context -> context

val pop_until_division: context -> context

val pop_until_compilation_unit: context -> context

val imp_scope_termination: context -> context

val phrase_termination: context -> context

val phrase_termination_until: context_kind -> context -> context
