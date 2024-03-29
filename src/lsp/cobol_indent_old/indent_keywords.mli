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

val string_of_keyword: Indent_type.context_kind -> string

val data_context_of_str: string -> Indent_type.data_context

val proc_context_of_str: string -> Indent_type.proc_context

(* To check whether the string is a keyword of statement *)
val is_statement: string -> bool

(* To check whether the keyword is implicitly terminable *)
val is_not_imp_terminable: Indent_type.context_kind -> bool

(* To check whether the keyword is a keyword of phrase *)
val is_phrase: Indent_type.context_kind -> bool
