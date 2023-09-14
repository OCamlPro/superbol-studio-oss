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

module type T = sig
  type t
  val accept_stmt: t
  val allocate_stmt: t
  val alphabet_clause: t
  val arithmetic_clause: t
  val class_specifier: t
  val column_clause: t
  val constant: t
  val currency_clause: t
  val default_clause: t
  val dynlen_struct_clause: t
  val erase_clause: t
  val exit_stmt: t
  val factory_paragraph: t
  val float_binary_clause: t
  val float_decimal_clause: t
  val function_specifier: t
  val interface_specifier: t
  val intermediate_rounding_clause: t
  val line_clause: t
  val lock_mode_clause: t
  val lock_on_phrase: t
  val object_computer_paragraph: t
  val object_paragraph: t
  val occurs_clause: t
  val options_paragraph: t
  val program_id_paragraph: t
  val read_stmt: t
  val resume_stmt: t
  val retry_phrase: t
  val rounded_phrase: t
  val screen_descr_entry: t
  val set_attribute_stmt: t
  val set_stmt: t
  val sharing_clause: t
  val sharing_phrase: t
  val stop_stmt: t
  val typedef_clause: t
  val usage_clause: t
  val validate_status_clause: t
end
