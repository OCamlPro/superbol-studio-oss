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

module type MISC_SECTIONS = sig
  (* IDENTIFICATION_DIVISION *)
  type informational_paragraphs [@@deriving ord]
  val pp_informational_paragraphs : informational_paragraphs Fmt.t
  type options_paragraph [@@deriving ord]
  val pp_options_paragraph : options_paragraph Fmt.t
  type environment_division [@@deriving ord]
  val pp_environment_division : environment_division Fmt.t
end

module type PICTURE = sig
  type picture                 [@@deriving show, ord]
end

module type DATA_SECTIONS = sig
  type working_storage_section [@@deriving ord]
  val pp_working_storage_section : working_storage_section Pretty.printer
  type linkage_section [@@deriving ord]
  val pp_linkage_section : linkage_section Pretty.printer
  type file_section [@@deriving ord]
  val pp_file_section : file_section Pretty.printer
  type communication_section [@@deriving ord]
  val pp_communication_section : communication_section Pretty.printer
  type local_storage_section [@@deriving ord]
  val pp_local_storage_section : local_storage_section Pretty.printer
  type report_section [@@deriving ord]
  val pp_report_section : report_section Pretty.printer
  type screen_section [@@deriving ord]
  val pp_screen_section : screen_section Pretty.printer
end

module type DATA_DIVISION = sig
  (* NOTE: could become PROG_DIVISIONS if relevant *)
  type data_division [@@deriving ord]
  val pp_data_division : data_division Pretty.printer
end

module type STATEMENTS = sig
  type statement [@@deriving ord]
  val pp_statement : statement Fmt.t
  type statements [@@deriving ord]
  val pp_statements : statements Fmt.t
  val pp_dump_statements : statements Fmt.t
end

module type PROC_DIVISION = sig
  type procedure_division [@@deriving ord]
  val pp_procedure_division : procedure_division Fmt.t
end

module type COMPILATION_GROUP = sig
  type compilation_group       [@@deriving show, ord]
end
