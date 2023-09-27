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
  type[@deriving ord] informational_paragraphs
  val pp_informational_paragraphs : informational_paragraphs Fmt.t
  type[@deriving ord] options_paragraph
  val pp_options_paragraph : options_paragraph Fmt.t
  type[@deriving ord] environment_division
  val pp_environment_division : environment_division Fmt.t
end

module type PICTURE = sig
  type picture                 [@@deriving show, ord]
end

module type DATA_SECTIONS = sig
  type[@deriving ord] working_storage_section
  val pp_working_storage_section : working_storage_section Pretty.printer
  type[@deriving ord] linkage_section
  val pp_linkage_section : linkage_section Pretty.printer
  type[@deriving ord] file_section
  val pp_file_section : file_section Pretty.printer
  type[@deriving ord] communication_section
  val pp_communication_section : communication_section Pretty.printer
  type[@deriving ord] local_storage_section
  val pp_local_storage_section : local_storage_section Pretty.printer
  type[@deriving ord] report_section
  val pp_report_section : report_section Pretty.printer
  type[@deriving ord] screen_section
  val pp_screen_section : screen_section Pretty.printer
end

module type DATA_DIVISION = sig
  (* NOTE: could become PROG_DIVISIONS if relevant *)
  type[@deriving ord] data_division
  val pp_data_division : data_division Pretty.printer
end

module type STATEMENTS = sig
  type[@deriving ord] statement
  val pp_statement : statement Fmt.t
  type[@deriving ord] statements
  val pp_statements : statements Fmt.t
  val pp_dump_statements : statements Fmt.t
end

module type PROC_DIVISION = sig
  type[@deriving ord] procedure_division
  val pp_procedure_division : procedure_division Fmt.t
end

module type COMPILATION_GROUP = sig
  type compilation_group       [@@deriving show, ord]
end
