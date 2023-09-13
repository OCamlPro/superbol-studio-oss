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
  type informational_paragraphs [@@deriving show]
  type options_paragraph        [@@deriving show]
  type environment_division     [@@deriving show]
end

module type PICTURE = sig
  type picture                 [@@deriving show]
end

module type DATA_SECTIONS = sig
  type working_storage_section [@@deriving show]
  type linkage_section         [@@deriving show]
  type file_section            [@@deriving show]
  type communication_section   [@@deriving show]
  type local_storage_section   [@@deriving show]
  type report_section          [@@deriving show]
  type screen_section          [@@deriving show]
end

module type DATA_DIVISION = sig
  (* NOTE: could become PROG_DIVISIONS if relevant *)
  type data_division           [@@deriving show]
end

module type STATEMENTS = sig
  type statement               [@@deriving show]
  type statements              [@@deriving show]
end

module type PROC_DIVISION = sig
  type procedure_division      [@@deriving show]
end

module type COMPILATION_GROUP = sig
  type compilation_group       [@@deriving show]
end
