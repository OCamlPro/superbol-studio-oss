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

open Cobol_ptree

module Overlay_manager: Cobol_preproc.Src_overlay.MANAGER

type data_division_sentence =
  | S_DATA_DIVISION_HEADER of srcloc
  | S_FILE_SECTION_HEADER of srcloc
  | S_FILE_SECTION of file_descr with_loc list with_loc
  | S_WORKING_STORAGE_SECTION of working_storage_section with_loc
  | S_LOCAL_STORAGE_SECTION of local_storage_section with_loc
  | S_LINKAGE_SECTION of linkage_section with_loc
  | S_COMMUNICATION_SECTION of communication_section with_loc
  | S_REPORT_SECTION of report_section with_loc
  | S_SCREEN_SECTION of screen_section with_loc

(* Takes a *reversed* list of sentences. *)
val build_data_division: data_division_sentence list with_loc ->
  data_division with_loc option

(** Parser-only representation of the body of an EVALUATE 
    remembering locations of ignored clauses to display warnings. *)
type evaluate_body =
  {
    eb_branches: evaluate_branch list;
    eb_when_other: (srcloc list * srcloc * statements) option; (** 
      (loc of ignored WHEN clauses, loc of WHEN OTHER, statements) *)
  }

val evaluate_body_when_other: srcloc -> statements -> evaluate_body
val evaluate_body_last_branch: selection_object list with_loc -> statements ->
  evaluate_body
val evaluate_body_prepend_when: selection_object list with_loc -> statements ->
  evaluate_body -> evaluate_body
val evaluate_stmt: selection_subject list -> evaluate_body -> evaluate_stmt

val build_simple_program:
  Cobol_ptree.options_paragraph with_loc option ->
  environment_division with_loc option ->
  data_division_sentence list with_loc option ->
  procedure_division with_loc ->
  pos:Lexing.position ->
  compilation_unit
