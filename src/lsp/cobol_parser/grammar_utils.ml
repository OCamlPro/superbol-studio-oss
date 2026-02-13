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
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

(* Note: we can share the same source overlay manager across several parsers as
   long as we don't parse localized tokens using multiple instances of the
   parser and in parallel.  If that were to change, the manager would need to be
   passed as parameter to the grammar. *)
module Overlay_manager =
  Cobol_preproc.Src_overlay.New_manager (struct
    let name = __MODULE__
  end) ()

let relation_condition ~neg (binrel: binary_relation) = function
  | None ->
      Cobol_ptree.Terms_helpers.neg_condition ~neg @@ Relation binrel
  | Some (LOr, flatop) ->
      Abbrev (neg, binrel, LOr, flatop)
  | Some (LAnd, flatop) ->
      Abbrev (neg, binrel, LAnd, flatop)


type data_division_sentence =
  | S_DATA_DIVISION_HEADER of srcloc
  | S_FILE_SECTION_HEADER of srcloc
  | S_FILE_SECTION of file_section with_loc
  | S_WORKING_STORAGE_SECTION of working_storage_section with_loc
  | S_LOCAL_STORAGE_SECTION of local_storage_section with_loc    (* +COB2002 *)
  | S_LINKAGE_SECTION of linkage_section with_loc
  | S_COMMUNICATION_SECTION of communication_section with_loc    (* +COB2002 *)
  | S_REPORT_SECTION of report_section with_loc
  | S_SCREEN_SECTION of screen_section with_loc                  (* +COB2002 *)

let empty_data_division = {
  file_sections = [];
  working_storage_sections = [];
  local_storage_sections = [];
  linkage_sections = [];
  communication_sections = [];
  report_sections = [];
  screen_sections = [];
}

let build_data_division = function
  | { payload = []; _ } -> None            (* no `DATA DIVISION` header at all *)
  | { payload = list; loc }  ->
      let ( +@+ ) = Cobol_common.Srcloc.concat in
      let rec rebuild_sections list d =    (* sentences are in reversed order *)
        match list with
        | [] ->
            d
        | S_DATA_DIVISION_HEADER _loc :: tl
        | S_FILE_SECTION_HEADER _loc :: tl ->
            rebuild_sections tl d                   (* ignore lonesome header *)
        | S_FILE_SECTION ss ::
          S_FILE_SECTION_HEADER loc :: tl ->
            let ss = ~&ss &@ (loc +@+ ~@ss) in
            rebuild_sections tl
              { d with file_sections = ss :: d.file_sections }
        | S_FILE_SECTION ss :: tl ->
            rebuild_sections tl
              { d with file_sections = ss :: d.file_sections }
        | S_WORKING_STORAGE_SECTION ss :: tl ->
            rebuild_sections tl
              { d with working_storage_sections =
                         ss :: d.working_storage_sections }
        | S_LOCAL_STORAGE_SECTION ss :: tl ->
            rebuild_sections tl
              { d with local_storage_sections = ss :: d.local_storage_sections }
        | S_LINKAGE_SECTION ss :: tl ->
            rebuild_sections tl
              { d with linkage_sections = ss :: d.linkage_sections }
        | S_COMMUNICATION_SECTION ss :: tl ->
            rebuild_sections tl
              { d with communication_sections = ss :: d.communication_sections }
        | S_REPORT_SECTION ss :: tl ->
            rebuild_sections tl
              { d with report_sections = ss :: d.report_sections }
        | S_SCREEN_SECTION ss :: tl ->
            rebuild_sections tl
              { d with screen_sections = ss :: d.screen_sections }
      in
      Some (rebuild_sections list empty_data_division &@ loc)

let build_simple_program opts_par env_par datat_div proc_div =
  Program {
    program_name =
      (Name (Cobol_ptree.Dummies.dummy_name
        ~pos:Lexing.dummy_pos)) &@ Cobol_common.Srcloc.dummy; (* TODO: Use filename *)
    program_as = None;
    program_level = ProgramDefinition {
      mode = None;
      has_identification_division_header = false;
      preliminary_informational_paragraphs = [];
      supplementary_informational_paragraphs = [];
      nested_programs = []; };
    program_options = opts_par;
    program_env = env_par;
    program_data = (match datat_div with
      | Some dd -> build_data_division dd
      | None -> None);
    program_proc = Some proc_div;
    program_end_name = None;
  }
