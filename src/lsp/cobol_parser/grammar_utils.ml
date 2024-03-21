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
  | S_DATA_DIVISION
  | S_FILE_SECTION
  | S_FILE_SECTION_PART of file_descr with_loc list with_loc
  | S_WORKING_STORAGE_SECTION of working_storage_section with_loc
  | S_LOCAL_STORAGE_SECTION of local_storage_section with_loc    (* +COB2002 *)
  | S_LINKAGE_SECTION of linkage_section with_loc
  | S_COMMUNICATION_SECTION of communication_section with_loc    (* +COB2002 *)
  | S_REPORT_SECTION of report_section with_loc
  | S_SCREEN_SECTION of screen_section with_loc                  (* +COB2002 *)

let empty_data_division = {
  file_section = [];
  working_storage_section = [];
  local_storage_section = [];
  linkage_section = [];
  communication_section = [];
  report_section = [];
  screen_section = [];
}

let build_data_division list =
  let loc = list.loc in
  let rec iter d list =
    match list with
    | [] -> d
    | sentence :: list ->
      let d = match sentence with
        | S_DATA_DIVISION
        | S_FILE_SECTION -> d
        | S_FILE_SECTION_PART ss ->
          { d with file_section = ss :: d.file_section }
        | S_WORKING_STORAGE_SECTION ss ->
          { d with working_storage_section =
                     ss :: d.working_storage_section }
        | S_LOCAL_STORAGE_SECTION ss ->
          { d with local_storage_section = ss :: d.local_storage_section }
        | S_LINKAGE_SECTION ss ->
          { d with linkage_section = ss :: d.linkage_section }
        | S_COMMUNICATION_SECTION ss ->
          { d with communication_section = ss :: d.communication_section }
        | S_REPORT_SECTION ss ->
          { d with report_section = ss :: d.report_section }
        | S_SCREEN_SECTION ss ->
          { d with screen_section = ss :: d.screen_section }
      in
      iter d list
  in
  let d = iter empty_data_division list.payload in
  let d =
    {
      file_section = List.rev d.file_section ;
      working_storage_section = List.rev d.working_storage_section ;
      local_storage_section = List.rev d.local_storage_section ;
      linkage_section = List.rev d.linkage_section ;
      communication_section = List.rev d.communication_section ;
      report_section = List.rev d.report_section ;
      screen_section = List.rev d.screen_section ;
    }
  in
  if d = empty_data_division then
    None
  else
    Some { payload=d ; loc }
