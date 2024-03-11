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

open PTree_types

open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)

(* --- *)

class virtual ['a] folder = object
  inherit ['a] Data_sections_visitor.folder
  method fold_data_division: (data_division, 'a) fold = default
  method fold_data_division': (data_division with_loc, 'a) fold = default

  method fold_file_section': (file_section with_loc, 'a) fold = default
  method fold_working_storage_section': (working_storage_section with_loc, 'a) fold = default
  method fold_linkage_section': (linkage_section with_loc, 'a) fold = default
  method fold_communication_section': (communication_section with_loc, 'a) fold = default
  method fold_local_storage_section': (local_storage_section with_loc, 'a) fold = default
  method fold_report_section': (report_section with_loc, 'a) fold = default
  method fold_screen_section': (screen_section with_loc, 'a) fold = default
end

let fold_file_section' (v: _ #folder) =
  handle' v#fold_file_section' v
    ~fold:Data_sections_visitor.fold_file_section

let fold_working_storage_section' (v: _ #folder) =
  handle' v#fold_working_storage_section' v
    ~fold:Data_sections_visitor.fold_working_storage_section

let fold_linkage_section' (v: _ #folder) =
  handle' v#fold_linkage_section' v
    ~fold:Data_sections_visitor.fold_linkage_section

let fold_communication_section' (v: _ #folder) =
  handle' v#fold_communication_section' v
    ~fold:Data_sections_visitor.fold_communication_section

let fold_local_storage_section' (v: _ #folder) =
  handle' v#fold_local_storage_section' v
    ~fold:Data_sections_visitor.fold_local_storage_section

let fold_report_section' (v: _ #folder) =
  handle' v#fold_report_section' v
    ~fold:Data_sections_visitor.fold_report_section

let fold_screen_section' (v: _ #folder) =
  handle' v#fold_screen_section' v
    ~fold:Data_sections_visitor.fold_screen_section

let fold_data_division (v: _#folder) =
  handle v#fold_data_division
    ~continue:begin fun { file_section; working_storage_section;
                          linkage_section; communication_section;
                          local_storage_section; report_section;
                          screen_section } x -> x
      >> fold_option v file_section
        ~fold:fold_file_section'
      >> fold_option v working_storage_section
        ~fold:fold_working_storage_section'
      >> fold_option v linkage_section
        ~fold:fold_linkage_section'
      >> fold_option v communication_section
        ~fold:fold_communication_section'
      >> fold_option v local_storage_section
        ~fold:fold_local_storage_section'
      >> fold_option v report_section
        ~fold:fold_report_section'
      >> fold_option v screen_section
        ~fold:fold_screen_section'
    end

let fold_data_division' (v: _ #folder) =
  handle' v#fold_data_division' v ~fold:fold_data_division
