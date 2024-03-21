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

open Common
open Data_sections

type data_division =
  {
    file_section: file_section with_loc list;
    working_storage_section: working_storage_section with_loc list;
    linkage_section: linkage_section with_loc list;
    communication_section: communication_section with_loc list;
    local_storage_section: local_storage_section with_loc list;
    report_section: report_section with_loc list;
    screen_section: screen_section with_loc list;
  }
[@@deriving ord]

let pp_data_division ppf { file_section;
                           working_storage_section; linkage_section;
                           communication_section; local_storage_section;
                           report_section; screen_section } =
  let pp_section pp ppf section =
    Fmt.(list (sp ++ vbox (pp_with_loc pp))) ppf section
  in
  Fmt.pf ppf "@[<v 2>DATA DIVISION.";
  pp_section pp_file_section ppf file_section;
  pp_section pp_working_storage_section ppf working_storage_section;
  pp_section pp_linkage_section ppf linkage_section;
  pp_section pp_communication_section ppf communication_section;
  pp_section pp_local_storage_section ppf local_storage_section;
  pp_section pp_report_section ppf report_section;
  pp_section pp_screen_section ppf screen_section;
  Fmt.pf ppf "@]"
