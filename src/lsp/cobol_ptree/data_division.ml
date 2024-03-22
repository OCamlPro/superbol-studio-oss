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
    file_sections: file_section with_loc list;
    working_storage_sections: working_storage_section with_loc list;
    linkage_sections: linkage_section with_loc list;
    communication_sections: communication_section with_loc list;
    local_storage_sections: local_storage_section with_loc list;
    report_sections: report_section with_loc list;
    screen_sections: screen_section with_loc list;
  }
[@@deriving ord]

let pp_data_division ppf { file_sections;
                           working_storage_sections; linkage_sections;
                           communication_sections; local_storage_sections;
                           report_sections; screen_sections } =
  let pp_section pp ppf section =
    Fmt.(list (sp ++ vbox (pp_with_loc pp))) ppf section
  in
  Fmt.pf ppf "@[<v 2>DATA DIVISION.";
  pp_section pp_file_section ppf file_sections;
  pp_section pp_working_storage_section ppf working_storage_sections;
  pp_section pp_linkage_section ppf linkage_sections;
  pp_section pp_communication_section ppf communication_sections;
  pp_section pp_local_storage_section ppf local_storage_sections;
  pp_section pp_report_section ppf report_sections;
  pp_section pp_screen_section ppf screen_sections;
  Fmt.pf ppf "@]"
