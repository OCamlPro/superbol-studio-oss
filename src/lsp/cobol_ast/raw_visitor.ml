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

module Make_for_misc_sections = Raw_misc_sections_visitor.Make
module Make_for_data_sections = Raw_data_sections_visitor.Make
module Make_for_data_division = Raw_data_division_visitor.Make
module Make_for_statements = Raw_statements_visitor.Make
module Make_for_proc_division = Raw_proc_division_visitor.Make
module Make_for_compilation_group = Raw_compilation_group_visitor.Make
