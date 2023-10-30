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

open Ez_file.V1

module DIAGS = Cobol_common.Diagnostics

let layout =
  Superbol_project.{
    project_config_filename = "superbol.toml";
    relative_work_dirname = "_superbol";
  }

let load_project ?(dirname = EzFile.current_dir_name) () =
  Pretty.error "Loading project in `%s'@." dirname;
  try
    let rootdir = Superbol_project.rootdir_at ~dirname in
    let project = Superbol_project.for_ ~rootdir ~layout in
    DIAGS.show_n_forget project
  with
  | Superbol_project.Config.ERROR e ->
      DIAGS.(pp Fmt.stderr @@ One.error "%a"
               Superbol_project.Diagnostics.pp_error e);
      exit 2
  | Sys_error msg | Invalid_argument msg ->
      DIAGS.(pp Fmt.stderr @@ One.error "%s" msg);
      exit 2
