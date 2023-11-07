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
    relative_work_dirname = Some "_superbol";
    rootdir_fallback_policy = Same_as_file_directory;
  }

let try_load ~f arg =
  try f arg with
  | Superbol_project.Config.ERROR e ->
      DIAGS.(pp Fmt.stderr @@ One.error "%a"
               Superbol_project.Diagnostics.pp_error e);
      exit 2
  | Sys_error msg | Invalid_argument msg ->
      DIAGS.(pp Fmt.stderr @@ One.error "%s" msg);
      exit 2

(* Direct loading (based on directory name) *)

let in_ ~dirname =
  Pretty.error "Loading project in `%s'@." dirname;
  try_load () ~f:begin fun () ->
    let rootdir = Superbol_project.rootdir_at ~dirname in
    let project = Superbol_project.for_ ~rootdir ~layout in
    DIAGS.show_n_forget project
  end

let load ?(dirname = EzFile.current_dir_name) () = in_ ~dirname

(* Indirect loading (based on file name, looking up the directory hierarchy) *)

let for_ ~filename =
  Pretty.error "Looking up project for `%s'@." filename;
  try_load () ~f:begin fun () ->
    let rootdir = Superbol_project.rootdir_for ~filename ~layout in
    let project = Superbol_project.for_ ~rootdir ~layout in
    DIAGS.show_n_forget project
  end

let for_filename filename = for_ ~filename
