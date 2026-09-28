(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Project.TYPES

let default =
  {
    project_config_filename = "superbol.toml";
    relative_work_dirname = Some "_superbol";
    rootdir_fallback_policy = Same_as_file_directory;
  }

let create ?shared_storage_directory () =
  if shared_storage_directory = None
  then default
  else { default with relative_work_dirname = None }
