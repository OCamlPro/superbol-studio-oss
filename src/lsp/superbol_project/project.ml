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
open EzFile.OP

module DIAGS = Cobol_common.Diagnostics

module TYPES = struct

  type rootdir = string

  type project = {
    rootdir: rootdir;
    config: Project_config.t;
    config_filename: string;
  }

  type layout = {
    project_config_filename: string;
    relative_work_dirname: string option;
    rootdir_fallback_policy: rootdir_fallback_policy;
  }

  and rootdir_fallback_policy =
    | Same_as_file_directory
    | Given_directory of string

end
include TYPES
type t = project

let rootdir { rootdir; _ } = rootdir
let config { config; _ } = config
let string_of_rootdir = Fun.id

module TABLE =
  Ephemeron.K1.Make (struct
    include String                                                 (* rootdir *)
    let hash = Hashtbl.hash
  end)

let table = TABLE.create 1

let rootdir_at ~dirname : rootdir =
  if EzFile.is_directory dirname
  then dirname
  else Fmt.invalid_arg "Expected existing directory: %s" dirname

let rootdir_for ~filename
    ~layout:{ project_config_filename; rootdir_fallback_policy; _ } =
  let rec try_dir dir =
    if EzFile.exists (dir // project_config_filename)
    then dir
    else
      let new_dir = EzFile.dirname dir in
      if new_dir = dir
      then raise Not_found                                  (* we are at root *)
      else try_dir new_dir
  in
  let dirname = EzFile.dirname filename in
  try try_dir dirname
  with Not_found -> match rootdir_fallback_policy with
    | Same_as_file_directory -> dirname
    | Given_directory dirname -> dirname

let with_default_config ~rootdir ~layout:{ project_config_filename; _ } =
  {
    rootdir;
    config = Project_config.new_default ();
    config_filename = rootdir // project_config_filename;
  }

let try_reading_config_file ~rootdir ~layout:{ project_config_filename; _ } =
  let config_filename = rootdir // project_config_filename in
  Project_config.load_file config_filename |>
  DIAGS.map_result ~f:(fun config -> { rootdir; config; config_filename })

let for_ ~rootdir ~layout =
  try DIAGS.result @@ TABLE.find table rootdir
  with Not_found ->
    let project = try_reading_config_file ~layout ~rootdir in
    TABLE.replace table rootdir project.result;
    project

let libpath_for ~filename { config; _ } =
  Project_config.libpath_for ~filename config

let detect_copybook ~filename { config; _ } =
  Project_config.detect_copybook ~filename config

let relative_path_for ~filename { rootdir; _ } =
  try Project_utils.relative_path ~filename rootdir
  with Invalid_argument _ -> filename             (* if not in project rootdir *)

let absolute_path_for ~filename { rootdir; _ } =
  if EzFile.is_absolute filename
  then filename       (* in case the file is not within its project directory *)
  else rootdir // filename

let save_config ?verbose { config_filename; config; _ } =
  Project_config.save ?verbose ~config_filename config

(* Caching *)

(** Persistent representation (for caching) *)
type cached = Project_config.cached

let to_cache { config; _ } =
  Project_config.to_cache config

let of_cache ~rootdir ~layout cached =
  let config_filename = rootdir // layout.project_config_filename in
  try
    let config = Project_config.of_cache ~config_filename cached in
    let project = { rootdir; config; config_filename } in
    TABLE.replace table rootdir project;
    DIAGS.result project
  with Project_config.BAD_CHECKSUM | Sys_error _ ->
    for_ ~rootdir ~layout

(* Collections *)

module M = struct
  type nonrec t = t
  let compare { rootdir = d1; _ } { rootdir = d2; _ } = String.compare d1 d2
  let equal { rootdir = d1; _ } { rootdir = d2; _ } = String.equal d1 d2
end
let have_same_rootdirs = M.equal

module SET = struct
  include Set.Make (M)
  let for_rootdir ~rootdir s =
    let p = find_first (fun p -> String.compare p.rootdir rootdir >= 0) s in
    if p.rootdir = rootdir then p else raise Not_found
  let mem_rootdir ~rootdir s =
    try ignore (for_rootdir ~rootdir s); true with Not_found -> false
end

module MAP = Map.Make (M)
