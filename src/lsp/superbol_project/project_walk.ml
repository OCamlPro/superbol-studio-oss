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

(* Symbolic links to directories are not followed, which keeps the walk
   finite. *)
let source_files ~(platform: Cobol_common.Platform.TYPES.platform) ~layout
    project =
  let configured =
    List.map (fun (ext, _) -> String.lowercase_ascii ext)
      (Project.config project).source_format_by_extension
  in
  let is_source filename =
    platform.is_source_file ~filename ||
    match Filename.extension filename with
    | "" ->
        false
    | ext ->
        List.mem String.(lowercase_ascii @@ sub ext 1 (length ext - 1))
          configured
  in
  let work_dirname = Project.work_dirname ~layout project in
  let skip path =
    platform.is_hidden_file ~filename:path ||
    Option.equal String.equal (Some path) work_dirname
  in
  let excluded_dir ~relative_path =
    Project_config.excludes_dir ~relative_path (Project.config project)
  in
  (* [reldir] is [dir] relative to the project root, with ['/'] as separator,
     as exclusions are given. *)
  let rec walk ~reldir dir acc =
    List.fold_left begin fun acc name ->
      let path = Filename.concat dir name in
      if name = "" || skip path then acc else
        match platform.stat_file ~follow:false path with
        | Some { file_kind = Directory; _ } ->
            let relative_path =
              if reldir = "" then name else reldir ^ "/" ^ name in
            if excluded_dir ~relative_path
            then acc
            else walk ~reldir:relative_path path acc
        | Some { file_kind = Regular_file; _ } when is_source name ->
            path :: acc
        | Some { file_kind = Symbolic_link; _ } when is_source name ->
            begin match platform.stat_file path with
              | Some { file_kind = Regular_file; _ } -> path :: acc
              | _ -> acc
            end
        | _ ->
            acc
    end acc (platform.list_dir dir)
  in
  walk ~reldir:"" Project.(string_of_rootdir @@ rootdir project) []
