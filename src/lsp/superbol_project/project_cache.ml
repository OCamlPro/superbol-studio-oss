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

open Ez_file.V1
open Project.TYPES

open EzFile.OP

(* --- *)

let storage_location ?(enable_caching = true)
    ?(project_layout = Project_layout.default) ?fallback_storage_directory
    () =
  match project_layout.relative_work_dirname,
        fallback_storage_directory with
  | _ when not enable_caching ->
      Ok No_cache_storage
  | None, None ->
      Ok No_cache_storage
  | None, Some dirname ->
      Ok (Store_cache_in_shared_dir { dirname })
  | Some relative_work_dirname, _ ->
      let invalid_dir reason =
        Pretty.string_to Result.error
          ("relative_work_dirname: "^^reason^^" (got `%s')")
          relative_work_dirname
      in
      if relative_work_dirname = ""
      then invalid_dir "invalid directory name"
      else if EzFile.is_absolute relative_work_dirname
      then invalid_dir "relative directory name expected"
      else
        Result.ok @@
        Store_cache_in_relative_dir {
          relative_dirname = relative_work_dirname // "cache";
        }

let default_storage_location =
  Result.get_ok (storage_location ())

(* --- *)

module CACHE = Superbol_cache.Cache
module CACHE_TABLE = Ephemeron.K1.Make (struct
    include String
    let hash = Hashtbl.hash
  end)

let per_rootdir_cache =
  CACHE_TABLE.create 1

(** Where a project's cache lives.  Both storage modes name a directory; the
    shared one keeps projects apart by the digest of their root. *)
let cache_root ~storage_location ~rootdir =
  match storage_location with
  | No_cache_storage ->
      None
  | Store_cache_in_relative_dir { relative_dirname } ->
      Some (rootdir // relative_dirname)
  | Store_cache_in_shared_dir { dirname } ->
      Some (dirname // Digest.(to_hex @@ string @@ rootdir))

let for_ ~rootdir ~storage_location : (string * CACHE.t) option =
  match CACHE_TABLE.find_opt per_rootdir_cache rootdir with
  | Some (root, cache) ->
      Some (root, cache)
  | None ->
      match cache_root ~storage_location ~rootdir with
      | Some root ->
          let store = Superbol_platform.Cache_store.create ~root () in
          let cache = CACHE.create ~store () in
          CACHE_TABLE.replace per_rootdir_cache rootdir (root, cache);
          Some (root, cache)
      | None ->
          None
