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

(** [storage_location ~enable_caching ~project_layout
    ~fallback_storage_directory ()] determines if and where project data is
    going to be cached.

    - [fallback_storage_directory], when provided (and if [enable_caching]
    holds), is the name of a directory that is used to store a cache whenever
    [project_layout] does not provide a per-project storage directory ({i i.e,}
    [project_layout.relative_work_dirname = None]). *)
val storage_location
  : ?enable_caching:bool
  -> ?project_layout:Project.TYPES.layout
  -> ?fallback_storage_directory:string
  -> unit
  -> (Project.TYPES.cache_storage_location, string) result

val default_storage_location: Project.TYPES.cache_storage_location

(* --- *)

(** [for_ ~rootdir ~storage_location] returns the cache (along with the
    directory it lives in) for a project at [rootdir] and given a
    [storage_location].  Returns [None] if [storage_location] does not permit
    caching. *)
val for_
  : rootdir: string
  -> storage_location: Project.TYPES.cache_storage_location
  -> (string * Superbol_cache.Cache.t) option
