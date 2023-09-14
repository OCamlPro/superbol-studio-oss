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

open Lsp_imports

module TYPES: sig
  type config =
    {
      (** Name of cache file, relative to project root directory. *)
      cache_relative_filename: string;
      cache_verbose: bool;
    }
end
include module type of TYPES
  with type config = TYPES.config

(** [save ~config docs] saves the caches of all the given document's
    projects.

    For any project [p] of which at least one document belongs to [docs], any
    document of [p] that does not belong to [docs] is removed from [p]'s
    cache.

    Some notifications about the saving process may be pushed via
    {!Lsp_io.send_notification}.  May raise some IO-related exceptions
    ({!Sys_error}, {!Failure}). *)
val save
  : config: TYPES.config
  -> Lsp_document.t URIMap.t
  -> unit

(** [load ~rootdir ~config ~layout] pre-loads cached documents pertaining to a
    project located in [rootdir], with project directory layout [layout].

    All projects in the returned map belong to the same project.  Note this map
    may actually be empty (for instance in case of missing, out-dated cache
    files, or cache files for a different version of the library).

    Some notifications about the loading process may be pushed via
    {!Lsp_io.send_notification}.  May raise some IO-related exceptions
    ({!Sys_error}, {!Failure}). *)
val load
  : rootdir:Lsp_project.rootdir
  -> layout: Lsp_project.layout
  -> config: TYPES.config
  -> Lsp_document.t URIMap.t
