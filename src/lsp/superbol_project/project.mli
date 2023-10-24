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

open Cobol_common.Diagnostics.TYPES

module TYPES: sig

  type rootdir

  type project = {
    rootdir: rootdir;
    config: Project_config.t;
    config_filename: string;
  }

  type layout = {
    project_config_filename: string;
  }

end
include module type of TYPES
  with type rootdir = TYPES.rootdir
   and type project = TYPES.project
   and type layout = TYPES.layout

type t = project

(** [for_ ~rootdir ~layout] retrieves a project based on its root directory.
    This may trigger reading project configuration files if the project was not
    yet loaded. *)
val for_: rootdir:rootdir -> layout:layout -> t with_diags

(** [in_existing_dir dirname ~layout] retrieves a project after checking
    [dirname] actually refers to an exising directory that can serve as root for
    the project.  The same notes as {!for_} apply, with the addition that
    {!Invalid_argument} is raised in case [dirname] is not the name of an
    existing directory. *)
val in_existing_dir: string -> layout:layout -> t with_diags

(** [rootdir_for ~filename ~layout] locates the project directory (that contains
    a file with given name [layout.project_config_filename]) for a given file
    name.  Returns the name of the directory that contains the file if no
    project file is found. *)
val rootdir_for: filename:string -> layout:layout -> rootdir

(** [libpath_for ~filename project] constructs a list of directory names where
    copybooks are looked up, for a given source file name, in the given
    project. *)
val libpath_for: filename:string -> t -> string list

(** [detect_copybook ~filename project] indicates whether a file name should be
    treated as a copybook within [project]. *)
val detect_copybook: filename:string -> t -> bool

(** Cached representation *)

type cached
val to_cache: t -> cached
val of_cache: rootdir:rootdir -> layout:layout -> cached -> t with_diags

(** Collections *)

module SET: sig
  include Set.S with type elt = t
  val for_rootdir: rootdir:rootdir -> t -> elt
  val mem_rootdir: rootdir:rootdir -> t -> bool
end
module MAP: Map.S with type key = t

(** Miscellaneous *)

val rootdir: t -> rootdir
val config: t -> Project_config.t
val string_of_rootdir: rootdir -> string
val relative_path_for: filename:string -> t -> string
val absolute_path_for: filename:string -> t -> string
