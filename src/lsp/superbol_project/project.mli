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

(** {1 Type definitions} *)

module TYPES: sig

  type rootdir

  type project = {
    rootdir: rootdir;
    config: Project_config.t;
    config_filename: string;
  }

  type layout = {
    project_config_filename: string;
    relative_work_dirname: string;
  }

end
include module type of TYPES
  with type rootdir = TYPES.rootdir
   and type project = TYPES.project
   and type layout = TYPES.layout

type t = project

(** {1 Initializers} *)

(** [rootdir_at ~dirname] initializes a project into an existing directory.

    Raises {!Invalid_argument} in case [dirname] is not the name of an existing
    directory. *)
val rootdir_at: dirname:string -> rootdir

(** [rootdir_for ~filename ~layout] locates the project directory for a given
    file name.  This project directory is the closest parent directory of
    [filename] that contains a file with the name
    [layout.project_config_filename].  Returns the name of the directory that
    contains [filename] if no such file is found. *)
val rootdir_for: filename:string -> layout:layout -> rootdir

(** [for_ ~rootdir ~layout] retrieves a project based on its root directory.

    This may trigger reading project configuration files if the project was not
    yet loaded.  The name of the project's configuration file is determined by
    the [layout] argument. *)
val for_: rootdir:rootdir -> layout:layout -> t with_diags

(** [with_default_config ~rootdir ~layout] initializes a project structure with
    a default configuration. *)
val with_default_config: rootdir:rootdir -> layout:layout -> t

(** {1 Accessors} *)

val rootdir: t -> rootdir

(** [libpath_for ~filename project] constructs a list of directory names where
    copybooks are looked up, for a given source file name, in the given
    project. *)
val libpath_for: filename:string -> t -> string list

(** [detect_copybook ~filename project] indicates whether a file name should be
    treated as a copybook within [project]. *)
val detect_copybook: filename:string -> t -> bool

(** {1 Cached representation} *)

type cached

(** [to_cache project] constructs a cached representation for [project]. *)
val to_cache: t -> cached

(** [of_cache ~rootdir ~layout cached_project] attempts to load and return a
    cached project.  Behaves like [for_ ~rootdir ~layout] in case of error
    (outdated or missing configuration file). *)
val of_cache: rootdir:rootdir -> layout:layout -> cached -> t with_diags

(** {1 Project configuration} *)

val config: t -> Project_config.t

(** [save_config project] dumps the current configuration of [project] into a
    TOML file.

    The name of the configuration file is determined via the [layout] argument
    given to (the first call to) {!for_} or {!of_cache} with [project]'s root
    directory. *)
val save_config: t -> unit

(** {1 Collections} *)

module SET: sig
  include Set.S with type elt = t
  val for_rootdir: rootdir:rootdir -> t -> elt
  val mem_rootdir: rootdir:rootdir -> t -> bool
end
module MAP: Map.S with type key = t

(** {1 Miscellaneous} *)

val string_of_rootdir: rootdir -> string
val relative_path_for: filename:string -> t -> string
val absolute_path_for: filename:string -> t -> string
