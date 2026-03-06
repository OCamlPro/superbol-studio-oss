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
    (** Name of the TOML file that is to be found at the root of each project's
        directory tree. *)

    relative_work_dirname: string option;
    (** Relative name of a directory where the LSP should put its working files
        (caches, etc).  No such storage is allowed when [None]. *)

    rootdir_fallback_policy: rootdir_fallback_policy;
    (** Policy to determine the root directory of projects based on individual
        filenames. *)
  }

  and rootdir_fallback_policy =
    | Same_as_file_directory
    | Given_directory of string

end
include module type of TYPES
  with type rootdir = TYPES.rootdir
   and type project = TYPES.project
   and type rootdir_fallback_policy = TYPES.rootdir_fallback_policy
   and type layout = TYPES.layout

type t = project

(** {1 Initializers} *)

(** [rootdir_at ~dirname] initializes a project into an existing directory.

    Raises [Invalid_argument] in case [dirname] is not the name of an existing
    directory. *)
val rootdir_at: dirname:string -> rootdir

(** [rootdir_for ~filename ~layout] locates the project directory for a given
    file name.  This project directory is the closest parent directory of
    [filename] that contains a file with the name
    [layout.project_config_filename].

    When no such file is found, the behavior depends on the given fallback
    policy: if [layout.rootdir_fallback_policy = Same_as_file_directory], the
    name of the directory that contains [filename] is chosen as root for the
    project; alternatively, if the fallback is [Given_directory dirname],
    directory [dirname] is used instead. *)
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

(** [copybook_lookup_config_for ~uri project] constructs a copybook lookup
    configuration for a source file with name [filename], in the given
    project. *)
val copybook_lookup_config_for
  : filename:string
  -> t
  -> Cobol_common.Copybook.lookup_config

(** [detect_copybook ~filename project] indicates whether a document with the
    given [filename] should be treated as a copybook in [project]. *)
val detect_copybook:
  platform : Cobol_common.Platform.TYPES.platform ->
  filename:string -> ?contents:string -> t -> bool

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

(** [save_config ~verbose project] dumps the current configuration of [project]
    into a TOML file.  Prints some informative message on [stderr] if [verbose]
    is set.

    The name of the configuration file is determined via the [layout] argument
    given to (the first call to) {!for_} or {!of_cache} with [project]'s root
    directory. *)
val save_config: ?verbose:bool -> t -> unit

(** [reload_config ~verbose project] reloads the configuration of [project] from
    its associated TOML file.  Returns [true] when the reloaded configuration
    does not match with the one before the call.  Prints some informative
    message on [stderr] if [verbose] is set. *)
val reload_config: ?verbose:bool -> t -> bool with_diags

(** {1 Collections} *)

val have_same_rootdirs: t -> t -> bool

module SET: sig
  include Set.S with type elt = t
  val for_: filename:string -> t -> elt
  val for_rootdir: rootdir:rootdir -> t -> elt
  val mem_rootdir: rootdir:rootdir -> t -> bool
end
module MAP: Map.S with type key = t

(** {1 Miscellaneous} *)

val string_of_rootdir: rootdir -> string
val relative_path_for: filename:string -> t -> string
val absolute_path_for: filename:string -> t -> string
