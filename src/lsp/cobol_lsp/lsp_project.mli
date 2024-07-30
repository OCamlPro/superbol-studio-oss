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

(** {1 Type definitions} *)

module TYPES: sig
  include module type of Superbol_project.Config.TYPES
  include module type of Superbol_project.TYPES
end
include module type of TYPES
  with type path = TYPES.path
   and type rootdir = TYPES.rootdir
   and type project = TYPES.project
   and type layout = TYPES.layout

type t = project

(** {1 Constructors & accessors} *)

(** [for_ ~rootdir ~layout] retrieves a project based on its root directory.
    This may trigger reading project configuration files if the project was not
    yet loaded.

    May puplish notifications about the loading process directly via
    {!Lsp_io.send_diagnostics} or {!Lsp_io.send_notification}, and send
    diagnostics about loaded configuration files via
    {!Lsp_diagnostics.publish}. *)
val for_: rootdir:rootdir -> layout:layout -> t

(** [in_existing_dir dirname ~layout] retrieves a project after checking
    [dirname] actually refers to an exising directory that can serve as root for
    the project.

    The same notes as for {!for_} apply, with the addition that
    [Invalid_argument] is raised in case [dirname] is not the name of an
    existing directory.  *)
val in_existing_dir: string -> layout:layout -> t

(** [rootdir_for ~uri ~layout] locates the project directory (that contains a
    file with given name [layout.project_config_filename]) for a file at the
    given URI.  The behavior when no such file is found is that of
    {!Superbol_project.rootdir_for}. *)
val rootdir_for: uri:Lsp.Uri.t -> layout:layout -> rootdir

(** [copybook_lookup_config_for ~uri project] constructs a copybook lookup
    configuration for a source file at the given URI, in the given project. *)
val copybook_lookup_config_for
  : uri:Lsp.Uri.t -> t -> Cobol_common.Copybook.lookup_config

(** [detect_copybook ~uri project] indicates whether a document at the given URI
    for [project] should be treated as a copybook. *)
val detect_copybook: uri:Lsp.Uri.t -> t -> bool

(** {1 Cached representation} *)

type cached

(** [to_cache project] constructs a cached representation for [project]. *)
val to_cache: t -> cached

(** [of_cache ~rootdir ~layout cached_project] attempts to load and return a
    cached project.  Behaves like [for_ ~rootdir ~layout] in case of error
    (outdated or missing configuration file). *)
val of_cache: rootdir:rootdir -> layout:layout -> cached -> t

(** {1 Collections} *)

module SET: sig
  include Set.S with type elt = t
  val for_: uri:Lsp.Uri.t -> t -> elt
  val for_rootdir: rootdir:rootdir -> t -> elt
  val mem_rootdir: rootdir:rootdir -> t -> bool
end
module MAP: Map.S with type key = t

(** {1 Configuration management}

    {e Warning}: functions below that return a Boolean actually perform some
    mutations on project's configurations. They return [true] if the
    configuration of the given project has changed. *)

val reload_project_config: t -> bool
val update_project_config: (string * Yojson.Safe.t) list -> t -> bool
val get_project_config: ?flat:bool -> t -> Yojson.Safe.t

(** {1 Miscellaneous} *)

val rootdir: t -> rootdir
val rooturi: t -> Lsp.Uri.t
val config: t -> Superbol_project.Config.t
val string_of_rootdir: rootdir -> string
val relative_path_for: uri:Lsp.Uri.t -> t -> string
val absolute_path_for: filename:string -> t -> string
