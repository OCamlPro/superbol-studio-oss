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

module TYPES: sig

  type path =
    | RelativeToProjectRoot of string
    | RelativeToFileDir of string

  type rootdir

  type project = private {
    rootdir: rootdir;
    config_checksum: Digest.t option;
    cobol_config: Cobol_config.t;
    source_format: Cobol_config.source_format_spec;
    libpath: path list;
    copybook_extensions: string list;
    copybook_if_no_extension: bool;
  }

  type layout = {
    project_config_filename: string;
  }

end
include module type of TYPES
  with type path = TYPES.path
   and type rootdir = TYPES.rootdir
   and type project = TYPES.project
   and type layout = TYPES.layout

type t = project

(** [for_ ~rootdir ~layout] retrieves a project based on its root directory.
    This may trigger reading project configuration files if the project was not
    yet loaded.  Any notification about the loading process is published
    directly via {!Lsp_io.send_diagnostics} or {!Lsp_io.send_notification}. *)
val for_: rootdir:rootdir -> layout:layout -> t

(** [in_existing_dir dirname ~layout] retrieves a project after checking
    [dirname] actually refers to an exising directory that can serve as root for
    the project.  The same notes as {!for_} apply, with the addition that
    {!Invalid_argument} may is raised in case [dirname] is not the name of an
    existing directory. *)
val in_existing_dir: string -> layout:layout -> t

(** [rootdir_for ~uri ~layout] locates the project directory (that contains a
    file with given name [layout.project_config_filename]) for a file at the
    given URI.  Returns the name of the directory that contains the file at URI
    if no project file is found. *)
val rootdir_for: uri:Lsp.Uri.t -> layout:layout -> rootdir

(** [libpath_for ~uri project] constructs a list of directory names where
    copybooks are looked up, for a source file at the given URI, in the given
    project. *)
val libpath_for: uri:Lsp.Uri.t -> t -> string list

(** [detect_copybook ~uri project] indicates whether a document at the given URI
    for [project] should be treated as a copybook. *)
val detect_copybook: uri:Lsp.Uri.t -> t -> bool

(** Cached representation *)

type cached
val to_cache: t -> cached
val of_cache: rootdir:rootdir -> layout:layout -> cached -> t

(** Sets and maps *)

module SET: sig
  include Set.S with type elt = t
  val for_rootdir: rootdir:rootdir -> t -> elt
  val mem_rootdir: rootdir:rootdir -> t -> bool
end
module MAP: Map.S with type key = t

(** Miscellaneous *)

val rootdir: t -> rootdir
val string_of_rootdir: rootdir -> string
val relative_path_for: uri:Lsp.Uri.t -> t -> string
val absolute_path_for: filename:string -> t -> string
