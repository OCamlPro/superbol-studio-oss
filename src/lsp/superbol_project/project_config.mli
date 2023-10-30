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

  type config = (* private *) {
    mutable cobol_config: Cobol_config.t;
    mutable source_format: Cobol_config.source_format_spec;
    mutable libpath: path list;
    mutable copybook_extensions: string list;
    mutable copybook_if_no_extension: bool;
    toml_handle: Ezr_toml.toml_handle;
  }

  exception ERROR of Project_diagnostics.error

end
include module type of TYPES
  with type path = TYPES.path
   and type config = TYPES.config

type t = config

val new_default: unit -> t

(** [load_file ~verbose config_filename] loads the given project configuration
    file.  Raises {!ERROR} or [Sys_error] in case of failure. *)
val load_file
  : ?verbose:bool
  -> string
  -> t Cobol_common.Diagnostics.with_diags

val save
  : ?verbose:bool
  -> config_filename:string
  -> t
  -> unit

(** [libpath_for ~filename project] constructs a list of directory names where
    copybooks are looked up, for a given source file name, in a project with the
    given configuration. *)
val libpath_for: filename:string -> t -> string list

(** [detect_copybook ~filename config] indicates whether a document with the
    given filename should be treated as a copybook in a project with
    configuration [config]. *)
val detect_copybook: filename:string -> t -> bool

(** Cached representation *)

exception BAD_CHECKSUM

type cached
val to_cache: t -> cached
val of_cache: config_filename:string -> cached -> t
