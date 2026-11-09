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

(** This library is used to build configuration modules, either from file or
    from a dialect.  All the [from_] functions will fail if a file is not found,
    or use the default value of any options that is badly typed in the
    configuration file or not set in the configuration file.*)

module Types = Types
include module type of Types

module Options = Options
module Default = Default
module Diagnostics = Config_diagnostics

exception ERROR of Diagnostics.error

val print_options: Format.formatter -> unit

val default: (module T)

(** [default_search_path ~platform] is where configuration files are looked
    up after any search path given to the functions below: the current working
    directory, [$COB_CONFIG_DIR], [$XDG_CONFIG_HOME/superbol/gnucobol-config],
    some system-specific directories, and [$COB_CONFIG_DIR_FALLBACK], as
    [platform] gives them.

    The functions below access the file system and the environment through
    [platform] only, so that what they read may be recorded. *)
val default_search_path
  : platform: Cobol_common.Platform.TYPES.platform
  -> string list

(** [from_file ~platform ~search_path filename] loads a configuration module
    from [filename], looked up in its directory, then [search_path], then
    {!default_search_path}.  Files it includes are looked up likewise, starting
    with the directory of the file found. *)
val from_file
  : platform: Cobol_common.Platform.TYPES.platform
  -> ?search_path: string list
  -> ?verbose:bool
  -> string
  -> (module T) Cobol_common.Diagnostics.with_diags

(** [from_dialect ~platform ~search_path dialect] returns the configuration
    module according to the dialect defaults, looking its file up as
    {!from_file} does. *)
val from_dialect
  : platform: Cobol_common.Platform.TYPES.platform
  -> ?search_path: string list
  -> ?verbose:bool
  -> Types.DIALECT.t
  -> (module T) Cobol_common.Diagnostics.with_diags

(** [from_dialect_n_file ~platform ~search_path dialect] is like
    {!from_dialect}, also giving the configuration file loaded ([None] for
    {!DIALECT.Default}). *)
val from_dialect_n_file
  : platform: Cobol_common.Platform.TYPES.platform
  -> ?search_path: string list
  -> ?verbose:bool
  -> Types.DIALECT.t
  -> ((module T) * string option) Cobol_common.Diagnostics.with_diags

val dialect: t -> dialect

val tab_stops: t -> int list
