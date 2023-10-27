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

include module type of Types

module Options = Options
module Default = Default
module Diagnostics = Config_diagnostics

exception ERROR of Diagnostics.error

val print_options: Format.formatter -> unit

val default: (module T)

(** Search path (suspension).

    {e When evaluated} (via {!Lazy.force}), [default_search_path] is a path that
    is equivalent to {v ".:$XDG_CONFIG_HOME/superbol:$COB_CONFIG_DIR" v}.

    Changing environment variables or the current working directory has no
    impact on the resulting path when done {e after} evaluation.  *)
val default_search_path: string list Lazy.t

(** [from_file ~search_path filename] loads a configuration module from the
    given filename.  Note: the evaluation of {!default_search_path} is forces in
    case [search_path] is not provided. *)
val from_file
  : ?search_path: string list
  -> string
  -> (module T) Cobol_common.Diagnostics.with_diags

(** [from_dialect ~search_path dialect] returns the configuration module
    according to the dialect defaults.  The caveat about {!default_search_path}
    given for {!from_file} applies here as well. *)
val from_dialect
  : ?search_path: string list
  -> Types.DIALECT.t
  -> (module T) Cobol_common.Diagnostics.with_diags

val dialect: t -> dialect
