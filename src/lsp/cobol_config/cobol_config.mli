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

val print_options: Format.formatter -> unit

val default: (module T)

val from_file
  : (module Cobol_common.Diagnostics.STATEFUL)
  -> ?dialect: Types.DIALECT.t
  -> string
  -> (module T)

(** [from_dialect (module Diags) ?strict dialect] returns the configuration
    module according to the dialect defaults. *)
val from_dialect
  : (module Cobol_common.Diagnostics.STATEFUL)
  -> strict: bool
  -> Types.DIALECT.t
  -> (module T)
