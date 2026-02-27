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

type t = {
  platform : Cobol_common.Platform.TYPES.platform;
  preproc_options: Cobol_preproc.Options.preproc_options;
  parser_options: Cobol_parser.Options.parser_options;
  pretty_verbose: 'a. 'a Pretty.proc;
}

(** [verbose_on] specifies where formatted output via [pretty_verbose] goes.
    The default behavior is [stdout].  Use [`Stdnul] to ignore such outputs. *)
val get : ?verbose_on:[`Stderr | `Stdout | `Stdnul] -> unit ->
  (unit -> t) * Ezcmd.V2.EZCMD.TYPES.arg_list
