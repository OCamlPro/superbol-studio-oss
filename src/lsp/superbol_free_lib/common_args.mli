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
  config: (module Cobol_config.T);
  source_format: Cobol_config.source_format_spec;
  libpath: string list;
  parser_options: Cobol_parser.Options.parser_options;
}

val get : unit -> (unit -> t) * Ezcmd.V2.EZCMD.TYPES.arg_list
