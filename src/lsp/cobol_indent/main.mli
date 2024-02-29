(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

val indent :
  source_format:Cobol_config.source_format_spec ->
  config: Types.unparsed_config ->
  dialect:'c ->
  filename:string ->
  ?verbose:bool ->
  ?output:string ->
  ?contents:string ->
  ?range:Types.range ->
  unit ->
  Types.indent_record list
