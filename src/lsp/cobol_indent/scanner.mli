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

type result = {
  toks : (Types.token * Types.token_descr) list ;
  revedits : Types.indent_record list (* in reverse order *) ;
  skipped_revlines : int list; (* only when not config.scan_for_indent *)
}

val tokenize :
  config:Types.config ->
  filename:string ->
  contents:string -> result
