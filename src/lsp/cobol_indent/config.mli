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

val source_format :
  Cobol_common.Config.TYPES.source_format_spec -> Types.source_format


(* [load ~source_format filename] tries to load ".superbol-indent" in
   all directories upper than [filename], merging their contents and,
   maybe, overriding ~source_format.

   Syntax of .superbol-indent:

   * # on first char for indent
   * format = fixed
   * arg-offset = 3
   * inner-offset = 3

 *)
val load :
  source_format:Types.source_format ->
  filename:string -> Types.config

val to_string : Types.config -> string

val generate : ?config:Types.config -> ?only_comment:bool -> string -> unit
