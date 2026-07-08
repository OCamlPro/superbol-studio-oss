(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.0, as described in the LICENSE.md file in the root  *)
(*  directory of this source tree.                                        *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

val parse_file : string -> M4Types.block
val parse_string : ?loc:M4Types.location -> string -> M4Types.block

(* remove one level of quotes (brackets). With `~last:true`, expand
   quadrigraphs *)
val unescape : ?last:bool -> string -> string

(* unescape with last=true. Must be done before using any string
   argument. Must not be done if argument is already escaped
   (typically in run-if-pass/run-if-fail) *)
val to_string : M4Types.arg -> string

val macro_error :
  M4Types.statement -> ('a, unit, string, 'b) format4 -> 'a
