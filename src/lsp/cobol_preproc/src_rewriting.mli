(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

val expand_tabs: ?tab_stop:int -> ?starting_col:int -> string -> string

val from_channel_expanding_tabs:
  ?with_positions:bool ->
  ?tab_stop:int ->
  in_channel ->
  Lexing.lexbuf

