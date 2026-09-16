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

val init : ?loc:M4Types.location -> Lexing.lexbuf -> unit

val unescape : ?last:bool -> Lexing.lexbuf -> string

val token : Lexing.lexbuf -> M4Types.location * M4Types.token

val location : Lexing.lexbuf -> M4Types.location
