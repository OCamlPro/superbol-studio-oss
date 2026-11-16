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

val name_of_loc : Types.location -> string

val m4_escape : ?can_quote:bool -> string -> string

(* Read filename to get the corresponding testsuite *)
val read : ?path:string list -> string -> Types.suite
