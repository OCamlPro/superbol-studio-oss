(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

val string_of_node : ?config:Types.config ->
  ?format : Types.format ->
  ?context : Types.context ->
  Types.node -> string

val string_of_key_path : Types.key_path -> string

val string_of_location : Types.location -> string

val string_of_error : Types.error -> string

val edump : Types.node -> unit
