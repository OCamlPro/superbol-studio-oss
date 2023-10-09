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

open Ez_toml.V1
open TOML.Types

module TYPES : sig

  type user_config = {
    mutable toml : node ;
    mutable modified : bool ;
    mutable save_hooks : (string * (user_config -> unit)) list ;
  }

  type section_option = {
    option_name : string ;
    option_before : string list ;
    option_value : value ;
  }

  type section = {
    section_name : string ;
    section_before : string list ;
    section_options : section_option list ;
  }

end

open TYPES

val load : unit -> user_config
val save : user_config -> unit

val add_save_hook : user_config -> string -> (user_config -> unit) -> unit

val section :
  name:string ->
  ?before:string list -> TYPES.section_option list -> TYPES.section
val option :
  name:string ->
  ?before:string list -> Ez_toml.Types.value -> TYPES.section_option

val add_section_hook :
  TYPES.user_config -> string -> (name:string -> TYPES.section) -> unit
