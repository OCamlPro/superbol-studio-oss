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

open Types

(*  Imperative update *)
val get : node -> key_path -> node
val set : ?config:config -> node -> key_path -> value:node -> unit
val remove : ?config:config -> node -> key_path -> unit

(* Functional update *)
val update : ?config:config -> node -> key_path -> node option -> node


(* low-level value access *)



val type_of_node : node -> string

(* customizing nodes *)
val add_comments : node -> string list -> unit
val add_eol_comment : node -> string -> unit

(* to create generic nodes, use `V1.node` *)
val value : ?before:string list -> ?pos:int -> value -> node
val string : ?before:string list -> ?pos:int -> string -> node
val bool : ?before:string list -> ?pos:int -> bool -> node
val int : ?before:string list -> ?pos:int -> int -> node
val float : ?before:string list -> ?pos:int -> float -> node
val date : ?before:string list -> ?pos:int -> float -> node
val array : ?before:string list -> ?pos:int -> node array -> node
val table : ?before:string list -> ?pos:int -> table -> node
val empty_table : ?before:string list -> ?pos:int -> unit -> node
val table_of_list :
  ?before:string list -> ?pos:int -> ( string * node ) list -> node

val set_string :
  ?before:string list -> ?pos:int -> node -> key_path -> string -> unit
val set_bool :
  ?before:string list -> ?pos:int -> node -> key_path -> bool -> unit
val set_int :
  ?before:string list -> ?pos:int -> node -> key_path -> int -> unit
val set_float :
  ?before:string list -> ?pos:int -> node -> key_path -> float -> unit
val set_date :
  ?before:string list -> ?pos:int -> node -> key_path -> float -> unit
val set_array :
  ?before:string list -> ?pos:int -> node -> key_path -> node array -> unit
val set_table :
  ?before:string list -> ?pos:int -> node -> key_path -> table -> unit
val set_value :
  ?before:string list -> ?pos:int -> node -> key_path -> value -> unit
val set_table_of_list :
  ?before:string list -> ?pos:int -> node -> key_path -> ( string * node ) list -> unit

val set_strings :
  ?before:string list -> ?pos:int -> node -> key_path -> string array -> unit
val set_ints :
  ?before:string list -> ?pos:int -> node -> key_path -> int array -> unit

(* extractors *)

val extract_value : node -> value

val extract_string : node -> string
val extract_bool : node -> bool
val extract_int : node -> int
val extract_float : node -> float
val extract_date : node -> float
val extract_array : node -> node array
val extract_table : node -> table

val extract_ints : node -> int array
val extract_strings : node -> string array

val get_string : ?default:string -> node -> key_path -> string
val get_bool : ?default:bool -> node -> key_path -> bool
val get_int : ?default:int -> node -> key_path -> int
val get_float : ?default:float -> node -> key_path -> float
val get_date : ?default:float -> node -> key_path -> float
val get_array : ?default:node array -> node -> key_path -> node array
val get_table : ?default:table -> node -> key_path -> table
val get_value : ?default:value -> node -> key_path -> value

val get_strings : ?default:string array -> node -> key_path -> string array
val get_ints : ?default:int array -> node -> key_path -> int array

val table_iter : node -> ( string -> node -> unit ) -> unit
val array_iteri : node -> ( int -> node -> unit ) -> unit
val array_length : node -> int

(* Higher-level functions *)

(* returns the [pos] value for a section that would appear last *)
val next_section_pos : Types.node -> int

(* Add a section if it does not exist. Returns true if the section was added,
   false if it already existed. *)
val maybe_add_section : ?before:string list -> string -> Types.node -> bool

(* Set a key -> value if it is not already set *)
val maybe_set_value : ?before:string list ->
  Types.node -> Types.key_path -> Types.value -> bool

(* low-level value access *)

val type_of_value : value -> string
val set_node_value : node -> value -> unit
val get_node_value : node -> value

val value_of_string : string -> value
val value_of_int : int -> value
val value_of_bool : bool -> value
val value_of_float : float -> value
val value_of_date : float -> value
val value_of_array : node array -> value
val value_of_table : node EzCompat.StringMap.t -> value

val value_of_strings : string array -> value
val value_of_ints : int array -> value

val array_of_list : ?before:string list -> ?pos:int -> node list -> node
