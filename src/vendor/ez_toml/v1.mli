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

module TOML : sig

  module Types = Types
  open Types

  val default_config : config

  (* Do not error on setting a value that already exists *)
  val override_config : config

  val of_string : ?file:string -> ?config:config -> string -> node
  val of_file : ?config:config -> string -> node

  val to_string : node -> string
  val to_file : node -> string -> unit

  (* useful to build values *)

  val noloc : location
  val node : ?format:Types.format ->
    ?loc:location -> ?before:string list ->
    ?name:key_path -> ?after:string -> ?pos:int ->
    value -> node

  include ( module type of Internal_printer )
  include ( module type of Internal_accessors )

end


module EZTOML : sig

  module TYPES: sig

    type toml_file

    type 'value section_option = {
      option_name: string;
      option_comments: string list;
      option_getter: ('value -> TOML.Types.value);
      option_setter: ('value -> TOML.Types.node -> unit);
    }

    type 'value section = {
      section_name: string;
      section_comments: string list;
      section_options: 'value section_option list;
    }

  end

  open TYPES

  (** {1 Initialization & Accessor} *)

  (** Creates a new handle with an empty TOML table. *)
  val make_empty: unit -> toml_file

  (** Access to the underlying TOML node.

      Please handle with care: any mutation of the returned node leads to
      undefined behaviors (mutations of this node should only happen via update
      hooks). *)
  val toml : toml_file -> TOML.Types.node

  val checksum : toml_file -> string

  (** {1 Section definitions} *)

  (** Combinator to define sections. *)
  val section :
    name: string
    -> ?comments: string list
    -> 'value section_option list -> 'value section

  (** Combinator to define section options. *)
  val option :
    ?comments: string list
    -> getter:('value -> TOML.Types.value)
    -> setter:('value -> TOML.Types.node -> unit)
    -> string
    -> 'value section_option

  (** [add_section_update handle section_name create_section] equips [handle] with
      an update hook that calls [create_section ~name] upon each call to {!save},
      to determine whether the TOML representation needs to be updated. *)
  val add_section:
    toml_file ->
    'value ->
    'value section ->
    unit

  (** {1 Input/output} *)

  (** [load ~verbose filename] load and returns a handle for the TOML file
      [filename].  Returns an empty TOML if [filename] does not exist.  Raises
      [Sys_error] if the file exists but is not readable. *)
  val load: ?verbose: bool -> string -> toml_file

  (** [save ~verbose filename handle] triggers update hooks (that update the TOML
      representation), and save it in [filename] if the representation has been
      modified. *)
  val save: ?verbose: bool -> string -> toml_file -> unit

  val option_string :
    ?comments:string list ->
    string ->
    getter:('a -> string) ->
    setter:('a -> string -> unit) -> 'a section_option

  (* None will be mapped to the empty string *)
  val option_string_option :
    ?comments:string list ->
    string ->
    getter:('a -> string option) ->
    setter:('a -> string option -> unit) -> 'a section_option

  val option_string_map :
    ?comments:string list ->
    string ->
    getter:('a -> string EzCompat.StringMap.t) ->
    setter:('a -> string EzCompat.StringMap.t -> unit) -> 'a section_option

  val option_int :
    ?comments:string list ->
    string ->
    getter:('a -> int) ->
    setter:('a -> int -> unit) -> 'a section_option

  val option_bool :
    ?comments:string list ->
    string ->
    getter:('a -> bool) ->
    setter:('a -> bool -> unit) -> 'a section_option

end
