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

(** Slightly abstract API to save and load TOML files. *)

open Ez_toml.V1

module TYPES: sig

  type toml_handle

  type section_option = {
    option_name: string;
    option_comments: string list;
    option_value: TOML.Types.value;
  }

  type section = {
    section_name: string;
    section_comments: string list;
    section_options: section_option list;
  }

end

include module type of TYPES
  with type toml_handle = TYPES.toml_handle
   and type section_option = TYPES.section_option
   and type section = TYPES.section

(** {1 Initialization & Accessor} *)

(** Creates a new handle with an empty TOML table. *)
val make_empty: unit -> toml_handle

(** Access to the underlying TOML node.

    Please handle with care: any mutation of the returned node leads to
    undefined behaviors (mutations of this node should only happen via update
    hooks). *)
val toml: toml_handle -> TOML.Types.node

(** {1 Section definitions} *)

(** Combinator to define sections. *)
val section
  : name: string
  -> ?after_comments: string list
  -> section_option list -> section

(** Combinator to define section options. *)
val option
  : name: string
  -> ?after_comments: string list
  -> TOML.Types.value -> section_option

(** [add_section_update handle section_name create_section] equips [handle] with
    an update hook that calls [create_section ~name] upon each call to {!save},
    to determine whether the TOML representation needs to be updated. *)
val add_section_update: toml_handle -> string -> (name: string -> section) -> unit

(** {1 Input/output} *)

(** [load ~verbose filename] load and returns a handle for the TOML file
    [filename].  Returns an empty TOML if [filename] does not exist.  Raises
    {!Sys_error} if the file exists but is not readavble. *)
val load: ?verbose: bool -> string -> toml_handle

(** [save ~verbose filename handle] triggers update hooks (that update the TOML
    representation), and save it in [filename] if the representation has been
    modified. *)
val save: ?verbose: bool -> string -> toml_handle -> unit

(** {1 Caching} *)

(** For the purposes of caching, we reuse the same representation but remove
    update hooks (that are functional values).

    Therefore, these need to be reinstated after loading a handle, using
    appropriate calls to {!add_section_update}. *)

type cacheable = toml_handle                     (* same representation... *)
val cacheable: toml_handle -> cacheable           (* just removes update hooks *)

val checksum: toml_handle -> Digest.t
