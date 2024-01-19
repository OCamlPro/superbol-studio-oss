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

open EzCompat
open Ez_file.V1
open Ez_toml.V1
open TOML.Types

module TYPES = struct

  type toml_handle = {
    toml: node;                     (* warning: hides even more mutable stuff *)
    mutable checksum: Digest.t;
    mutable update_hooks: (string * (toml_handle -> bool)) list;
  }

  type section_option = {
    option_name: string;
    option_comments: string list;
    option_value: value;
  }

  type section = {
    section_name: string;
    section_comments: string list;
    section_options: section_option list;
  }

end

include TYPES

let checksum th = th.checksum
let toml th = th.toml

let add_update_hook toml name hook =
  toml.update_hooks <- toml.update_hooks @ [ name, hook ]

let option ~name ?(after_comments = []) value =
  { option_name = name;
    option_comments = after_comments;
    option_value = value }

let section ~name ?(after_comments = []) options =
  { section_name = name;
    section_comments = after_comments;
    section_options = options }

(* --- *)

let make_empty () =
  { toml = TOML.node (Table StringMap.empty);
    checksum = Digest.string "";
    update_hooks = [] }

let load ?(verbose = false) filename =
  if verbose then
    Pretty.error "Trying to load `%s'...@?" filename;

  try
    (* Use exception to check file existence while attempting to read. *)
    let string = EzFile.read_file filename in
    if verbose then Pretty.error " done@.";
    { toml = TOML.of_string ~file:filename string;
      checksum = Digest.string string;
      update_hooks = [] }
  with Sys_error _ when not (EzFile.exists filename) ->
    (* Only intercept and return empty if the file does not exist, and let every
       other case escape. *)
    if verbose then Pretty.error " not found@.";
    make_empty ()

let save ?(verbose = false) filename toml =
  if toml.update_hooks = [] then
    Pretty.error "@[<2>** Internal warning:@ asked@ to@ save@ using a@ TOML@ \
                  handle@ that@ has@ not@ been@ equipped@ with@ any@ update@ \
                  hook.@]@.";

  let modified =
    List.fold_left (fun modified (name, f) ->
        try f toml || modified
        with e ->
          Pretty.error "@[<2>Exception@ raised@ in@ update@ hook@ %s:@ %a@]@.\
                       " name Fmt.exn e;
          raise e
      ) false toml.update_hooks
  in

  if modified then
    let s = TOML.to_string toml.toml in
    File_utils.write_file ~mkdir:true filename s;
    if verbose then
      Pretty.error "Updated file `%s'@." filename;
    toml.checksum <- Digest.string s

(* --- *)

let update_section
    { section_name; section_comments; section_options }
    { toml; _ } =
  let modified =
    TOML.maybe_add_section section_name toml ~before:("" :: section_comments)
  in
  List.fold_left
    (fun modified { option_name; option_comments;
                    option_value } ->
      TOML.maybe_set_value toml [section_name; option_name] option_value
        ~before: ("" :: option_comments) || modified
    ) modified section_options

let add_section_update toml_handle section_name create_section =
  add_update_hook toml_handle
    (Printf.sprintf "[%s]" section_name)
    (fun toml ->
       (* create_section must be called everytime to compute the new
          section, so that it can be compared with the last one. *)
       update_section (create_section ~name:section_name) toml)

(* --- *)

type cacheable = toml_handle
let cacheable th = { th with update_hooks = [] }
