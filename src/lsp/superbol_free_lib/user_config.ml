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
open EzFile.OP

module TYPES = struct

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

let set_section section user_config =
  if TOML.maybe_add_section ~before:( "" :: section.section_before )
      section.section_name user_config.toml then
    user_config.modified <- true ;

  List.iter (fun option ->
      if TOML.maybe_set_value ~before: ( "" :: option.option_before )
          [ section.section_name ; option.option_name ]
          user_config.toml
        @@ option.option_value then
        user_config.modified <- true ;
    ) section.section_options ;

  ()


let user_config_file = Misc.config_dir // "config.toml"

let load () =
  if Sys.file_exists user_config_file then
    let s = EzFile.read_file user_config_file in
    let toml = TOML.of_string ~file:user_config_file s in
    {
      toml ;
      modified = false ;
      save_hooks = [];
    }
  else
    {
      toml = TOML.node (Table StringMap.empty) ;
      modified = true ;
      save_hooks = [] ;
    }

let save t =
  List.iter (fun (name,f) ->
      try f t
      with exn ->
        Printf.eprintf "Save hook %s raise exception\n%!" name;
        raise exn
    ) t.save_hooks;
  if t.modified then
    let s = TOML.to_string t.toml in
    Misc.write_file ~mkdir:true user_config_file s;
    Printf.eprintf "User config updated in %s\n%!" user_config_file;
    t.modified <- false



let add_save_hook config name hook =
  config.save_hooks <- config.save_hooks @ [ name, hook ]

let option ~name ?(before=[]) value =
  {
    option_name = name ;
    option_before = before ;
    option_value = value }

let section ~name ?(before=[]) options =
  { section_name = name ;
    section_before = before ;
    section_options = options }

let add_section_hook user_config section_name create_section =
  add_save_hook user_config
    (Printf.sprintf "[%s]" section_name)
    (fun user_config ->
       let section = create_section ~name:section_name in
       set_section section user_config)
