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

open EzCompat
open Ez_file.V1
open EzFile.OP

open Types

module Misc = Autofonce_misc.Misc

let get_string ~prefix ~file table suffix =
  try
    EzToml.get_string table suffix
  with Not_found ->
    Misc.error "Missing key %s in file %s"
      ( String.concat "." ( prefix @ suffix ) ) file

let find_dir_by_anchor ?cwd anchors =
  let cwd = match cwd with
    | None -> Misc.getcwd ()
    | Some cwd -> cwd in

  let rec iter anchors =
    match anchors with
    | [] -> raise Not_found
    | anchor :: anchors ->
        if anchor = "!" then raise Exit else
          find_anchor cwd anchor anchors

  and find_anchor dir anchor anchors =
    let file = dir // anchor in
    if Sys.file_exists file then
      dir
    else
      let new_dir = Filename.dirname dir in
      if dir = new_dir then
        iter anchors
      else
        find_anchor new_dir anchor anchors
  in
  iter anchors

let parse_table
    ?force_build_dir ?force_source_dir
    ?(computed=true) ~file table =
  assert (not @@ Filename.is_relative file);
  let project_file = file in
  let file_dir = Filename.dirname file in
  let project_source_anchors =
    EzToml.get_string_list_default table [ "project" ; "source_anchors" ]
      [ Globals.project_config_source; Globals.project_config_build ]
  in
  let project_build_anchors =
    EzToml.get_string_list_default table [ "project" ; "build_anchors" ]
      [ Globals.project_config_build ]
  in
  let project_build_dir_candidates =
    EzToml.get_string_list_default table [ "project" ; "build_dir_candidates" ]
      [ "_build" ]
  in
  let project_source_dir =
    match force_source_dir with
    | Some dir ->
        if Filename.is_relative dir then
          Misc.error "Forced source dir must be absolute: %s\n%!" dir;
        dir
    | None ->
        try
          find_dir_by_anchor project_source_anchors
        with
          Not_found -> file_dir
        | Exit ->
            if computed then
              Misc.error
                "Could not locate mandatory source dir from project.source_anchors"
            else
              file_dir
  in
  let project_build_dir =
    match force_build_dir with
    | Some dir ->
        if Filename.is_relative dir then
          Misc.error "Forced build dir must be absolute: %s\n%!" dir;
        dir
    | None ->
        let rec iter ~fail candidates =
          match candidates with
          | [] ->
              if fail then
                if computed then
                  Misc.error
                    "Could not locate mandatory build dir from project.build_anchors"
                else
                  file_dir
              else
                file_dir
          | cwd :: candidates ->
              match
                find_dir_by_anchor ~cwd project_build_anchors
              with
              | exception Not_found -> iter ~fail:false candidates
              | exception Exit -> iter ~fail:true candidates
              | dir -> dir
        in
        iter ~fail:false
          ( Misc.getcwd () ::
            (List.map (fun s -> project_source_dir // s) project_build_dir_candidates))
  in
  let project_envs =
    let table = try
        EzToml.get_table table [ "envs" ]
      with Not_found ->
        Misc.error "[envs] not found in %s" file
    in
    let envs = ref StringMap.empty in
    EzToml.iter
      (fun env_name value ->
         let t =
           match value with
           | EzToml.TYPES.TString env_value ->
               let len = String.length env_value in
               if len > 0 && env_value.[0] = '<' then
                 let file = String.sub env_value 1 (len-1) in
                 let filename = project_source_dir // file in
                 let env_content = try
                     EzFile.read_text_file filename
                   with _ ->
                     Misc.error "envs.%s references unexistent file %s"
                       env_name file
                 in
                 { env_name ;
                   env_kind = Env_file file ;
                   env_content }
               else
                 { env_name ;
                   env_kind = Env_content ;
                   env_content = env_value
                 }
           | _ ->
               Misc.error "Wrong type for envs.%s" env_name
         in
         envs := StringMap.add t.env_name t !envs )
      table;
    !envs
  in

  let find_env ~config_name env =
    try
      StringMap.find env project_envs
    with
      Not_found ->
        Misc.error "testsuites.%s references unknown env %S"
          config_name env
  in

  let project_testsuites =
    let table = EzToml.get_table table [ "testsuites" ] in
    let testsuites = ref [] in
    EzToml.iter
      (fun config_name value ->
         let t =
           match value with
           | EzToml.TYPES.TString file ->
               {
                 config_name ;
                 config_file = file ;
                 config_path = [ Filename.dirname file // "testsuite.src" ];
                 config_env = find_env ~config_name "testsuite" ;
               }
           | EzToml.TYPES.TTable table ->
               let prefix = [ "testsuites" ; config_name ] in
               let config_file = get_string ~file ~prefix table [ "file" ] in
               let config_env = get_string ~file ~prefix table [ "env" ] in
               let config_path = EzToml.get_string_list_default
                   table [ "path" ] [] in
               {
                 config_name ;
                 config_file ;
                 config_path ;
                 config_env = find_env ~config_name config_env;
               }
           | _ ->
               Misc.error "Wrong type for testsuites.%s" config_name
         in
         testsuites := t :: !testsuites )
      table;
    !testsuites
  in
  let project_captured_files =
    EzToml.get_string_list_default table [ "project" ; "captured_files" ]
      []
  in

  let project_name = EzToml.get_string_option table
      [ "project" ; "name" ] in
  let project_run_from = EzToml.get_string_default table
      [ "project" ; "run_from" ] "build" in
  let project_run_from = match project_run_from with
    | "build" -> Build_dir
    | "source" -> Source_dir
    | "config" -> Config_dir
    | _ -> Misc.error
             "Wrong value %S for project.run_from, should be 'build', 'source' or 'config'"
             project_run_from
  in
  let project_run_dir =
    match project_run_from with
    | Build_dir -> project_build_dir
    | Source_dir -> project_source_dir
    | Config_dir -> Filename.dirname project_file
  in
  {
    project_name ;
    project_source_anchors ;
    project_run_from ;
    project_build_anchors ;
    project_build_dir_candidates ;
    project_testsuites ;
    project_envs ;
    project_captured_files ;

    (* computed *)
    project_file ;
    project_source_dir ;
    project_build_dir ;
    project_run_dir ;
  }

let from_file
    ?force_build_dir ?force_source_dir ?computed
    file =
  let table =
    match EzToml.from_file file with
    | `Ok table -> table
    | `Error (s, loc) ->
        Misc.error
          "Could not parse project config %S: %s at %s" file s
          (EzToml.string_of_location loc)
  in
  parse_table ?force_source_dir ?force_build_dir ?computed ~file table

let from_string
    ?force_build_dir ?force_source_dir
    ?computed ~file string =
  let table =
    match EzToml.from_string string with
    | `Ok table -> table
    | `Error (s, loc) ->
        Misc.error
          "Could not parse project config %S: %s at %s" file s
          (EzToml.string_of_location loc)
  in
  parse_table
    ?force_build_dir ?force_source_dir
    ?computed ~file table

let to_string p =
  let b = Buffer.create 10000 in

  Printf.bprintf b "[project]\n";
  Buffer.add_string b "# name to use to infer config\n";
  begin match p.project_name with
    | None ->
        Printf.bprintf b "# name = %S\n" "project-name" ;
    | Some name ->
        Printf.bprintf b "name = %S\n" name ;
  end;
  Buffer.add_char b '\n';

  Buffer.add_string b "# files used to locate the project top directory\n";
  Buffer.add_string b "#   and to set the AUTOFONCE_SOURCE_DIR\n";
  Printf.bprintf b "source_anchors = [ %s ]\n"
    ( String.concat ", "
        ( List.map (Printf.sprintf "%S") p.project_source_anchors )) ;
  Buffer.add_char b '\n';

  Buffer.add_string b "# files used to locate the project build directory\n";
  Buffer.add_string b "#   where the _autofonce/ directory will be created\n";
  Buffer.add_string b "#   and to set the AUTOFONCE_BUILD_DIR\n";
  Buffer.add_string b "#   use \"!\" to trigger an error if build dir is mandatory\n";
  Printf.bprintf b "build_anchors = [ %s ]\n"
    ( String.concat ", "
        ( List.map (Printf.sprintf "%S") p.project_build_anchors )) ;
  Buffer.add_char b '\n';

  Buffer.add_string b "# paths in project sources that are good candidates to\n";
  Buffer.add_string b "#   be tested as build dirs. Useful to run autofonce\n";
  Buffer.add_string b "#   from outside the build directory\n";
  Printf.bprintf b "build_dir_candidates = [ %s ]\n"
    ( String.concat ", "
        ( List.map (Printf.sprintf "%S") p.project_build_dir_candidates )) ;
  Buffer.add_char b '\n';

  Buffer.add_string b "# where the _autofonce/ dir should be created:\n";
  Buffer.add_string b "#   * 'build': in the build directory\n";
  Buffer.add_string b "#   * 'source': in the source directory\n";
  Buffer.add_string b "#   * 'config': in the directory of the config file\n";
  Printf.bprintf b "run_from = %S\n"
    ( match p.project_run_from with
      | Build_dir -> "build"
      | Source_dir -> "source"
      | Config_dir -> "config" ) ;
  Buffer.add_char b '\n';

  Printf.bprintf b "[testsuites]\n" ;
  Buffer.add_string b {|# alias = "path-from-topdir"|} ;
  Buffer.add_char b '\n' ;
  List.iter (fun t ->
      Printf.bprintf b "[testsuites.%s]\n" t.config_name ;
      Printf.bprintf b "file = %S\n" t.config_file ;
      Printf.bprintf b "path = [%s]\n"
        ( String.concat ", "
            ( List.map (Printf.sprintf " %S") t.config_path) ) ;
      Printf.bprintf b "env = %S\n" t.config_env.env_name ;
    ) p.project_testsuites ;
  Buffer.add_char b '\n';


  Printf.bprintf b "[envs]\n" ;
  Buffer.add_string b {|# env_name = """..."""|} ;
  Buffer.add_char b '\n' ;
  Buffer.add_string b {|# env_name = "<local-path-to-env-file"|} ;
  Buffer.add_char b '\n' ;
  StringMap.iter (fun env_name t ->
      match t.env_kind with
      | Env_file file ->
          Printf.bprintf b "%s = \"<%s\"\n" env_name file
      | Env_content ->
          Printf.bprintf b {|%s = """%s"""|} env_name t.env_content;
          Buffer.add_char b '\n';
    ) p.project_envs ;
  Buffer.add_char b '\n';

  Printf.bprintf b "[project]\n" ;
  Buffer.add_string b "# files to be captured into results.log\n";
  Buffer.add_string b "#   in case of test failure.\n";
  Printf.bprintf b "captured_files = [ %s ]\n"
    ( String.concat ", "
        ( List.map (Printf.sprintf "%S") p.project_captured_files )) ;
  Buffer.add_char b '\n';

  Buffer.contents b

let to_file p =
  EzFile.write_text_file p.project_file ( to_string p )
