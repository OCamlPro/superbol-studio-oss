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

open EzCompat (* for StringMap *)
open Ezcmd.V2
open EZCMD.TYPES
open Ez_file.V1
open EzFile.OP

module Misc = Autofonce_misc.Misc
module Project_config = Autofonce_config.Project_config

open Types

(*
  Read the `autofonce.toml` file. If it does not exist, read the
  `.autofonce`. If it does not exist, try to autodetect the project.
*)

let list_known_project () =
  Printf.printf "Known projects with environment files:\n";
  List.iter (fun file ->
      if Filename.check_suffix file ".toml" then
        Printf.printf "* %S\n%!" (Filename.chop_suffix file ".toml")
    ) Autofonce_share.Tree.file_list;
  Printf.printf "%!";
  exit 0

let autodetect_project () =
  let rec autodetect dirname =
    let basename = Filename.basename dirname in
    match Autofonce_share.Files.content ( basename ^ ".toml" ) with
    | content -> (basename, content)
    | exception Not_found ->
        let parent_dirname = Filename.dirname dirname in
        if parent_dirname = dirname then raise Not_found;
        autodetect parent_dirname
  in
  autodetect ( Sys.getcwd () )


let default_project_config project_file =
  let env = {
    env_name = "testsuite" ;
    env_kind = Env_content ;
    env_content = {|
# 'testsuite' env content
# Purpose it to translate AUTOFONCE_ variables to the
# variables expected by the project testsuite
|} ;
  } in
  let testsuite_config = {
    config_name = "testsuite" ;
    config_file = "tests/testsuite.at" ;
    config_env = env ;
    config_path = [ "tests/testsuite.src" ] ;
  } in
  {
    project_name = None ;
    project_source_anchors = [
      Autofonce_config.Globals.project_config_source ;
      Autofonce_config.Globals.project_config_build
    ] ;
    project_build_anchors = [
      Autofonce_config.Globals.project_config_source
    ] ;
    project_build_dir_candidates = [ "_build" ];
    project_testsuites = [ testsuite_config ] ;
    project_envs = StringMap.of_list [ env.env_name, env ];
    project_run_from = Build_dir ;
    project_captured_files = [];

    project_file = project_file ;
    project_source_dir = Filename.dirname project_file ;
    project_build_dir = Filename.dirname project_file ;
    project_run_dir = Filename.dirname project_file ;
  }

let cmd =
  let force_update = ref false in
  let project = ref None in
  let args =
    [
      [ "l"; "list-known" ], Arg.Unit list_known_project,
      EZCMD.info "List known projects with environment files";

      [ "p" ;"project" ], Arg.String (fun s -> project := Some s),
      EZCMD.info ~docv:"PROJECT" "Set project name to infer config";

      [ "f" ;"force-update" ], Arg.Set force_update,
      EZCMD.info "Force creation/update if file already exists";

    ]
  in
  EZCMD.sub
    "init"
    (fun () ->

       if Sys.file_exists Autofonce_config.Globals.project_config_build
       && not !force_update then
         Misc.error "File %S already exists in current dir. Use -f to force update"
           Autofonce_config.Globals.project_config_build;

       begin
         match Misc.find_file Autofonce_config.Globals.project_config_build with
         | exception Not_found -> ()
         | file ->
             if not !force_update then
               Misc.error
                 "Warning: %S already present in top dirs at\n %s\n   Use -f to create anyway.%!"
                 Autofonce_config.Globals.project_config_build file
       end;

       begin
         match Misc.find_file Autofonce_config.Globals.project_config_source with
         | exception Not_found -> ()
         | file ->
             Printf.eprintf
               "Warning: %S already present in top dirs at\n %s\n%!"
               Autofonce_config.Globals.project_config_source file;
       end;

       let file = Sys.getcwd () // "autofonce.toml" in
       let project =
         match
           match !project with
           | None
           | Some "auto" ->
               autodetect_project ()
           | Some project ->
               match Autofonce_share.Files.content ( project ^ ".toml" ) with
               | content -> (project, content)
               | exception Not_found ->
                   Misc.error "Project %S is not known" project
         with
         | (_project, content) ->
             Project_config.from_string ~computed:false ~file content
         | exception Not_found ->
             Printf.eprintf "Warning: autodetection of project failed\n%!";
             default_project_config file
       in
       Project_config.to_file project;
       Printf.eprintf "File %S created for project %s.\n%!"
         file (match project.project_name with
             | None -> "default"
             | Some name -> Printf.sprintf "%S" name);

    )
    ~args
    ~doc: "Initialize project to run the testsuite with autofonce"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P {|To run tests with $(b,autofonce), tests typically require
some environment variables to be set. For that, $(b,autofonce) uses a
file named $(b,autofonce.toml) in the project (or $(b,.autofonce)).
$(b,autofonce) will also use this file to create a directory $(b,_autofonce/)
where tests are run and results are kept.|} ;
        `P {|This command can be used to create the file $(b,autofonce.toml)
in the current directory. |} ;
        `P {|Yet, in some cases, $(b,autofonce) knows the project in which
you are and can provide you with an example of $(b,autofonce.toml) for that
particular project.|} ;
        `P {|You can use the following command to list known projects:|} ;
        `Pre {|\$ autofonce init --list|};
        `P {|You can then select the project using:|} ;
        `Pre {|\$ autofonce init -p gnucobol|};
        `P {|$(b,autofonce) will also inspect the path to see if it
recognize the name of a project it knows. In such cases, you won't need
to provide the project name, as it is automatically detected.|};
      ];
    ]
