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

open Ezcmd.V2

open Autofonce_config.Types

let cmd =
  let testsuite_args, get_testsuite_args = Testsuite.args () in
  let args =
    testsuite_args @
    [
    ]
  in
  EZCMD.sub
    "config"
    (fun () ->
       let (p, _tc, _suite) = Testsuite.find (get_testsuite_args ()) in
       Printf.printf "Project filename: %S\n" p.project_file ;
       Printf.printf "  Directories: %S\n" p.project_source_dir ;
       Printf.printf "    Source dir: %S\n" p.project_source_dir ;
       Printf.printf "    Build dir: %S\n" p.project_build_dir ;
       Printf.printf "    Run dir: %S\n" p.project_run_dir ;
       Option.iter (Printf.printf "  Project name: %S\n") p.project_name ;
       Printf.printf "  Testsuites:\n";
       List.iter (fun t ->
           Printf.printf "    name: %S\n" t.config_name ;
           Printf.printf "      file: %S\n" t.config_file ;
           Printf.printf "      path: %s\n"
             ( String.concat "\n            " t.config_path) ;
           Printf.printf "      env:\n";
           Printf.printf "        name: %S\n" t.config_env.env_name ;
           Printf.printf "        kind: %s\n" (match t.config_env.env_kind with
                 Env_file file -> file
               | Env_content -> "[...]");
         ) p.project_testsuites ;

       Printf.printf "%!";
    )
    ~args
    ~doc: "Print config of the current project"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P {|List the tests, with their numeric identifier, their name and their location in the testsuite files.|}
      ];
    ]
