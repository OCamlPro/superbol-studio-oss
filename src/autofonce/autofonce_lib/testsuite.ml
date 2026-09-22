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

open EzCompat (* for IntMap *)
open Ezcmd.V2
open EZCMD.TYPES
open Ez_file.V1
open EzFile.OP

module MISC = Autofonce_misc.Misc
module PARSER = Autofonce_core.Parser
module CONFIG = Autofonce_config.Project_config
open Types

type args = {
  mutable arg_testsuite : string option ;
  mutable arg_testsuite_file : string option ;
  mutable arg_testsuite_env : string option ;  (* path to env file *)
  mutable arg_testsuite_path : string list ;
}

(* returns run_dir and project_config *)
let find_project_config () =
  let file =
    match MISC.find_file Autofonce_config.Globals.project_config_build with
    | exception Not_found ->
        begin
          match MISC.find_file Autofonce_config.Globals.project_config_source with
          | exception Not_found ->
              Printf.eprintf "Error: files %S or %S not found in top dirs\n%!"
                Autofonce_config.Globals.project_config_build
                Autofonce_config.Globals.project_config_source ;
              Printf.eprintf
                "  Use `autofonce init` to create a file %S.\n"
                Autofonce_config.Globals.project_config_build ;
              exit 2
          | file -> file
        end
    | file -> file
  in
  Autofonce_config.Project_config.from_file file

let read p tc =
  let testsuite_file = p.project_source_dir // tc.config_file in
  if not (Sys.file_exists testsuite_file) then
    MISC.error "Could not find testsuite file %S in project" testsuite_file ;
  let path = List.map (fun path ->
      p.project_source_dir // path
    ) tc.config_path in
  Printf.eprintf "Loading tests from file %S\n%!" testsuite_file ;
  let suite = PARSER.read ~path testsuite_file in
  p, tc, suite

let find args =

  begin
    if not ( Sys.file_exists ( MISC.getcwd () )) then
      MISC.error "Current directory does not exist anymore. Move back up.\n%!";
  end ;

  begin
    try
      let file = MISC.find_file "autofonce.env" in
      MISC.error
        "File %S found. This file is deprecated, remove it and run `autofonce init`" file
    with Not_found -> ()
  end;

  let p = find_project_config () in

  let p =
    match args.arg_testsuite_file with
    | None -> p
    | Some config_file ->
        let env = match args.arg_testsuite_env with
          | None ->
              {
                env_name = "";
                env_kind = Env_content ;
                env_content = "";
              }
          | Some testsuite_env ->
              {
                env_name = "";
                env_kind = Env_file testsuite_env ;
                env_content = EzFile.read_text_file testsuite_env ;
              }
        in
        let config_name = match args.arg_testsuite with
          | None -> ""
          | Some config_name -> config_name
        in
        let t = {
          config_name ;
          config_file ;
          config_path = List.rev args.arg_testsuite_path ;
          config_env = env ;
        } in
        { p with
          project_envs = StringMap.add "" env p.project_envs ;
          project_testsuites = t :: p.project_testsuites ;
        }
  in
  Printf.eprintf "Project description loaded from %s\n%!" p.project_file;
  let tc =
    match args.arg_testsuite with
    | None ->
        begin
          match p.project_testsuites with
          | [] -> MISC.error
                    "Project does not define any testsuite in %s!\n"
                    p.project_file
          | tc :: _ -> tc
        end
    | Some testsuite ->
        let rec iter testsuites =
          match testsuites with
          | [] ->
              MISC.error "Testsuite %S not found among testsuites in %s\n%!"
                testsuite p.project_file
          | tc :: testsuites ->
              if tc.config_name = testsuite then tc else
                iter testsuites
        in
        iter p.project_testsuites
  in
  read p tc

let exec ~filter_args ~exec_args p tc suite =
  MISC.set_signal_handle Sys.sigint (fun _ -> exit 2);
  MISC.set_signal_handle Sys.sigterm (fun _ -> exit 2);

  let state = Runner_common.create_state ~exec_args p tc suite in
  (* we are now in state_run_dir, i.e. before _autofonce/ *)

  let tests_dir = Autofonce_config.Globals.tests_dir in
  if exec_args.arg_clean_tests_dir && not filter_args.Filter.arg_filter &&
     Sys.file_exists tests_dir then
    MISC.remove_rec tests_dir ;

  if not ( Sys.file_exists tests_dir ) then begin
    Runner_common.output state "Creating testing directory %s\n%!"
      (MISC.getcwd () // tests_dir);
    Unix.mkdir tests_dir 0o755;
  end else begin
    Runner_common.output state "Using testing directory %s\n%!"
      (MISC.getcwd () // tests_dir);
  end;

  if state.state_args.arg_max_jobs = 1 then
    Runner_seq.exec_testsuite ~filter_args state
  else
    Runner_par.exec_testsuite ~filter_args state;

  Terminal.move_bol ();
  Printf.printf "Results:\n%!"; Terminal.erase Eol;
  Terminal.printf [] "* %d checks performed\n%!" state.state_nchecks ;
  let style =
    if state.state_tests_failed <> [] then [ Terminal.red ]
    else [ Terminal.green ]
  in
  Terminal.printf style
    "* %d / %d tests executed successfully\n%!"
    state.state_ntests_ok state.state_ntests_ran ;
  begin match state.state_tests_failed with
    | [] -> ()
    | list ->
        let nb = List.length list in
        Terminal.printf [ Terminal.red ] "* %d tests failed:" nb;
        Runner_common.print_ntests 10 (List.rev list);
        Printf.printf "\n%!";
  end;
  begin match state.state_tests_skipped with
    | [] -> ()
    | list ->
        let nb = List.length list in
        Terminal.printf [ Terminal.magenta ]
          "* %d tests were skipped\n%!" nb;
  end;
  begin match state.state_tests_failexpected with
    | [] -> ()
    | list ->
        let nb = List.length list in
        Terminal.printf [ Terminal.magenta ]
          "* %d tests were expected to fail:%!" nb;
        Runner_common.print_ntests 5 (List.rev list);
        Printf.printf "\n%!";
  end;
  if state.state_args.arg_print_all then begin
    let buffer = Buffer.contents state.state_buffer in
    Terminal.printf [ Terminal.magenta ] "%s\n%!" buffer;
  end ;
  Logging.log_state_buffer state ;
  List.length state.state_tests_failed

let print_test _c t =
  Printf.printf "%04d %-50s %s\n%!" t.test_id t.test_name
    ( PARSER.name_of_loc t.test_loc );
  ()

let print ~filter_args c =
  let current_banner = ref "" in
  Filter.select_tests  ~args:filter_args
    (fun t ->
       if t.test_banner <> !current_banner then begin
         Printf.eprintf "\n%s\n\n%!" t.test_banner;
         current_banner := t.test_banner
       end;
       print_test c t;
    ) c

let args () =
  let args = {
    arg_testsuite = None ;
    arg_testsuite_file = None ;
    arg_testsuite_env = None ;
    arg_testsuite_path = [] ;
  } in
  let get_args () = args in
  [
    [ "t" ; "testsuite" ], Arg.String (fun s ->
        args.arg_testsuite <- Some s),
    EZCMD.info
      ~env:(EZCMD.env "AUTOFONCE_TESTSUITE")
      ~docv:"TESTSUITE" "Name of the testsuite to run (as specified in 'autofonce.toml')";

    [ "T" ; "at" ], Arg.String (fun s -> args.arg_testsuite_file <- Some s),
    EZCMD.info
      ~docv:"TESTSUITE.at" "Path of the file containing the testsuite" ;

    [ "E" ; "env" ], Arg.String (fun s -> args.arg_testsuite_env <- Some s),
    EZCMD.info
      ~docv:"TESTSUITE.sh" "Env file for all tests" ;

    [ "I" ], Arg.String (fun s ->
        args.arg_testsuite_path <- s :: args.arg_testsuite_path),
    EZCMD.info
      ~docv:"DIR" "Add DIR to search path for tests" ;

  ],
  get_args
