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

open Ez_call.V1
open EzCompat (* for IntMap *)
open Ez_file.V1
open EzFile.OP
open Ezcmd.V2
open EZCMD.TYPES

open Types

module PARSER = Autofonce_core.Parser
module MISC = Autofonce_misc.Misc
module CONFIG = Autofonce_config.Project_config

let args () =
  let args =
    {
      arg_clean_tests_dir = true ;
      arg_max_jobs = 16 ;
      arg_ignore_exitcode = false;
      arg_print_results = false ;
      arg_subst_env = StringMap.empty ;
      arg_stop_on_first_failure = false ;
      arg_print_all = false ;
      arg_keep_skipped = false ;
      arg_keep_all = false ;
      arg_output = None ;
    }
  in
  [
    [ "print-all" ], Arg.Unit (fun () ->
        args.arg_print_all <- true),
    EZCMD.info "Print also expected failures";

    [ "e" ; "stop-on-failure" ], Arg.Unit (fun () ->
        args.arg_stop_on_first_failure <- true),
    EZCMD.info "Stop on first failure";

    [ "j" ], Arg.Int (fun n -> args.arg_max_jobs <- max 1 n),
    EZCMD.info ~docv:"NJOBS" "Set maximal parallelism";

    [ "1" ; "j1" ], Arg.Unit (fun () -> args.arg_max_jobs <- 1),
    EZCMD.info "Use Sequential scheduling of tests";

    [ "l" ; "print-seq" ], Arg.Unit (fun () -> args.arg_print_results <- true),
    EZCMD.info
      "Print results immediately (default is to print a summary at the end)";

    [ "s" ; "keep-more" ], Arg.Unit (fun () -> args.arg_keep_skipped <- true),
    EZCMD.info "Keep directories of skipped and expected failed";

    [ "S" ; "keep-all" ], Arg.Unit (fun () -> args.arg_keep_all <- true),
    EZCMD.info "Keep all directories of tests";

    [ "no-clean" ], Arg.Unit (fun () -> args.arg_clean_tests_dir <- false),
    EZCMD.info "Do not clean _autofonce/ dir on startup";

    [ "ignore-exitcode" ], Arg.Unit (fun () -> args.arg_ignore_exitcode <- true),
    EZCMD.info "Do not promote or fail for wrong exit code" ;

    [ "o" ; "output" ], Arg.String (fun s -> args.arg_output <- Some s),
    EZCMD.info
      ~env:(EZCMD.env "AUTOFONCE_OUTPUT")
      ~docv:"TESTSUITE" "Path of the output file (default: _autofonce/results.log)";

  ] ,
  args

let status_len = 30
let spaces = String.make 80 ' '

let test_status ter fmt =
  let t = ter.tester_test in
  Printf.kprintf (fun s ->
      let len = String.length s in
      let status_len = if len > status_len then len else status_len in
      let test_id = Printf.sprintf "%04d" t.test_id in
      let id_len = String.length test_id in
      let max_name_len = 79 - 2 - status_len - id_len in
      let max_name_len = if max_name_len < 0 then 0 else max_name_len in
      let len = String.length t.test_name in
      let test_name =
        if len > max_name_len then
          String.sub t.test_name 0 max_name_len
        else
          t.test_name ^ String.sub spaces 0 (max_name_len - len)
      in
      Printf.sprintf "%s %s %s" test_id test_name s
    ) fmt

let buffer_test state test_status =
  if state.state_args.arg_print_results then begin
    if Terminal.isatty then begin
      Terminal.move_bol ();
      Terminal.erase Eol;
      Terminal.printf [] "%s\n" test_status
    end else
      Printf.eprintf "%s\n" test_status ;
  end ;
  Printf.bprintf state.state_buffer "%s\n" test_status

let commented s =
  "# " ^ String.concat "\n# " (EzString.split s '\n')

let output state fmt =
  Printf.kprintf (fun s ->
      Terminal.move_bol ();
      Terminal.erase Eol;
      Terminal.printf [] "%s\n%!" s;
      state.state_status_printed <- false
    ) fmt

let test_dir t =
  Autofonce_config.Globals.tests_dir // Printf.sprintf "%04d" t.test_id
let tester_dir ter = test_dir ter.tester_test

let test_is_ok ter =
  let test = ter.tester_test in
  let state = ter.tester_state in
  if not state.state_args.arg_keep_all then
    MISC.remove_rec ( tester_dir ter ) ;
  state.state_ntests_ok <- state.state_ntests_ok + 1;
  buffer_test state (
    test_status ter "OK (%s)"
      ( PARSER.name_of_loc test.test_loc )
  )

let test_is_skipped_fail cer s =
  let ter = cer.checker_tester in
  let state = ter.tester_state in
  let check = cer.checker_check in
  if not state.state_args.arg_keep_skipped then
    MISC.remove_rec ( tester_dir ter ) ;
  state.state_tests_failexpected <- ter :: state.state_tests_failexpected ;
  buffer_test state (
    test_status ter "SKIPPED FAIL (%s %s)"
      ( PARSER.name_of_loc check.check_loc ) s
  )

let test_is_failed loc ter ?check s =
  let state = ter.tester_state in
  ter.tester_fail_reason <- Some ( loc, s, check );
  if ter.tester_fail_expected then begin
    state.state_tests_failexpected <- ter :: state.state_tests_failexpected ;
    if not state.state_args.arg_keep_skipped then
      MISC.remove_rec ( tester_dir ter ) ;
    buffer_test state
      ( test_status ter "EXPECTED FAIL (%s %s)"
          ( PARSER.name_of_loc loc ) s )
  end else begin
    state.state_tests_failed <- ter :: state.state_tests_failed ;
    let status =
      test_status ter "FAIL (%s %s)" ( PARSER.name_of_loc loc ) s
    in
    output state "%s" status;
    buffer_test state status;
    if state.state_args.arg_stop_on_first_failure then exit 2;
  end

let test_is_skip ter =
  let t = ter.tester_test in
  let state = ter.tester_state in
  state.state_tests_skipped <- ter :: state.state_tests_skipped ;
  if not state.state_args.arg_keep_skipped then MISC.remove_rec ( tester_dir ter ) ;
  buffer_test state
    (test_status ter "SKIP (%s)" ( PARSER.name_of_loc t.test_loc ) )

let exec_action_no_check ter action =
  match action with
  | AT_DATA { file ; content } ->
      EzFile.write_text_file ( tester_dir ter // file ) content
  | AT_CLEANUP _ -> ()
  | AF_COMMENT _ -> ()
  | AT_XFAIL -> ter.tester_fail_expected <- true
  | AF_ENV env -> ter.tester_renvs <- env :: ter.tester_renvs
  | AT_CAPTURE_FILE file ->
      ter.tester_captured_files <- StringSet.add file ter.tester_captured_files
  | AT_SKIP
  | AT_FAIL _
  | AT_XFAIL_IF _
  | AT_SKIP_IF _
  | AT_FAIL_IF _
  | AT_CHECK _
  | AF_COPY _
    ->
      Printf.kprintf failwith "exec_action: %s not implemented"
        ( string_of_action action )

let check_of_AT_XFAIL_IF ter check_step check_loc check_command =
  {
    check_step ;
    check_kind = "XFAILIF" ;
    check_command ;
    check_loc ;
    check_retcode = Some 1 ;
    check_stdout = Ignore ;
    check_stderr = Ignore ;
    check_test = ter.tester_test;
    check_run_if_pass = [] ;
    check_run_if_fail = [ AT_XFAIL ] ;
  }

let check_of_AT_SKIP_IF ter check_step check_loc check_command =
  {
    check_step ;
    check_kind = "SKIPIF" ;
    check_command ;
    check_loc ;
    check_retcode = Some 1 ;
    check_stdout = Ignore ;
    check_stderr = Ignore ;
    check_test = ter.tester_test;
    check_run_if_pass = [] ;
    check_run_if_fail = [ AT_SKIP ] ;
  }

let check_of_AT_FAIL_IF ter check_step check_loc check_command =
  {
    check_step ;
    check_kind = "FAILIF" ;
    check_command ;
    check_loc ;
    check_retcode = Some 1 ;
    check_stdout = Ignore ;
    check_stderr = Ignore ;
    check_test = ter.tester_test;
    check_run_if_pass = [] ;
    check_run_if_fail = [ AT_FAIL { loc = check_loc } ] ;
  }


let check_of_at_file ter ~copy check_step check_loc check_command =
  {
    check_step ;
    check_kind = if copy then "COPY" else "LINK";
    check_command ;
    check_loc ;
    check_retcode = Some 0 ;
    check_stdout = Ignore ;
    check_stderr = Ignore ;
    check_test = ter.tester_test;
    check_run_if_pass = [] ;
    check_run_if_fail = [] ;
  }


let print_ntests n list =
  List.iteri (fun i ter ->
      if i < n then Printf.printf " %04d" ter.tester_test.test_id
      else
      if i = n then Printf.printf ".."
    ) list

(* header: 12
   fails: 8 + 4 + 5*3 + 2 = 29
   status reste 40
*)
let print_status state =
  if Terminal.isatty && not state.state_status_printed then begin
    Terminal.move_bol ();
    Terminal.erase Eol;
    Terminal.printf [] " %d / %d" state.state_ntests_ran state.state_ntests;
    if state.state_tests_failed <> [] then begin
      Terminal.printf [ Terminal.red ]
        " %d fails:%!" (List.length state.state_tests_failed);
      print_ntests 3 state.state_tests_failed;
    end;
    Terminal.printf [] " %s" state.state_status;
    Printf.printf "%!";
    state.state_status_printed <- true
  end

let start_test state t =
  let c = state.state_suite in
  state.state_ntests_ran <- state.state_ntests_ran + 1;
  state.state_status_printed <- false ;
  print_status state;
  let ter = {
    tester_test = t ;
    tester_state = state ;
    tester_suite = c ;
    tester_renvs = [] ;
    tester_fail_expected = false ;
    tester_captured_files = StringSet.empty ;
    tester_fail_reason = None ;
  }
  in
  let test_dir = tester_dir ter in
  if Sys.file_exists test_dir then
    MISC.remove_all test_dir
  else
    Unix.mkdir test_dir 0o755;

  EzFile.write_text_file ( test_dir // Autofonce_config.Globals.env_autofonce_sh ) @@
  Printf.sprintf {|#!/bin/sh
# name of testsuite in 'autofonce.toml'
AUTOFONCE_TESTSUITE="%s"
# location of directory containing _autofonce/ dir
AUTOFONCE_RUN_DIR="%s"
# project source directory
AUTOFONCE_SOURCE_DIR="%s"
# build directory of project
AUTOFONCE_BUILD_DIR="%s"

# test env by autofonce.toml
%s

# test env by AF_ENV
%s
|}
    state.state_config.config_name
    state.state_run_dir
    state.state_project.project_source_dir
    state.state_project.project_build_dir
    state.state_config.config_env.env_content
    t.test_env
  ;
  Unix.chmod ( test_dir // Autofonce_config.Globals.env_autofonce_sh ) 0o755;
  ter

let check_dir check = test_dir check.check_test
let check_prefix check =
  Printf.sprintf "%s_%s" check.check_step check.check_kind

let start_check ter check =
  let state = ter.tester_state in
  let check_prefix = check_prefix check in
  let check_sh = Printf.sprintf "%s.sh" check_prefix in
  let check_stdout = Printf.sprintf "%s.out" check_prefix in
  let check_stderr = Printf.sprintf "%s.err" check_prefix in
  let check_content =
    Printf.sprintf {|#!/bin/sh

# create test env
. ./%s

# additional test env by AT_ENV
%s

# check to perform
%s

%s
|}
      Autofonce_config.Globals.env_autofonce_sh
      (String.concat "\n" ( List.rev ter.tester_renvs ))
      (commented (string_of_check check))
      check.check_command
  in
  let test_dir = tester_dir ter in
  EzFile.write_text_file ( test_dir // check_sh ) check_content ;
  Unix.chmod (test_dir // check_sh ) 0o755 ;
  Unix.chdir test_dir ;
  let checker_pid = EzCall.create_process_for_shell
      [ "./" ^ check_sh ]
      ~stdout:check_stdout
      ~stderr:check_stderr
  in
  Unix.chdir state.state_run_dir ;
 {
    checker_check = check ;
    checker_tester = ter ;
    checker_pid ;
  }

let replace str ~by ~s =
  let re = Re.Posix.compile_pat str in
  Re.replace_string re ~by s

let subst_env state var =
  match StringMap.find var state.state_args.arg_subst_env with
  | pair -> pair
  | exception Not_found ->
      let pair =
        match Sys.getenv var with
        | exception Not_found -> None
        | str ->
            let by = Printf.sprintf "${%s}" var in
            Some (str, by)
      in
      state.state_args.arg_subst_env <-
        StringMap.add var pair state.state_args.arg_subst_env;
      pair

let read_subst_output ter file =
  let s = EzFile.read_text_file file in
  let t = ter.tester_test in
  let state = ter.tester_state in
  List.fold_left (fun s var ->
      if var = "AUTOFONCE" then
        let p = state.state_project in
        let test_dir = tester_dir ter in
        let s = replace test_dir ~by:"${AUTOFONCE_RUN_DIR}/${TEST_ID}" ~s in
        let s =
          if p.project_build_dir <> p.project_source_dir then
            replace p.project_build_dir ~by:"${AUTOFONCE_BUILD_DIR}" ~s
          else
            s
        in
        let s = replace p.project_source_dir ~by:"${AUTOFONCE_SOURCE_DIR}" ~s in
        s
      else
        match subst_env state var with
        | None -> s
        | Some (str, by) -> replace str ~by ~s
    ) s t.test_subst

let check_failures cer retcode =
  let check = cer.checker_check in
  let ter = cer.checker_tester in
  let state = ter.tester_state in
  let test_dir = tester_dir ter in
  let check_prefix = Printf.sprintf "%s_%s" check.check_step
      check.check_kind
  in
  if check.check_kind = "CHECK" then
    state.state_nchecks <- state.state_nchecks + 1;
  let check_stdout = Printf.sprintf "%s.out" check_prefix in
  let check_stderr = Printf.sprintf "%s.err" check_prefix in
  let compare to_check file kind =
    match to_check with
    | Ignore -> []
    | Content expected ->
        let stdout_file = test_dir // file in
        let subst_stdout = read_subst_output ter stdout_file in
        if subst_stdout <> expected then begin
          let stdout_expected_file = stdout_file ^ ".expected" in
          EzFile.write_text_file stdout_expected_file expected ;
          let stdout_subst_file = stdout_file ^ ".subst" in
          EzFile.write_text_file stdout_subst_file subst_stdout ;
          MISC.command_ "diff -u %s %s > %s.diff"
            stdout_subst_file stdout_expected_file stdout_file;
          [ kind ]
        end else []
    | Save_to_file target ->
        let stdout_file = test_dir // file in
        let target_file = test_dir // target in
        let content = EzFile.read_text_file stdout_file in
        EzFile.write_text_file target_file content;
        []
    | Diff_with_file expected ->
        let stdout_file = test_dir // file in
        let stdout_content = EzFile.read_text_file stdout_file in
        let expected_file = test_dir // expected in
        let expected_content = EzFile.read_text_file expected_file in
        if expected_content <> stdout_content then begin
          assert (if true then false else false);
          let stdout_expected_file = stdout_file ^ ".expected" in
          EzFile.write_text_file stdout_expected_file expected_content ;
          MISC.command_ "diff -u %s %s > %s.diff"
            stdout_file stdout_expected_file stdout_file;
          [ kind ]
        end else []

  in
  begin match check.check_retcode with
    | None -> []
    | Some expected ->
        if retcode <> expected then begin
          let check_exit = Printf.sprintf "%s.exit" check_prefix in
          let check_exit = test_dir // check_exit in
          EzFile.write_text_file check_exit (string_of_int retcode);
          EzFile.write_text_file (check_exit ^ ".expected")
            (string_of_int expected);
          [ "exitcode" ]
        end else []
  end
  @
  compare check.check_stdout check_stdout "stdout"
  @
  compare check.check_stderr check_stderr "stderr"

let create_state ~exec_args p tc suite =
  let state_run_dir = p.project_run_dir in
  Unix.chdir state_run_dir;
  {
    state_args = exec_args ;
    state_suite = suite ;
    state_config = tc ;
    state_project = p ;
    state_run_dir ;
    state_status = "";
    state_banner = "" ;
    state_ntests_ran = 0 ;
    state_ntests_ok = 0 ;
    state_tests_failed = [] ;
    state_tests_skipped = [] ;
    state_tests_failexpected = [] ;
    state_buffer = Buffer.create 10000;
    state_ntests = suite.suite_ntests ;
    state_nchecks = 0;
    state_status_printed = false ;
  }
