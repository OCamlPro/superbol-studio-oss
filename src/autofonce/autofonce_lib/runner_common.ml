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

(* Default number of parallel jobs: the number of online processors,
   or 16 if it cannot be found *)
let default_max_jobs = 16

let number_of_processors () =
  let positive s =
    match int_of_string_opt ( String.trim s ) with
    | Some n when n > 0 -> Some n
    | _ -> None
  in
  if Sys.win32 then
    match Sys.getenv "NUMBER_OF_PROCESSORS" with
    | exception Not_found -> None
    | s -> positive s
  else
    match Unix.open_process_in "getconf _NPROCESSORS_ONLN 2>/dev/null" with
    | exception _ -> None
    | ic ->
        let line = try Some ( input_line ic ) with _ -> None in
        match Unix.close_process_in ic, line with
        | Unix.WEXITED 0, Some line -> positive line
        | _ -> None

let args () =
  let args =
    {
      arg_clean_tests_dir = true ;
      arg_max_jobs =
        ( match number_of_processors () with
          | Some n -> n
          | None -> default_max_jobs ) ;
      arg_ignore_exitcode = false;
      arg_print_results = false ;
      arg_subst_env = StringMap.empty ;
      arg_stop_on_first_failure = false ;
      arg_print_all = false ;
      arg_keep_skipped = false ;
      arg_keep_all = false ;
      arg_output = None ;
      arg_only_successful = false ;
      arg_timeout = None ;
      arg_cpu_timeout = None ;
      arg_timeout_warning = 50 ;
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
    EZCMD.info
      ~env:(EZCMD.env "AUTOFONCE_JOBS")
      ~docv:"NJOBS"
      "Set maximal parallelism (default: number of processors, or 16 \
       if it cannot be found)";

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

    [ "ignore-exitcode" ], Arg.Unit (fun () ->
        args.arg_ignore_exitcode <- true),
    EZCMD.info "Do not promote or fail for wrong exit code" ;

    [ "only-successful" ], Arg.Unit (fun () ->
        args.arg_only_successful <- true),
    EZCMD.info "Promote only failed checks for commands with \
                successful exit status" ;

    [ "timeout" ], Arg.Int (fun n -> args.arg_timeout <- Some (max 0 n)),
    EZCMD.info
      ~env:(EZCMD.env "AUTOFONCE_TIMEOUT")
      ~docv:"SECONDS"
      "Kill checks running longer than $(docv) seconds (wall-clock), \
       using GNU timeout. 0 disables it (default: 30, or 10 times the \
       CPU timeout if --cpu-timeout is set, or 0 if no GNU timeout \
       command is found). Multiplied by N in tests using AF_TIMEOUT(N)";

    [ "cpu-timeout" ], Arg.Int (fun n -> args.arg_cpu_timeout <- Some (max 0 n)),
    EZCMD.info
      ~env:(EZCMD.env "AUTOFONCE_CPU_TIMEOUT")
      ~docv:"SECONDS"
      "Kill processes of checks using more than $(docv) seconds of CPU \
       time (ulimit -t), which is mostly independent of the load of the \
       machine. The limit applies to each process separately. 0 disables \
       it (default: 0). Multiplied by N in tests using AF_TIMEOUT(N)";

    [ "timeout-warning" ], Arg.Int (fun n ->
        args.arg_timeout_warning <- max 0 n),
    EZCMD.info
      ~env:(EZCMD.env "AUTOFONCE_TIMEOUT_WARNING")
      ~docv:"PERCENT"
      "At the end, warn about checks that used more than $(docv)% of \
       their wall-clock or CPU timeout, to help adding AF_TIMEOUT(N) \
       where needed. 0 disables the warning (default: 50)";

    [ "o" ; "output" ], Arg.String (fun s -> args.arg_output <- Some s),
    EZCMD.info
      ~env:(EZCMD.env "AUTOFONCE_OUTPUT")
      ~docv:"TESTSUITE"
      "Path of the output file (default: _autofonce/results.log)";

  ] ,
  args

let status_len = 30
let spaces = String.make 80 ' '

let test_status ter fmt =
  Printf.kprintf (fun s ->
      let t = ter.tester_test in
      let tc = ter.tester_config in
      let len = String.length s in
      let status_len = if len > status_len then len else status_len in
      let test_id = Printf.sprintf "%04d" t.test_id in
      let id_len = String.length test_id in
      let max_name_len = 79 - 2 - status_len - id_len in
      let max_name_len = if max_name_len < 0 then 0 else max_name_len in
      let test_name = Types.long_test_name t tc in
      let len = String.length test_name in
      let test_name =
        if len > max_name_len then
          String.sub test_name 0 max_name_len
        else
          test_name ^ String.sub spaces 0 (max_name_len - len)
      in
      Printf.sprintf "%s %s %s" test_id s test_name
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
    test_status ter "GOOD . (%s)"
      ( PARSER.name_of_loc test.test_loc )
  )

(* a return code of 77 means that the failure is ok to skip *)
let test_is_skipped_fail cer s =
  let ter = cer.checker_tester in
  let state = ter.tester_state in
  let check = cer.checker_check in
  if not state.state_args.arg_keep_skipped then
    MISC.remove_rec ( tester_dir ter ) ;
  state.state_tests_failexpected <- ter :: state.state_tests_failexpected ;
  buffer_test state (
    test_status ter "SK77 _ (%s %s)"
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
      ( test_status ter "KOOK ! (%s %s)"
          ( PARSER.name_of_loc loc ) s )
  end else begin
    state.state_tests_failed <- ter :: state.state_tests_failed ;
    state.state_ntests_failed <- state.state_ntests_failed + 1;
    let status =
      test_status ter "FAIL E (%s %s)" ( PARSER.name_of_loc loc ) s
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
    (test_status ter "SKIP   (%s)" ( PARSER.name_of_loc t.test_loc ) )

(* Create [dir] and all its missing parents *)
let rec mkdir_p dir =
  if not ( Sys.file_exists dir ) then begin
    let parent = Filename.dirname dir in
    if parent <> dir then mkdir_p parent;
    try Unix.mkdir dir 0o755 with
    | Unix.Unix_error ( Unix.EEXIST, _, _ ) -> ()
  end

(* Prepare [dst_file] for writing: create its parent directories
   and remove any existing file *)
let prepare_dst_file dst_file =
  mkdir_p ( Filename.dirname dst_file );
  if Sys.file_exists dst_file then Unix.unlink dst_file

(* GNU [timeout] is used to kill checks that run for too long. It is not
   available on macOS (unless coreutils are installed as [gtimeout]), and
   [timeout] on Windows is an unrelated command. *)
let default_timeout = 30

let timeout_command = lazy (
  if Sys.win32 then None
  else
    let find_in_path cmd =
      match Sys.getenv "PATH" with
      | exception Not_found -> None
      | path ->
          List.find_map (fun dir ->
              let file = Filename.concat dir cmd in
              if Sys.file_exists file then Some file else None)
            ( String.split_on_char ':' path )
    in
    List.find_map find_in_path [ "timeout" ; "gtimeout" ]
)

let timeout_warned = ref false

(* Effective timeouts for a check, as [(wall_clock, cpu)] in seconds,
   0 meaning no timeout. Base timeouts come from --timeout and
   --cpu-timeout. If only --cpu-timeout is given, the wall-clock
   timeout defaults to 10 times the CPU timeout, as a CPU limit alone
   would never kill a check that is waiting (deadlock, input...).
   Both are multiplied by the factor N set in the test by AF_TIMEOUT(N),
   so AF_TIMEOUT(0) disables them. Without a GNU timeout command, the
   default wall-clock timeout is 0, and an explicit one is ignored with
   a warning. *)
let timeouts ter =
  let args = ter.tester_state.state_args in
  let timeout_command = Lazy.force timeout_command in
  let cpu = match args.arg_cpu_timeout with
    | None -> 0
    | Some n -> n
  in
  let wall = match args.arg_timeout, timeout_command with
    | Some n, _ -> n
    | None, Some _ -> if cpu > 0 then 10 * cpu else default_timeout
    | None, None -> 0
  in
  let factor = match ter.tester_timeout with
    | None -> 1
    | Some factor -> factor
  in
  let wall = wall * factor in
  let cpu = cpu * factor in
  let wall =
    if wall > 0 && timeout_command = None then begin
      if not !timeout_warned then begin
        timeout_warned := true;
        Printf.eprintf
          "Warning: no GNU timeout command found, timeout ignored\n%!"
      end;
      0
    end else wall
  in
  wall, cpu

(* Command prefix used to run checks: [timeout] is only used when the
   wall-clock timeout is not 0. *)
let timeout_prefix wall =
  match Lazy.force timeout_command with
  | Some cmd when wall > 0 ->
      [ cmd ; "-k" ; "5s" ; Printf.sprintf "%ds" wall ]
  | _ -> []

(* Exit codes of a check killed by a timeout: 124 = GNU timeout,
   137 = SIGKILL (timeout -k, or hard CPU limit), 152 = SIGXCPU.
   For the wall-clock timeout, we also check the elapsed time, as the
   check itself may return 124 or 137. *)
let killed_by_timeout cer ~elapsed retcode =
  ( cer.checker_timeout > 0 && ( retcode = 124 || retcode = 137 )
    && elapsed >= float cer.checker_timeout -. 0.1 )
  || ( cer.checker_cpu_timeout > 0 && ( retcode = 152 || retcode = 137 ) )

let exec_action_no_check ter action =
  match action with
  | AT_DATA { file ; content } ->
      EzFile.write_text_file ( tester_dir ter // file ) content
  | AF_DATA_COPY { file ; dir ; content_file } ->
      let dst_file = tester_dir ter // file in
      prepare_dst_file dst_file;
      let content = EzFile.read_file ( dir // content_file ) in
      EzFile.write_file dst_file content
  | AF_DATA_LINK { file ; dir ; content_file } ->
      let dst_file = tester_dir ter // file in
      prepare_dst_file dst_file;
      Unix.link ( dir // content_file ) dst_file
  | AT_CLEANUP _ -> ()
  | AF_COMMENT _ -> ()
  | AT_XFAIL -> ter.tester_fail_expected <- true
  | AF_ENV env -> ter.tester_renvs <- env :: ter.tester_renvs
  | AF_TIMEOUT n -> ter.tester_timeout <- Some n
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
    if state.state_ntests_failed > 0 then begin
      Terminal.printf [ Terminal.red ]
        " %d fails:%!" state.state_ntests_failed;
      (* print_ntests 2 state.state_tests_failed; *)
    end;
    Terminal.printf [] " %s" state.state_status;
    Printf.printf "%!";
    state.state_status_printed <- true
  end

let start_test state t  cg =
  let c = t.test_suite in
  state.state_ntests_ran <- state.state_ntests_ran + 1;
  state.state_status_printed <- false ;
  print_status state;
  let ter = {
    tester_test = t ;
    tester_state = state ;
    tester_suite = c ;
    tester_config = cg ;
    tester_renvs = [] ;
    tester_fail_expected = false ;
    tester_captured_files = StringSet.empty ;
    tester_fail_reason = None ;
    tester_timeout = t.test_timeout ;
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
    cg.config_name
    state.state_run_dir
    state.state_project.project_source_dir
    state.state_project.project_build_dir
    cg.config_env.env_content
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
  let timeout, cpu_timeout = timeouts ter in
  let check_stdout = Printf.sprintf "%s.out" check_prefix in
  let check_stderr = Printf.sprintf "%s.err" check_prefix in
  let check_content =
    Printf.sprintf {|#!/bin/sh

# create test env
. ./%s

# additional test env by AT_ENV
%s
%s
# check to perform
%s

%s
|}
      Autofonce_config.Globals.env_autofonce_sh
      (String.concat "\n" ( List.rev ter.tester_renvs ))
      (if cpu_timeout > 0 then
         Printf.sprintf "\n# CPU time limit (--cpu-timeout, AF_TIMEOUT)\n\
                         ulimit -t %d\n" cpu_timeout
       else "")
      (commented (string_of_check check))
      check.check_command
  in
  let test_dir = tester_dir ter in
  EzFile.write_text_file ( test_dir // check_sh ) check_content ;
  Unix.chmod (test_dir // check_sh ) 0o755 ;
  Unix.chdir test_dir ;
  (* [times] saves the CPU time used by the check, for timeout warnings *)
  let checker_start = Unix.gettimeofday () in
  let checker_pid = EzCall.create_process
      ( timeout_prefix timeout @
        [ "sh" ; "-c" ;
          Printf.sprintf "./%s; r=$?; times > %s.times; exit $r"
            check_sh check_prefix ] )
      ~stdout:check_stdout
      ~stderr:check_stderr
  in
  Unix.chdir state.state_run_dir ;
 {
    checker_check = check ;
    checker_tester = ter ;
    checker_pid ;
    checker_timeout = timeout ;
    checker_cpu_timeout = cpu_timeout ;
    checker_start ;
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

(* CPU time (user+sys) of the children of the check, as printed by
   [times] in its second line. This is the total of all the processes
   of the check, whereas the CPU limit applies to each process, so it
   is an upper bound. *)
let read_cpu_time file =
  match EzFile.read_lines file with
  | exception _ -> None
  | lines ->
      if Array.length lines < 2 then None else
        try
          Scanf.sscanf lines.(1) " %dm%fs %dm%fs"
            (fun um us sm ss ->
               Some ( float ( um + sm ) *. 60. +. us +. ss ))
        with _ -> None

(* Record checks that used more than [arg_timeout_warning]% of one of
   their timeouts *)
let record_slow_check cer ~elapsed ~cpu_file =
  let state = cer.checker_tester.tester_state in
  let percent = state.state_args.arg_timeout_warning in
  if percent > 0 then begin
    let check limit kind used =
      if limit > 0 &&
         used *. 100. > float ( limit * percent ) then
        state.state_slow_checks <- {
          slow_check = cer.checker_check ;
          slow_kind = kind ;
          slow_used = used ;
          slow_limit = limit ;
        } :: state.state_slow_checks
    in
    check cer.checker_timeout "timeout" elapsed;
    if cer.checker_cpu_timeout > 0 then
      match read_cpu_time cpu_file with
      | None -> ()
      | Some used -> check cer.checker_cpu_timeout "cpu-timeout" used
  end

let print_slow_checks state =
  match state.state_slow_checks with
  | [] -> ()
  | list ->
      let ratio s = s.slow_used /. float s.slow_limit in
      let list =
        List.sort (fun s1 s2 -> compare (ratio s2) (ratio s1)) list in
      Terminal.printf [ Terminal.magenta ]
        "* %d checks used more than %d%% of their timeout:\n%!"
        ( List.length list ) state.state_args.arg_timeout_warning ;
      List.iteri (fun i s ->
          if i < 20 then
            Printf.printf "  %04d %s: %s %.1fs / %ds (%.0f%%)\n%!"
              s.slow_check.check_test.test_id
              ( PARSER.name_of_loc s.slow_check.check_loc )
              s.slow_kind s.slow_used s.slow_limit ( 100. *. ratio s )
          else if i = 20 then
            Printf.printf "  ...\n%!"
        ) list ;
      Printf.printf
        "  Use AF_TIMEOUT(N) in these tests to multiply their timeouts\n%!"

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
          let stdout_expected_file = stdout_file ^ ".expected" in
          EzFile.write_text_file stdout_expected_file expected_content ;
          MISC.command_ "diff -u %s %s > %s.diff"
            stdout_file stdout_expected_file stdout_file;
          [ kind ]
        end else []

  in
  let elapsed = Unix.gettimeofday () -. cer.checker_start in
  let killed = killed_by_timeout cer ~elapsed retcode in
  if not killed then
    record_slow_check cer ~elapsed
      ~cpu_file:( test_dir // ( check_prefix ^ ".times" ) );
  if killed && check.check_retcode <> Some retcode then begin
    (* stdout/stderr are truncated, no need to compare them. The
       .timeout file prevents promotion of this check. *)
    let check_timeout = test_dir // ( check_prefix ^ ".timeout" ) in
    EzFile.write_text_file check_timeout
      ( Printf.sprintf
          "exit code %d, timeout %ds, cpu timeout %ds\n"
          retcode cer.checker_timeout cer.checker_cpu_timeout );
    [ "timeout" ]
  end else
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

let create_state ~exec_args p suites =
  let state_run_dir = p.project_run_dir in
  Unix.chdir state_run_dir;
  let ntests = ref 0 in
  List.iter (fun (suite,_tc) ->
      ntests := !ntests + suite.suite_ntests ;
    ) suites;
  {
    state_args = exec_args ;
    state_suites = suites ;

    state_project = p ;
    state_run_dir ;
    state_status = "";
    state_banner = "" ;
    state_ntests_ran = 0 ;
    state_ntests_ok = 0 ;
    state_tests_failed = [] ;
    state_ntests_failed = 0 ;
    state_tests_skipped = [] ;
    state_tests_failexpected = [] ;
    state_buffer = Buffer.create 10000;
    state_ntests = !ntests ;
    state_nchecks = 0;
    state_status_printed = false ;
    state_slow_checks = [] ;
  }
