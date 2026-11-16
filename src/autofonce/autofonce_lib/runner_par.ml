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

open Types

type scheduler = {
  state : state ;
  test_fifo : test Queue.t ;
  mutable running_tests : running_test IntMap.t ;
  mutable current_jobs : int ;
}

and running_test = {
  scheduler : scheduler ;
  running_test : tester ;
  mutable waiting_actions : action list list ;
  mutable current_check : checker option ;
}

let b = Buffer.create 100
let title_size = 28
let spaces = String.make title_size ' '
let update_status s = (* 1 + 4 + 1 + 32 + 3 + 2 = 43 chars *)
  Buffer.clear b;
  let n = IntMap.cardinal s.running_tests in
  if n > 0 then begin
    Printf.bprintf b "[";
    match IntMap.min_elt s.running_tests with
    | None -> assert false
    | Some (_, r) ->
        let ter = r.running_test in
        let t = ter.tester_test in
        Printf.bprintf b "%d %s"
          t.test_id
          (let len = String.length t.test_name in
           if len > title_size then
             (String.sub t.test_name 0 (title_size-2) ^ "..")
           else
             t.test_name ^ (String.sub spaces 0 (title_size - len))
          );
        if n > 1 then
          Printf.bprintf b " +%d]" (n-1)
        else
          Buffer.add_string b "]"
  end;
  s.state.state_status <- Buffer.contents b;
  s.state.state_status_printed <- false

let rec schedule_job r =
  match r.waiting_actions with
  | [] ->
      Runner_common.test_is_ok r.running_test ;
      Runner_common.print_status r.running_test.tester_state
  | actions :: action_queue ->
      match actions with
      | [] ->
          r.waiting_actions <- action_queue ;
          schedule_job r
      | action :: actions ->
          r.waiting_actions <- actions :: action_queue ;
          schedule_action r action

and schedule_action r action =
  if !Globals.verbose > 1 then Printf.eprintf "schedule_action\n%!";
  match action with
  | AT_SKIP -> Runner_common.test_is_skip r.running_test
  | AT_FAIL { loc } ->
      (* TODO: cannot be auto-promoted, of course... *)
      Runner_common.test_is_failed loc r.running_test "AT_FAIL_IF"
  | AT_CHECK check ->
      schedule_check r check
  | AT_XFAIL_IF { step ; loc ; command } ->
      let ter = r.running_test in
      schedule_check r
        ( Runner_common.check_of_AT_XFAIL_IF ter step loc command )
  | AT_SKIP_IF { step ; loc ; command } ->
      let ter = r.running_test in
      schedule_check r
        ( Runner_common.check_of_AT_SKIP_IF ter step loc command )
  | AT_FAIL_IF { step ; loc ; command } ->
      let ter = r.running_test in
      schedule_check r
        ( Runner_common.check_of_AT_FAIL_IF ter step loc command )
  | AF_COPY { step ; loc ; command ; copy ; _ } ->
      let ter = r.running_test in
      schedule_check r (
        Runner_common.check_of_at_file ~copy ter step loc command )

  | AF_COMMENT _
  | AT_XFAIL
  | AT_DATA _
  | AT_CAPTURE_FILE _
  | AT_CLEANUP _
  | AF_ENV _
    ->
      Runner_common.exec_action_no_check r.running_test action;
      schedule_job r

and schedule_check r check =
  let ter = r.running_test in
  let cer = Runner_common.start_check ter check in
  if !Globals.verbose > 1 then
    Printf.eprintf "JOB %d STARTED\n%!" cer.checker_pid;
  r.current_check <- Some cer ;

  let s = r.scheduler in
  s.current_jobs <- s.current_jobs + 1;
  s.running_tests <- IntMap.add cer.checker_pid r s.running_tests;
  update_status s

let schedule_test s t =
  if !Globals.verbose > 1 then Printf.eprintf "schedule_test\n%!";
  let state = s.state in
  let ter = Runner_common.start_test state t in
  let r = {
    scheduler = s ;
    running_test = ter ;
    waiting_actions = [ t.test_actions ] ;
    current_check = None ;
  } in
  schedule_job r

and job_terminated r retcode =
  if !Globals.verbose > 1 then Printf.eprintf "job_terminated\n%!";
  match r.current_check with
  | None -> assert false
  | Some cer ->
      let check = cer.checker_check in
      let failures = Runner_common.check_failures cer retcode in
      match failures with
      | [] -> (* SUCCESS *)
          r.waiting_actions <- check.check_run_if_pass :: r.waiting_actions;
          schedule_job r
      | failures ->
          let failures = String.concat " " failures in
          if retcode = 99 then
            let check = cer.checker_check in
            let ter = cer.checker_tester in
            let loc = check.check_loc in
            Runner_common.test_is_failed ~check loc ter failures
          else
            match check.check_run_if_fail with
            | [] ->
                if retcode = 77 then
                  Runner_common.test_is_skipped_fail cer failures
                else
                  let check = cer.checker_check in
                  let ter = cer.checker_tester in
                  let loc = check.check_loc in
                  Runner_common.test_is_failed ~check loc ter failures
            | actions ->
                r.waiting_actions <- actions :: r.waiting_actions;
                schedule_job r

let run s =
  let rec iter () =
    if !Globals.verbose > 1 then Printf.eprintf "iter %d\n%!" s.current_jobs;
    if s.current_jobs < s.state.state_args.arg_max_jobs &&
       not (Queue.is_empty s.test_fifo) then
      let t = Queue.take s.test_fifo in
      schedule_test s t;
      iter ()
    else
    if s.current_jobs > 0 then
      let () = Runner_common.print_status s.state in
      let pid, status = EzCall.wait_pids () in
      if !Globals.verbose > 1 then Printf.eprintf "JOB %d finished\n%!" pid;
      let ret_code =
        match status with
        | WEXITED n -> n
        | WSIGNALED _ -> -1 (* TODO: what ? *)
        | WSTOPPED _ ->
            ( try Unix.kill Sys.sigkill pid with _ -> ());
            ( try Unix.kill Sys.sigcont pid with _ -> ());
            -1
      in
      let job = IntMap.find pid s.running_tests in
      s.running_tests <- IntMap.remove pid s.running_tests;
      s.current_jobs <- s.current_jobs - 1;
      update_status s;
      job_terminated job ret_code;
      iter ()
  in
  iter ()

let exec_testsuite ~filter_args state =
  let c = state.state_suite in
  let s = {
    state;
    test_fifo = Queue.create () ;
    running_tests = IntMap.empty;
    current_jobs = 0;
  } in
  let select_test t =
    Queue.add t s.test_fifo;
  in
  Filter.select_tests ~args:filter_args ~state select_test c;
  state.state_ntests <- Queue.length s.test_fifo ;
  run s
