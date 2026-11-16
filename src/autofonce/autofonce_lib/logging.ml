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
(*
open Ezcmd.V2
open EZCMD.TYPES
*)
open Ez_file.V1
open EzFile.OP

module MISC = Autofonce_misc.Misc
module PARSER = Autofonce_core.Parser
(* module CONFIG = Autofonce_config.Project_config *)

open Types

let indents =  Array.init 10 (fun i -> String.make i ' ')

let log_header ?(indent=0) state fmt =
  let b = state.state_buffer in
  let indent = indents.( indent ) in
  Printf.kprintf (fun s ->
      Printf.bprintf b "\n%s#######################################\n" indent;
      Printf.bprintf b    "%s#\n%s#          %50s\n%s#\n" indent indent s indent;
      Printf.bprintf b "%s#######################################\n\n" indent;
    ) fmt

let log_checks ?failed_check state ter =
  let t = ter.tester_test in
  let b = state.state_buffer in

  let rec print_check b check_kind check =

    log_header ~indent:4 state "Check %s %s" check_kind check.check_step;

    Printf.bprintf b "Command:\n%s\n" ( PARSER.m4_escape check.check_command );

    let check_prefix = Runner_common.check_prefix check in
    let check_sh = Printf.sprintf "%s.sh" check_prefix in
    let check_exit = Printf.sprintf "%s.exit" check_prefix in
    let check_stdout = Printf.sprintf "%s.out" check_prefix in
    let check_stderr = Printf.sprintf "%s.err" check_prefix in

    let files =
      [
        check_sh ;
        check_exit ;
        check_exit ^ ".expected" ;
        check_stdout
      ]
      @
      ( match t.test_subst with
          [] -> []
        | _ ->
            [
              check_stdout ^ ".subst";
            ]
      )
      @
      [
        check_stdout ^ ".expected";
        check_stdout ^ ".diff";
        check_stderr ;
      ]
      @
      ( match t.test_subst with
          [] -> []
        | _ ->
            [
              check_stderr ^ ".subst";
            ]
      )
      @
      [
        check_stderr ^ ".expected";
        check_stderr ^ ".diff";
      ]
    in

    let check_dir = Runner_common.check_dir check in

    List.iter (fun file ->
        let filename = check_dir // file in
        if Sys.file_exists filename then begin
          log_header ~indent:6 state "File %s" file;
          match EzFile.read_text_file filename with
          | exception exn ->
              Printf.bprintf b "Exception while reading %S:\n  %s\n"
                filename ( Printexc.to_string exn )
          | file ->
              Printf.bprintf b "\n```\n%s```\n" file
        end
      ) files ;
    match failed_check with
    | None -> ()
    | Some failed_check ->
        if check == failed_check then raise Exit

  and print_action b action =
    match action with
    | AT_CLEANUP _ ->
        Printf.bprintf b "  AT_CLEANUP reached\n"
    | AT_FAIL _ ->
        Buffer.add_string b "   AT_FAIL_IF([true]) reached"; raise Exit
    | AT_FAIL_IF { step ; loc ; command } ->
        let check = Runner_common.check_of_AT_FAIL_IF ter step loc command in
        print_check b "AT_FAIL_IF" check

    | AT_CHECK check ->
        print_check b "AT_CHECK" check

    | AF_COMMENT _
    | AF_COPY _
    | AT_DATA _
    | AF_ENV _
    | AT_CAPTURE_FILE _
    | AT_XFAIL
    | AT_SKIP
    | AT_XFAIL_IF _
    | AT_SKIP_IF _ -> ()

  and print_actions b actions =
    List.iter ( print_action b ) actions

  in
  try
    print_actions b t.test_actions
  with Exit -> ()



let log_captured_files ?indent ?dir state msg files =
  let b = state.state_buffer in
  let p = state.state_project in
  let dir = match dir with
    | None -> p.project_source_dir
    | Some dir -> dir in
  List.iter (fun file ->
      log_header ?indent state "%s: captured file %S" msg file ;
      let filename = dir // file in
      match EzFile.read_text_file filename with
      | exception exn ->
          Printf.bprintf b "Exception while reading %S:\n  %s\n"
            filename ( Printexc.to_string exn )
      | file ->
          Printf.bprintf b "\n```\n%s```\n" file
    ) files

let b1 = Buffer.create 10000
let b2 = Buffer.create 10000

let log_failed_tests state msg tests =
  let b = state.state_buffer in
  List.iter (fun ter ->
      let t = ter.tester_test in
      let test_dir = Runner_common.tester_dir ter in

      let (reason, failed_check) =
        match ter.tester_fail_reason with
        | None -> assert false
        | Some (_loc, reason, check ) -> reason, check
      in
      log_header state "%s %04d %s (%s %s)"
        msg t.test_id t.test_name
        (PARSER.name_of_loc t.test_loc) reason;

      begin
        match t.test_keywords with
        | [] -> ()
        | list ->
            Printf.bprintf b "AT_KEYWORDS(%s)\n"
              (PARSER.m4_escape (String.concat " " list))
      end;

      Buffer.reset b1;
      Promote.print_actions
        ~ignore_exitcode:false
        ~keep_old:true
        t b1 t.test_actions ;
      let s1 = Buffer.contents b1 in
      let f1 = test_dir // "test.at.expected" in
      EzFile.write_text_file f1 s1;

      Buffer.reset b2;
      Promote.print_actions
        ~ignore_exitcode:false
        ~keep_old:false
        t b2 t.test_actions ;
      let s2 = Buffer.contents b2 in
      let f2 = test_dir // "test.at.promoted" in
      EzFile.write_text_file f2 s2;

      let test_at_diff = test_dir // "test.at.diff" in
      MISC.command_ "diff -u %s %s > %s"
        f1 f2 test_at_diff ;
      let diff = EzFile.read_text_file test_at_diff in

      log_header ~indent:2 state "Expected test:";
      Printf.bprintf state.state_buffer "%s\n" s1;

      log_header ~indent:2 state "Promoted test:";
      Printf.bprintf state.state_buffer "%s\n" s2;

      log_header ~indent:2 state "Diff test:";
      Printf.bprintf state.state_buffer "%s\n" diff;

      log_header ~indent:2 state "Test checks:";
      log_checks state ?failed_check ter ;

      log_captured_files
        ~indent:3
        ~dir:test_dir
        state
        (Printf.sprintf "Test %04d" t.test_id)
        (StringSet.to_list ter.tester_captured_files);
      ()
    ) tests ;
  ()

let log_state_buffer state =
  let p = state.state_project in
  log_failed_tests state "Failure"
    ( List.rev state.state_tests_failed ) ;
  log_failed_tests state "Expected Failure"
    ( List.rev state.state_tests_failed ) ;
  log_captured_files state "Project" p.project_captured_files;

  let buffer_file = match state.state_args.arg_output with
    | None ->
        let tests_dir = Autofonce_config.Globals.tests_dir in
        MISC.getcwd () // tests_dir // "results.log"
    | Some output -> output
  in
  let buffer = Buffer.contents state.state_buffer in
  EzFile.write_text_file buffer_file buffer;
  Printf.eprintf "File %S created with failure results\n%!" buffer_file;
