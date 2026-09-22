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
open EZCMD.TYPES

module Patch_lines = Autofonce_patch.Patch_lines
module Parser = Autofonce_core.Parser
module Misc = Autofonce_misc.Misc
open Types
open Filter

(* TODO: check why the ignore pattern does not work *)
let diff args = Patch_lines.Diff { exclude = [ "^# promoted on .*" ]; args }
let todo = ref (diff None)

(* TODO: remove code duplication with Command_diff *)
let patch_action ~filter_args ~exec_args ~action p tc suite =
  filter_args.arg_only_failed <- true ;
  Patch_lines.reset ();
  let state = Runner_common.create_state ~exec_args p tc suite in
  Unix.chdir state.state_run_dir ;
  (*
  let comment_line =
    let t = Unix.gettimeofday () in
    let tm = Unix.localtime t in
    Printf.sprintf "# promoted on %04d-%02d-%02dT%02d:%02d"
      ( 1900 + tm.tm_year )
      ( 1 + tm.tm_mon )
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
  in
*)
  let promote_test t =

    let rec check actions =
      match actions with
      | [] -> false
      | AF_COMMENT comment ::
        AT_DATA _ ::
        _ when EzString.starts_with comment ~prefix:"autofonce.read:"
        -> true
      | _ :: actions -> check actions
    in
    if check t.test_actions then
      let file = t.test_loc.file in
      Printf.eprintf "Promoting test %d %s\n%!"
        t.test_id ( Parser.name_of_loc t.test_loc );
      let line_first = t.test_loc.line in
      let line_last =
        match List.rev t.test_actions with
        | AT_CLEANUP { loc } :: _ -> loc.line
        | _ -> Misc.error
                 "Last test in %s does not end with AT_CLEANUP ?" file
      in

      let b = Buffer.create 10000 in

      Printf.bprintf b "AT_SETUP(%s)\n" (Parser.m4_escape t.test_name);

      begin
        match t.test_keywords with
        | [] -> ()
        | list ->
            Printf.bprintf b "AT_KEYWORDS(%s)\n\n"
              (Parser.m4_escape (String.concat " " list))
      end;
      Promote.print_actions
        ~ignore_exitcode:exec_args.arg_ignore_exitcode
        ~keep_old:true
        t b t.test_actions;

      let content = Buffer.contents b in
      Patch_lines.replace_block ~file ~line_first ~line_last content
  in
  List.iter promote_test suite.suite_tests;
  Patch_lines.commit_to_disk ~action ();
  ()


let cmd =
  let testsuite_args, get_testsuite_args = Testsuite.args () in
  let filter_args, get_filter_args = Filter.args () in
  let runner_args, exec_args = Runner_common.args () in
  let args =
    runner_args @
    testsuite_args @
    filter_args @
    [

      [ "diff-args" ], Arg.String (fun s -> todo := diff (Some s)),
      EZCMD.info ~docv:"ARGS" "Pass these args to the diff command" ;

      [ "apply" ], Arg.Unit (fun () -> todo := Apply),
      EZCMD.info "Apply the generated diff promoting the test results once" ;

      [ "fake-apply" ], Arg.String (fun ext -> todo := Fake ext),
      EZCMD.info ~docv:".EXT"
        "Apply the diff to create new files with extension $(docv)" ;

    ]
  in
  EZCMD.sub
    "regen"
    (fun () ->
       let filter_args = get_filter_args () in
       let p, tc, suite = Testsuite.find ( get_testsuite_args () ) in
       patch_action ~filter_args ~exec_args ~action:!todo p tc suite
    )
    ~args
    ~doc: "Regenerate tests from templates"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P {|.|} ;
      ];
    ]
