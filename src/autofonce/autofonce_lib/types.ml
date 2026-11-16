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

open EzCompat (* for StringSet *)

include Autofonce_core.Types
include Autofonce_config.Types


type exec_args = {
  mutable arg_clean_tests_dir : bool ;
  mutable arg_max_jobs : int ;
  mutable arg_ignore_exitcode : bool ;
  mutable arg_print_results : bool ;
  mutable arg_subst_env : ( string * string ) option StringMap.t ;
  mutable arg_stop_on_first_failure : bool ;
  mutable arg_print_all : bool ;
  mutable arg_keep_skipped : bool ;
  mutable arg_keep_all : bool ;
  mutable arg_output : string option ; (* full path to results.log *)
  mutable arg_only_successful : bool ;
}

(* imperative context, these values are meaningless at the end of
   functions *)
type state = { (* variable name is `state` *)
  state_args : exec_args ;
  state_suites : (suite * testsuite_config) list ;
  state_run_dir : string ;
  state_project : project_config ;
  mutable state_banner : string ;
  mutable state_status : string ;
  mutable state_ntests_ran : int ;
  mutable state_ntests_ok : int ;
  mutable state_tests_failed : tester list ;
  mutable state_ntests_failed : int ;
  mutable state_tests_skipped : tester list ;
  mutable state_tests_failexpected : tester list ;
  mutable state_buffer : Buffer.t ;
  mutable state_ntests : int ;
  mutable state_nchecks : int ;
  mutable state_status_printed : bool ;
}

and tester = { (* variable name is `ter` *)
  tester_state : state ;
  tester_suite : suite ;
  tester_test : test ;
  tester_config : testsuite_config ;
  mutable tester_renvs : string list ;
  mutable tester_fail_expected : bool ;
  mutable tester_captured_files : StringSet.t ;
  mutable tester_fail_reason :
    ( location * string * check option ) option ;
}

and checker = { (* variable name is `cer` *)
  checker_check : check ;
  checker_tester : tester ;
  checker_pid : int ;
}

type suites = (suite * testsuite_config) list

let long_test_name t tc =
  Printf.sprintf "%s:%s" tc.config_name t.test_name
