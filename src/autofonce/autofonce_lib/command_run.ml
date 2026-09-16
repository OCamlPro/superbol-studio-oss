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


let cmd =
  let testsuite_args, get_testsuite_args = Testsuite.args () in
  let filter_args, get_filter_args = Filter.args () in
  let runner_args, exec_args = Runner_common.args () in
  let args =
    runner_args @
    testsuite_args @
    filter_args
  in
  EZCMD.sub
    "run"
    (fun () ->
       try
         let filter_args = get_filter_args () in
         let (p, tc, suite) = Testsuite.find ( get_testsuite_args () ) in
         let n = Testsuite.exec ~filter_args ~exec_args p tc suite in
         if n > 0 then exit 1
       with
         exn ->
           if Printexc.backtrace_status () then
             Printexc.print_backtrace stderr;
           raise exn
    )
    ~args
    ~doc: "Run testsuite of the current project"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P {|Run the testsuite.|} ;
        `P {|$(b,autofonce) expects the existence of either $(b,autofonce.toml) or $(b,.autofonce).|} ;
        `P {|$(b,autofonce.toml) is required to configure the tests that will be run, depending on the project. Check the following command for more information:|} ;
        `Pre {|\$ autofonce init --help|} ;
        `P {|Before running the tests, you may want to list the test in the current testsuite with:|} ;
        `Pre {|\$ autofonce list --help|} ;
        `P {|To run tests, $(b,autofonce) will create a directory $(b,_autofonce/) in the directory containing the file $(b,autofonce.env).|} ;
        `P {|Every test is run independantly in a test directory with its number in the $(b,_autofonce/) directory. The test directory is removed if the test does not fail, or if it was expected to fail. Use the $(b,--keep-more) argument to keep directories of tests that have been skipped or were expected to fail. Use the $(b,--keep-all) argument to keep all directories.|} ;
        `P {|You can select which tests to run, by selecting a range of tests using $(b,--after TEST) or $(b,--before TEST), by selecting individual tests identifiers using $(b,--id NUM) or by selecting keywords using $(b,--keyword KEYWORD).|} ;
        `P {|$(b,autofonce) will only display failed tests on its output. You can use the argument $(b,--print-all) to display all tests that were not OK, or just read the generated file $(b,_autofonce/results.log).|};
      ];
    ]
