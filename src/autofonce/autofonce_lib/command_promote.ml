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
module PARSER = Autofonce_core.Parser

let nb_promotions = ref 10

let rec action ~filter_args ~exec_args p suites =
  let n = Testsuite.exec ~filter_args ~exec_args p suites in
  if n > 0 then begin
    if !nb_promotions > 0 then begin
      decr nb_promotions;
      filter_args.arg_only_failed <- true ;
      filter_args.arg_filter <- true ;
      Command_diff.patch_action ~filter_args ~exec_args ~action:Patch_lines.Apply p suites ;
      PARSER.reset_ntests ();
      let suites = List.map (fun (_, tc) -> Testsuite.read p tc) suites in
      action ~filter_args ~exec_args p suites
    end else
      exit 1
  end

let cmd =
  let testsuite_args, get_testsuite_args = Testsuite.args () in
  let filter_args, get_filter_args = Filter.args () in
  let runner_args, exec_args = Runner_common.args () in
  let args =
    runner_args @
    testsuite_args @
    filter_args @
    [

      [ "n" ; "nb-promotions" ], Arg.Set_int nb_promotions, 
      EZCMD.info ~docv:"n" "Maximum number of result promotions before failing (10 by default)" ;

    ]
  in
  EZCMD.sub
    "promote"
    (fun () ->
       let filter_args = get_filter_args () in
       let p, suites = Testsuite.find ( get_testsuite_args () ) in
       action ~filter_args ~exec_args p suites
    )
    ~args
    ~doc: "Promote tests results as expected results"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Use this command to promote the results of tests to expected results.\
            This command runs and promote failing tests several times if needed." ;
      ];
    ]
