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
  let args =
    testsuite_args @
    filter_args @
    [
    ]
  in
  EZCMD.sub
    "list"
    (fun () ->
       let filter_args = get_filter_args () in
       let (_p, _tc, suite) = Testsuite.find ( get_testsuite_args ())  in
       Testsuite.print ~filter_args suite
    )
    ~args
    ~doc: "Print testsuite of the current project"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P {|List the tests, with their numeric identifier, their name and their location in the testsuite files.|}
      ];
    ]
