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

type args = {
  mutable arg_testsuite : string option ;
  mutable arg_testsuite_file : string option ;
  mutable arg_testsuite_env : string option ;  (* path to env file *)
  mutable arg_testsuite_path : string list ;
}

val args : unit ->
  Ezcmd.V2.EZCMD.TYPES.arg_list * ( unit -> args )

val exec :
  filter_args:Filter.args ->
  exec_args:Types.exec_args ->
  Types.project_config ->
  Types.testsuite_config -> Types.suite -> int

val find : args ->
  Types.project_config * Types.testsuite_config * Types.suite

val print : filter_args:Filter.args -> Types.suite -> unit

val read :
  Types.project_config -> Types.testsuite_config ->
  Types.project_config * Types.testsuite_config * Types.suite
