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

val check_dir : Types.check -> string
val create_state :
  exec_args:Types.exec_args ->
  Types.project_config ->
  Types.testsuite_config -> Types.suite -> Types.state
val check_prefix : Types.check -> string
val test_dir : Types.test -> string
val test_is_ok : Types.tester -> unit
val print_status : Types.state -> unit
val check_of_AT_SKIP_IF :
  Types.tester -> string -> Types.location -> string -> Types.check
val check_of_AT_XFAIL_IF :
  Types.tester -> string -> Types.location -> string -> Types.check
val check_of_AT_FAIL_IF :
  Types.tester -> string -> Types.location -> string -> Types.check
val check_of_at_file :
  Types.tester ->
  copy:bool -> string -> Types.location -> string -> Types.check
val exec_action_no_check : Types.tester -> Types.action -> unit
val test_is_skip : Types.tester -> unit

val check_failures : Types.checker -> int -> string list
val start_test : Types.state -> Types.test -> Types.tester
val start_check : Types.tester -> Types.check -> Types.checker
val tester_dir : Types.tester -> string
val test_is_failed :
  Types.location ->
  Types.tester -> ?check:Types.check -> string -> unit
val test_is_skipped_fail : Types.checker -> string -> unit
val output : Types.state -> ('a, unit, string, unit) format4 -> 'a
val print_ntests : int -> Types.tester list -> unit

val args : unit -> Ezcmd.V2.EZCMD.TYPES.arg_list * Types.exec_args
