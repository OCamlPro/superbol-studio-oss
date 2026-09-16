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
  mutable arg_filter : bool ;

  mutable arg_failures : string option ;
  mutable arg_exec_after : int ;
  mutable arg_exec_before : int ;
  mutable arg_tests_ids : ( int * int ) list ;
  mutable arg_tests_keywords : string list ;
  mutable arg_tests_nokeywords : string list ;
  mutable arg_only_failed : bool ;
  mutable arg_all_keywords : bool ;
}

val select_tests :
  args:args ->
  ?state:Types.state -> (Types.test -> unit) -> Types.suite -> unit

val args : unit -> Ezcmd.V2.EZCMD.TYPES.arg_list * ( unit -> args )
