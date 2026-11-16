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

type promote_options = {
  ignore_exitcode : bool ;
  keep_old : bool ;
  (* Only update iff the new exitcode is 0 *)
  only_successful : bool ;
}

(* print the given actions in the buffer *)
val print_actions :
  promote_options -> Types.test -> Buffer.t -> Types.action list -> unit
