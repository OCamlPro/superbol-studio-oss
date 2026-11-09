(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Arguments of commands that analyze files within their projects, on top of
    {!Common_args}. *)

(** [get ()] is the arguments, along with a function that applies them to a
    setup ({!Caching_commands.default_setup} by default). *)
val get
  : unit
  -> (?setup:Caching_commands.setup -> unit -> Caching_commands.setup)
     * Ezcmd.V2.EZCMD.TYPES.arg_list
