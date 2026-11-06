(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Placeholder for process exit status *)

(** Records that errors were reported: the exit status will be odd. *)
val raise: unit -> unit

(** Records that some work could not be carried out (e.g. some input could not
    be processed): the exit status will be at least 2. *)
val fail: unit -> unit

val exit: ?status:int -> unit -> 'a
