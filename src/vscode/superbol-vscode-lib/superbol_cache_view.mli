(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** Tree view of the caches of the workspace folders.

    Entries of each cache are grouped by the date they were last used, and then
    by directory and file.  Caches can be pruned from there, by applying the
    configured limits, or by removing entries unused for some time. *)

(** Identifier of the view. *)
val view_id: string

(** Commands, whose handlers follow. *)

val show_command: string
val refresh_command: string
val prune_command: string
val prune_unused_since_command: string

(** [start instance] registers the view. *)
val start: Superbol_instance.t -> unit

(** [show ()] reveals the view and refreshes it. *)
val show: unit -> unit

(** [refresh ()] reads every cache shown again, as well as the status bar. *)
val refresh: unit -> unit

(** [prune ~args] asks how to prune the cache of a workspace folder: that of
    the node given as first of [args], if any, or else that of the folder
    selected by the user. *)
val prune: args: Ojs.t list -> unit

(** [prune_unused_since ~args] prunes the entries of the age group given as
    first of [args], along with every entry used even less recently, once
    confirmed by the user. *)
val prune_unused_since: args: Ojs.t list -> unit
