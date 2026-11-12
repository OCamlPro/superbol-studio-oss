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

(** Status bar item that shows how much room the caches of the workspace
    folders take, on the left of the status bar.

    Each workspace folder has a cache of its own: the language server puts
    every file of a workspace folder in the project of that folder.  The item
    is shown whenever one of them has a cache, and highlighted whenever one of
    them exceeds the size limit. *)

(** Command that refreshes the item, run when it is clicked. *)
val refresh_command: string

(** [start instance] creates the item, and has it refreshed whenever a document
    of a workspace folder is saved, workspace folders change, a setting about
    caches changes, or the language server (re)starts. *)
val start: Superbol_instance.t -> unit

(** [refresh ()] has the item refreshed, e.g. once a workspace analysis is
    over. *)
val refresh: unit -> unit
