(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)


(** Building witnesses.

    Whether two paths differing only in case denote one file is a property of
    the host rather than of any one cache, so it is settled once for the
    process.  It defaults to what the system name implies and is corrected by
    whoever creates a concrete store, which is the only place the actual
    file-system is known --- macOS is case-insensitive by default but need not
    be, and a Linux host may have a share mounted that is not. *)

(** [set_case_insensitive b] declares whether the host tells two paths differing
    only in case apart.  Call it before anything is cached; changing it later
    invalidates every witness recorded so far, which costs misses rather than
    wrong hits. *)
val set_case_insensitive: bool -> unit

(** [case_insensitive ()] is what {!set_case_insensitive} last established. *)
val case_insensitive: unit -> bool

(** [canonical_path name] is how [name] is spelled in a witness. *)
val canonical_path: string -> string

(** [file ~name ~contents] witnesses that the file [name] was read and held
    [contents]. *)
val file: name:string -> contents:string -> Types.witness

(** [dir ~name ~entries] witnesses that the directory [name] was searched and
    held exactly [entries].

    This is what keeps a copybook lookup honest: resolving a copybook in one
    directory of a search path asserts that the directories before it held no
    candidate, so a copybook {e added} to one of them must be a miss even though
    no file that was read has changed. *)
val dir: name:string -> entries:string list -> Types.witness

(** [env ~var ~value] witnesses that [var] was consulted and found unset
    ([None]) or set to a given value. *)
val env: var:string -> value:string option -> Types.witness

(** [sub key] witnesses that the entry [key] was reused. *)
val sub: Types.key -> Types.witness
