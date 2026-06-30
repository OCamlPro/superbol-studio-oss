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

(* store multiple block replacements in files until `commit_on_disk`
   is called to perform all of them. *)

val reset : unit -> unit
val replace_block :
  file:string -> line_first:int -> line_last:int -> string -> unit

type action =
  | Apply (* really apply to file *)
  | Fake of string  (* generate files with EXTENSION instead of former
                       name *)
  | Diff of { exclude : string list ; args : string option }

val commit_to_disk : ?action:action -> ?backup:string -> unit -> unit
