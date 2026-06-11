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

open Cobol_common.Platform.TYPES

type t =
  { source: source; filename: string }
and source =
  | String of string
  | Channel of in_channel

let string ~filename contents =
  { source = String contents; filename }

let channel ~filename ic =
  { source = Channel ic; filename }

(** [from ~filename ~f ~platform] feeds [f] with the contents of a file named
    [filename]; uses [stdin] (as per [platform]) if [filename = ""]. *)
let from ~filename ~f ~platform =
  if filename = ""
  then platform.with_stdin ~f:(fun ic -> f @@ channel ~filename ic)
  else f @@ string ~filename @@ platform.read_file filename
