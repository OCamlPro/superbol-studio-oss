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
  { source: source; filename: string; platform: platform }
and source =
  | String of string
  | Channel of in_channel

let string ~filename contents ~platform =
  { source = String contents; filename; platform }

let channel ~filename ic ~platform =
  { source = Channel ic; filename; platform }

let source_platform t =
  t.platform

(** [from ~filename ~f] feeds [f] with the contents of a file named [filename];
    uses [stdin] if [filename = ""]. *)
let from ~filename ~f ~platform =
  f @@
  if filename = ""
  then channel ~filename stdin ~platform
  else string ~filename ~platform @@ platform.read_file filename
