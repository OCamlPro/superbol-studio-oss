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

type t =
  | String of { contents: string; filename: string }
  | Channel of { ic: in_channel; filename: string }

let string ~filename contents =
  String { contents; filename }

let channel ~filename ic =
  Channel { ic; filename }

(** [from ~filename ~f] feeds [f] with the contents of a file named [filename];
    uses [stdin] if [filename = ""]. *)
let from ~filename ~f =
  if filename = ""
  then f (Channel { ic = stdin; filename = "" })
  else let ic = open_in_bin filename in
    try let res = f (Channel { ic; filename }) in close_in ic; res
    with e -> close_in_noerr ic; raise e
