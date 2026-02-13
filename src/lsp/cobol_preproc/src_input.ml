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
  | String of { contents: string; filename: string }
  | Channel of { ic: in_channel; filename: string }

let string ~filename contents =
  String { contents; filename }

let channel ~filename ic =
  Channel { ic; filename }

(** [from ~filename ~f] feeds [f] with the contents of a file named [filename];
    uses [stdin] if [filename = ""]. *)
let from ~platform ~filename ~f =
  if filename = ""
  then f (channel ~filename stdin)
  else
    let contents = platform.read_file filename in
    f (string ~filename contents)

