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

(* [string ~filename content] *)
val string : filename:string -> string -> t

val channel : filename:string -> in_channel -> t

val from : filename:string -> f:(t -> 'a) -> 'a
