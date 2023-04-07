(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

type 'a result =
  | Ok of 'a
  | Error of string

val check_project : string ->
  (* warnings *) string list * (* errors *) string list
val manifest_of_file : string -> Manifest.vscode result
val file_of_manifest : string -> Manifest.vscode -> unit
