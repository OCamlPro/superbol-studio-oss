(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

val type_: string

val provider: Superbol_instance.t -> Vscode.Task.t Vscode.TaskProvider.t

(** Keys of the settings that deal with copybooks. *)
val copybooks_setting: string
val copyexts_setting: string

(** One entry of the [superbol.cobol.copybooks] setting. *)
type copybook_path =
  {
    dir: string;
    file_relative: bool;
  }

val copybook_path_of_jsonoo: Jsonoo.t -> copybook_path
val copybook_path_to_jsonoo: copybook_path -> Jsonoo.t
