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

val config
  : project_layout: Superbol_project.layout
  -> ?enable_caching: bool
  -> ?fallback_storage_directory: string
  -> unit
  -> Server.config

val run
  : config: Server.config
  -> Server.exit_status
