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

(* files looked up for project config *)
let project_config_source = ".autofonce"
let project_config_build = "autofonce.toml"

(* toplevel dir created to run tests *)
let tests_dir = "_autofonce"

(* name of env script created in every test dir *)
let env_autofonce_sh = "env_autofonce.sh"
