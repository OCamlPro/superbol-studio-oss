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

open Ezcmd.V2

let verbosity = ref 0

module MAIN = EZCMD.MAKE(struct

    let command = "superbol"
    let about = {|superbol [SUBCOMMANDS] [ARGUMENTS]|}
    let set_verbosity n = verbosity := n
    let get_verbosity () = !verbosity

    let backtrace_var = Some "SUPERBOL_BACKTRACE"
    let usage =
      {|
swiss-knife for the COBOL language by OCamlPro"
|}
    let version = Version.version

    exception Error = Cobol_common.FatalError

  end)
