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
open EZCMD.TYPES

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

(** Environment variable that sets whether outputs hold colors, as [--color]
    does. *)
let colors_var = "SUPERBOL_COLOR"

(** Environment variable that sets the margins of outputs (as accepted by
    {!Pretty.set_margins}), for processes whose outputs are copied to those of
    another one. *)
let margins_var = "SUPERBOL_MARGINS"

(** Arguments of every sub-command. *)
let common_args =
  [
    ["color"],
    Arg.String begin fun spec ->
      match Pretty.set_colors spec with
      | Ok () -> ()
      | Error msg -> Cobol_common.fatal "Invalid value for --color: %s" msg
    end,
    EZCMD.info ~docv:"WHEN" @@
    Printf.sprintf
      "Whether to color outputs: $(b,always), $(b,never), or $(b,auto) (the \
       default) to color those bound to a terminal.  WHEN may also be a \
       comma-separated list of $(b,stdout:WHEN) or $(b,stderr:WHEN), to color \
       the standard output or error alone.  Overrides the environment \
       variable %s, that accepts the same values" colors_var;
  ]
