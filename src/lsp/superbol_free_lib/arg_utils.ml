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

let switch (kind: [`enable_disable | `with_without | `boolean]) ~name ~default =
  let switch = ref default in
  let set, unset = match kind with
    | `enable_disable -> "Enable", "Disable"
    | `with_without   -> "With", "Without"
    | `boolean        -> "Set", "Clear"
  in
  let arg_set, arg_unset = match kind with
    | `enable_disable | `boolean -> name, "no-"^name
    | `with_without -> "with-"^name, "without-"^name
  in
  let default = match kind with
    | `enable_disable when default -> "enabled"
    | `enable_disable              -> "disabled"
    | `with_without when default   -> "with"
    | `with_without                -> "without"
    | `boolean when default        -> "true"
    | `boolean                     -> "false"
  in
  switch,
  [
    [arg_set], Arg.Set switch,
    Pretty.string_to EZCMD.info "%s %s (%s by default)" set name default;

    [arg_unset], Arg.Clear switch,
    Pretty.string_to EZCMD.info "%s %s (%s by default)" unset name default;
  ]
