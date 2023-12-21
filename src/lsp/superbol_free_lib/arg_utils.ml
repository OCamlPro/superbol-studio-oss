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

type dual_switch = [`enable_disable | `with_without | `boolean]
type single_switch = [`use]

let dual_switch ?descr (kind: dual_switch) ~name ~default =
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
  let descr = Option.value descr ~default:name in
  switch,
  [
    [arg_set], Arg.Set switch,
    Pretty.string_to EZCMD.info "%s %s (%s by default)" set descr default;

    [arg_unset], Arg.Clear switch,
    Pretty.string_to EZCMD.info "%s %s (%s by default)" unset descr default;
  ]

let single_switch ?descr (kind: single_switch) ~name ~default =
  let switch = ref default in
  let set = match kind with
    | `use when not default -> "Use"
    | `use -> "Don't use"
  in
  let arg_set = match kind with
    | `use when not default -> name
    | `use -> "no-"^name
  in
  let descr = Option.value descr ~default:name in
  switch,
  [
    [arg_set], Arg.Set switch,
    Pretty.string_to EZCMD.info "%s %s" set descr;
  ]

let switch ?descr = function
  | #dual_switch as kind -> dual_switch ?descr kind
  | #single_switch as kind -> single_switch ?descr kind
