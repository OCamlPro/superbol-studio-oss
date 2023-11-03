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

let en'dis'able_switch ~name ~default =
  let switch = ref default in
  let default = if default then "enabled" else "disabled" in
  switch,
  [
    [name], Arg.Set switch,
    Pretty.string_to EZCMD.info "Enable %s (%s by default)" name default;

    ["no-"^name], Arg.Clear switch,
    Pretty.string_to EZCMD.info "Disable %s (%s by default)" name default;
  ]
