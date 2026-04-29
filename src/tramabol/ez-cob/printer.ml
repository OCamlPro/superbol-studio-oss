(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Types

let pp_libcob_call ppf = function
  | Module_init { module_name } ->
      Fmt.pf ppf "initialization@ of@ module@ %S" module_name

let pp_error ppf = function
  | Nonzero_status { call; status } ->
      Fmt.pf ppf "%a@ returned@ status@ code@ %nd" pp_libcob_call call status
