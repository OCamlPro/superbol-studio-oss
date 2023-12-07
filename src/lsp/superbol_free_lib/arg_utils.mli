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

(** [switch kind ~name ~default] returns a Boolean flag [flg] and specifications
    of arguments for setting/unsetting [flg].  The flag has default value
    [default], and the phrasing of the documentation for arguments depends on
    the [kind] of switch: [`enable_disable] and [`with_without] are
    self-explanatory; [`boolean] uses "Set" and "Clear" verbs.  [`use] generates
    a single argument selected based on the value of [default]. *)
val switch
  : ?descr: string
  -> [ `enable_disable | `with_without | `boolean | `use ]
  -> name: string
  -> default: bool
  -> bool ref * Ezcmd.V2.EZCMD.TYPES.arg_list
