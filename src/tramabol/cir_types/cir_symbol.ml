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

type t = { base: string; num: int }
let count = ref 0
let reset () = count := 0
let fresh ~base = incr count; { base; num = !count }
let pp ppf { base; num } = Pretty.print ppf "%s/%u" base num
let hash { num; _ } = num
let equal a b = a.num == b.num
let compare a b = Int.compare a.num b.num
