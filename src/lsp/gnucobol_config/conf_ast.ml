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

type support_value =
  | Ok
  | Warning
  | Archaic
  | Obsolete
  | Skip
  | Ignore
  | Error
  | Unconformable

type support =
  | Normal of support_value
  | Additional of support_value

type alias =
  | Normal of string * string
  | Context of string * string

type value =
  | Any of string
  | Bool of bool
  | Int of int
  | String of string
  | Support of support
  | ContextWord of string
  | Alias of alias

let pp_value fmt = function
  | Any s -> Pretty.print fmt "any (%s)" s
  | Bool b -> Pretty.print fmt "boolean (%B)" b
  | Int i -> Pretty.print fmt "integer (%d)" i
  | String s -> Pretty.print fmt "string (%s)" s
  | ContextWord s -> Pretty.print fmt "word (%s*)" s
  | Support _ -> Pretty.print fmt "support"
  | Alias (Context (a, v)) -> Pretty.print fmt "alias (%s*=%s)" a v
  | Alias (Normal (a, v)) -> Pretty.print fmt "alias (%s=%s)" a v


type set_value =
  { key: string;
    value: value; }

type conf_node =
  | ReservedWords of string
  | Include of string
  | Value of set_value

type t = conf_node list

