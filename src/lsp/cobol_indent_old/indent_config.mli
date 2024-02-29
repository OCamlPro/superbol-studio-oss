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

type t = Indent_type.indent_config

val default : t

(** Merge two configs. Offsets in the second config take precedence. *)
val merge : t -> t -> t

val of_list : (string * int) list -> t

val offset_of_keyword: t -> Indent_type.context_kind -> int
