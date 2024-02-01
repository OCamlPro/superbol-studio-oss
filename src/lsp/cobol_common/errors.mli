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

exception FatalError of string
val fatal: ('a, Format.formatter, unit, _) format4 -> 'a

(* TODO: remove this (note: only one ref in `cobol_data`) *)
(** [join_all diags res_l] takes every value of a list of [('a, 'e) result list]
    and return a [('a, 'e) result]. If all the elements in the parameter list are
    [Ok _] then the result is [Ok _] otherwise [Error _] *)
val join_all: ('a, 'e) result list -> ('a list, 'e) result

val init_default_exn_printers: unit -> unit

(** Exit the program with a status that depends on diagnostics reported in
    {!Diagnostics.show_n_forget} (or {!Diagnostics.sink_result}).  *)
val exit: ?status:int -> unit -> _
