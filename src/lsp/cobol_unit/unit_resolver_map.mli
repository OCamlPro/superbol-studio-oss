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

module TYPES: sig

  type +'a resolver_map

  type 'a binding =
    {
      value: 'a;
      full_qn: Cobol_ptree.qualname;
    }

  exception Ambiguous of Cobol_ptree.qualname Cobol_common.Basics.NEL.t Lazy.t

end
include module type of TYPES
  with type 'a resolver_map = 'a TYPES.resolver_map

type +'a t = 'a resolver_map

val pp: 'a Pretty.printer -> 'a resolver_map Pretty.printer
val pp_struct: 'a Pretty.printer -> 'a resolver_map Pretty.printer

val empty: 'a resolver_map
val add: Cobol_ptree.qualname -> 'a -> 'a resolver_map -> 'a resolver_map

val fold: f:('a binding -> 'b -> 'b) -> 'a resolver_map -> 'b -> 'b
val find: Cobol_ptree.qualname -> 'a resolver_map -> 'a

val bindings: 'a resolver_map -> 'a binding list
val find_binding: Cobol_ptree.qualname -> 'a resolver_map -> 'a binding

val is_empty: _ t -> bool
val cardinal: _ t -> int
