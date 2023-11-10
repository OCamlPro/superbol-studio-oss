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

  type +'a qualmap

  type 'a binding =
    {
      value: 'a;
      full_qn: Cobol_ptree.qualname;
    }

  exception Ambiguous of Cobol_ptree.qualname list Lazy.t

end
include module type of TYPES
  with type 'a qualmap = 'a TYPES.qualmap

type +'a t = 'a qualmap

val pp_qualmap: 'a Pretty.printer -> 'a qualmap Pretty.printer
val pp_qualmap_struct: 'a Pretty.printer -> 'a qualmap Pretty.printer

val empty: 'a qualmap
val add: Cobol_ptree.qualname -> 'a -> 'a qualmap -> 'a qualmap

val fold: f:('a binding -> 'b -> 'b) -> 'a qualmap -> 'b -> 'b
val find: Cobol_ptree.qualname -> 'a qualmap -> 'a

val bindings: 'a qualmap -> 'a binding list
val find_binding: Cobol_ptree.qualname -> 'a qualmap -> 'a binding
