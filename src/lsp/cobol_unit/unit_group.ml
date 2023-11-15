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

module TYPES = struct
  type group = Unit_collections.SET.t
  type +'a group_map = 'a Unit_collections.MAP.t
end

(* (\** Alias for {!Collections.SET.empty} *\) *)
(* let empty = Unit_collections.SET.empty *)

(* (\** Alias for {!Collections.SET.add} *\) *)
(* let add = Unit_collections.SET.add *)

(* (\** Alias for {!Collections.SET.iter} *\) *)
(* let iter = Unit_collections.SET.iter *)

include Unit_collections.SET
