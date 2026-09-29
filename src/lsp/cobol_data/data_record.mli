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

(** Utilities for accessing record info and managing collections of records *)

open Data_types

val size: record -> Data_memory.size
val storage: record -> data_storage

module SET: sig
  include Stdlib.Set.S with type elt = record
  val pp: t Pretty.printer
end

module MAP: sig
  include Stdlib.Map.S with type key = record
  val pp: ?fbind: ('x, _, 'x) format -> 'a Pretty.printer -> 'a t Pretty.printer
end
