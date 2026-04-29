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

include Libcob_ctypes.V1.Numeric

open Libcob_ctypes.V1.Types

let add ?(number_store_flags = Number_store_flags.none) a b =
  add a b number_store_flags

let sub ?(number_store_flags = Number_store_flags.none) a b =
  sub a b number_store_flags
