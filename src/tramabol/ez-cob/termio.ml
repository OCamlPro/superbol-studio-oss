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

include Libcob_ctypes.V1.Termio

let display ?(newline = false) fields =
  Array.iteri begin fun i field ->
    let nl = newline && i = Array.length fields - 1 in
    Libcob_ctypes.V1.Termio.display 0 (if nl then 1 else 0) 1 field
  end fields
