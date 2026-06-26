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

include Libcob_ctypes.V1.Module

open Libcob_ctypes.V1.Types
open Ctypes

let init ~name ~source =
  let module_ptr = Utils.nullptr cob_module in
  let globals_ptr = Utils.nullptr cob_global in
  let status = Libcob_ctypes.V1.Module.init module_ptr globals_ptr name source in
  Utils.check_status status ~call:(Module_init { module_name = name });
  !@module_ptr, !@globals_ptr
