(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

val run_module
  : vm:('f, 'b, 'r, 'm, 's, 'branch) Types.manager
  -> ('f, 'r, 'm) Cir_types.module_handle
  -> 's
  -> ('s * int, Types.localized_runtime_errors) result
