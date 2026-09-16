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

val of_cobol_unit
  : vm:('f, 'r, 'module_memory) Types.value_manager
  -> Cobol_unit.Types.t
  -> (('f, 'module_memory) Types.module_handle, Types.errors) result

val init
  : vm:('f, 'r, 'module_memory) Types.value_manager
  -> ('f, 'module_memory) Types.module_handle
  -> (unit, Types.errors) result
