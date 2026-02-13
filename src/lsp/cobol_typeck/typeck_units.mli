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

val of_compilation_group
  : c:Cobol_common.Config.TYPES.cobol_config
  -> fold_exec_block':Typeck_outputs.fold_exec_block'
  -> Cobol_ptree.compilation_group
  -> Typeck_outputs.t Typeck_results.with_diags
