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

open Cobol_common.Diagnostics.TYPES

val working_data_of_compilation_unit'
  : Cobol_config.Types.t
  -> Cobol_data_old.Env.PROG_ENV.t
  -> Cobol_ptree.Types.compilation_unit Cobol_ptree.Types.with_loc
  -> Cobol_data_old.Group.t' Cobol_ptree.Types.with_loc list with_diags
