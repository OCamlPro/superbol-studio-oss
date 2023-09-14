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

open Cobol_common.Basics

(** This functor is used to build a config ({! Types.T}) module from a file *)
module Make
    (Diags: Cobol_common.Diagnostics.STATEFUL)
    (Conf: sig
       include Types.CONFIG
       val options: Conf_ast.value StringMap.t
       val words: Types.words_spec
       val intrinsic_functions: StringSet.t
       val system_names: StringSet.t
       val registers: StringSet.t
     end): Types.T
