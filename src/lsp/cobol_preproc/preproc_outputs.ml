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

open Preproc_diagnostics

include Cobol_common.Diagnostics_accumulator.MAKE
    (struct
      type t = diagnostics
      let none = none
      let union = union
      let translate = translate
    end)

let of_config_verif c =
  match Cobol_common.Config.DIAG.decompose_verification_result c with
  | result, Some Error e ->
      { result; diags = add_error (Feature_error e) none }
  | result, Some Warning w ->
      { result; diags = add_warning (Feature_warning w) none }
  | result, None ->
      { result; diags = none }

let error_result result e = { result; diags = add_error e none }
