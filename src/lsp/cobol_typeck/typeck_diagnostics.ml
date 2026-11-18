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


module DIAGS = Cobol_common.Diagnostics
module LIST = Cobol_common.Basics.LIST

(** more general diagnostics *)
type diagnostic =
  | Env_error of Typeck_env_diagnostics.error
  | Data_error of Typeck_data_diagnostics.error
  | Data_warning of Typeck_data_diagnostics.warning
  | Proc_error of Typeck_procedure_diagnostics.error
  | Condition_error of Typeck_condition_diagnostics.error
  | Condition_warning of Typeck_condition_diagnostics.warning
  | Dialect_feature_error of Cobol_dialect.Types.usage
  | Dialect_feature_warning of Cobol_dialect.Types.usage

type diagnostics = diagnostic list
type t = diagnostics
let none: diagnostics = []
let union d1 d2 = LIST.append ~loc:__LOC__ d2 d1

let diagnostic_severity = function
  | Env_error _
  | Data_error _
  | Proc_error _ 
  | Condition_error _
  | Dialect_feature_error _ ->
      `Print DIAGS.Error
  | Data_warning _ 
  | Condition_warning _
  | Dialect_feature_warning _ ->
      `Print DIAGS.Warn

let diagnostic_loc = function
  | Env_error e ->
      Typeck_env_diagnostics.error_loc e
  | Data_error e ->
      Typeck_data_diagnostics.error_loc e
  | Data_warning w ->
      Typeck_data_diagnostics.warning_loc w
  | Proc_error e ->
      Typeck_procedure_diagnostics.error_loc e
  | Condition_error e ->
      Typeck_condition_diagnostics.error_loc e
  | Condition_warning w ->
      Typeck_condition_diagnostics.warning_loc w
  | Dialect_feature_error { loc; _ }
  | Dialect_feature_warning { loc; _ } ->
      loc

let pp_diagnostic ppf = function
  | Env_error e ->
      Typeck_env_diagnostics.pp_error ppf e
  | Data_error e ->
      Typeck_data_diagnostics.pp_error ppf e
  | Data_warning w ->
      Typeck_data_diagnostics.pp_warning ppf w
  | Proc_error e ->
      Typeck_procedure_diagnostics.pp_error ppf e
  | Condition_error e ->
      Typeck_condition_diagnostics.pp_error ppf e
  | Condition_warning w ->
      Typeck_condition_diagnostics.pp_warning ppf w
  | Dialect_feature_error u
  | Dialect_feature_warning u ->
      Cobol_dialect.Diagnostics.pp_usage ppf u

let has_errors: diagnostics -> bool =
  List.exists begin function
    | Env_error _
    | Data_error _
    | Proc_error _ 
    | Condition_error _
    | Dialect_feature_error _ -> true
    | Data_warning _
    | Condition_warning _
    | Dialect_feature_warning _ -> false
  end

let count_errors: diagnostics -> int =
  List.fold_left begin fun acc -> function
    | Env_error _
    | Data_error _
    | Proc_error _ 
    | Condition_error _
    | Dialect_feature_error _ -> succ acc
    | Data_warning _
    | Condition_warning _
    | Dialect_feature_warning _ -> acc
  end 0

let count_warnings: diagnostics -> int =
  List.fold_left begin fun acc -> function
    | Env_error _
    | Data_error _
    | Proc_error _
    | Condition_error _
    | Dialect_feature_error _ -> acc
    | Data_warning _
    | Condition_warning _
    | Dialect_feature_warning _ -> succ acc
  end 0

let translate diagnostics =
  (* Temporary hack: reverse errors list so order of generated diagnostics
     corresponds to order of emission in the code below. *)
  List.fold_left begin fun diags d ->
    match diagnostic_severity d with
    | `Ignore ->
        diags
    | `Print s ->
        DIAGS.Acc.diag s diags ~loc:(diagnostic_loc d) "%a" pp_diagnostic d
    (* | `Feature f -> *)
  end DIAGS.Set.none (List.rev diagnostics)
