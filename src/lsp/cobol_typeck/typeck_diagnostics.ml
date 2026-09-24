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

open Cobol_common.Srcloc.TYPES

module DIAGS = Cobol_common.Diagnostics
module LIST = Cobol_common.Basics.LIST

(** more general diagnostics *)
type diagnostic =
  | Config_error of Typeck_config_diagnostics.error
  | Data_error of Typeck_data_diagnostics.error
  | Data_warning of Typeck_data_diagnostics.warning
  | Proc_error of Typeck_procedure_diagnostics.error
  | Condition_error of Typeck_condition_diagnostics.error
  | Condition_warning of Typeck_condition_diagnostics.warning
  | Dialect_feature_used of
      {
        feature: unit Cobol_config.feature;
        usage_loc: srcloc;
      }

type diagnostics = diagnostic list
type t = diagnostics
let none: diagnostics = []
let union d1 d2 = LIST.append ~loc:__LOC__ d2 d1

let diagnostic_severity = function
  | Config_error _
  | Data_error _
  | Proc_error _ 
  | Condition_error _ ->
      `Print DIAGS.Error
  | Data_warning _ 
  | Condition_warning _ ->
      `Print DIAGS.Warn
  | Dialect_feature_used { feature = _; _ } ->
      (* TODO: explicitly add a feature -> support mapping in [config] *)
      `Ignore

let diagnostic_loc = function
  | Config_error e ->
      Typeck_config_diagnostics.error_loc e
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
  | Dialect_feature_used { usage_loc = loc; _ } ->
      Some loc

let pp_diagnostic ppf = function
  | Config_error e ->
      Typeck_config_diagnostics.pp_error ppf e
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
  | Dialect_feature_used { feature; _ } ->
      Pretty.print ppf "%(%)@ used" feature#short

let translate diagnostics =
  (* Temporary hack: reverse errors list so order of generated diagnostics
     corresponds to order of emission in the code below. *)
  List.fold_left begin fun diags d ->
    match diagnostic_severity d with
    | `Ignore ->
        diags
    | `Print s ->
        DIAGS.Acc.diag s diags ?loc:(diagnostic_loc d) "%a" pp_diagnostic d
    (* | `Feature f -> *)
  end DIAGS.Set.none (List.rev diagnostics)
