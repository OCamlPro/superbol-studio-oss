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

open Cobol_ptree
open Cobol_common.Srcloc.INFIX
open Cobol_common.Diagnostics.TYPES

module Visitor = Cobol_common.Visitor
module CharSet = Cobol_common.Basics.CharSet
module DIAGS = Cobol_common.Diagnostics

let acc_result f { result; diags } =
  match f result with
  | Visitor.SkipChildren result ->
      Visitor.SkipChildren (DIAGS.with_more_diags result ~diags)
  | Visitor.DoChildren result ->
      Visitor.DoChildren (DIAGS.with_more_diags result ~diags)
  | Visitor.DoChildrenAndThen (result, f) ->
      Visitor.DoChildrenAndThen (DIAGS.with_more_diags result ~diags, f)

(* type error = *)
(*   | Invalid_picture_symbol of string with_loc *)

let base_config =
  Cobol_unit.Types.{ unit_decimal_point = '.';
                     unit_currency_signs = CharSet.empty }

let valid_picture_symbol = function
  | '0'..'9' | 'a'..'e' | 'A'..'E' | 'N' | 'P' | 'R' | 'S' | 'V'
  | 'X' | 'Z' | 'n' | 'p' | 'r' | 's' | 'v' | 'x' | 'z'
  | '+' | '-' | ',' | '.' | '/' | ';' | '(' | ')' | '='
  | '\'' |'"' | ' ' -> false
  | 'L' | 'G' | 'l' | 'g'                 (* <- TODO: Check dialect for those *)
  | _ -> true

let ensure_currency_sign_is_defined (config: Cobol_unit.Types.unit_config) =
  (* Currency sign defaults to '$' *)
  if CharSet.is_empty config.unit_currency_signs
  then { config with unit_currency_signs = CharSet.singleton '$' }
  else config

let special_names_clause_folder = object
  inherit [Cobol_unit.Types.unit_config with_diags] Cobol_ptree.Visitor.folder

  method! fold_special_names_clause' { loc; payload = clause } =
    acc_result @@ fun (config: Cobol_unit.Types.unit_config) -> match clause with
    | DecimalPointIsComma ->
        Visitor.skip @@
        DIAGS.result { config with unit_decimal_point = ',' }
    | CurrencySign { sign = (Alphanum (s, _) | National s);
                     picture_symbol = None }
    | CurrencySign { picture_symbol = Some (Alphanum (s, _) | National s); _ }
      when String.length s != 1 ->
        Visitor.skip @@
        DIAGS.error_result config ~loc "%s@ is@ not@ a@ valid@ PICTURE@ symbol." s
    | CurrencySign { sign = Alphanum (s, _) | National s;
                     picture_symbol = None }
    | CurrencySign { picture_symbol = Some (Alphanum (s, _) | National s); _ }
      when not (valid_picture_symbol s.[0]) ->
        Visitor.skip @@
        DIAGS.error_result config ~loc "%c@ is@ not@ a@ valid@ PICTURE@ symbol."
          s.[0]
    | CurrencySign { sign = Alphanum (s, _) | National s;
                     picture_symbol = None }
    | CurrencySign { picture_symbol = Some (Alphanum (s, _) | National s); _ } ->
        Visitor.skip @@
        DIAGS.result { config with
                       unit_currency_signs =
                         CharSet.add s.[0] config.unit_currency_signs }
    | _ ->                           (* TODO: other clauses? *)
        Visitor.proceed @@          (* may report unfinished visitor warnings *)
        DIAGS.result config
end

let initialize_config environment_division =
  DIAGS.result base_config |>
  Cobol_ptree.Visitor.fold_environment_division'_opt
    special_names_clause_folder environment_division |>
  DIAGS.map_result ~f:ensure_currency_sign_is_defined

let build_config environment_division parent_config =
  Visitor.skip @@ match parent_config with
  | None ->
      DIAGS.map_result ~f:Option.some (initialize_config environment_division)
  | Some config ->
      (* TODO: Check no configuration section for nested/contained units. *)
      DIAGS.result (Some config)

let config_builder = object
  inherit [_] Cobol_ptree.Visitor.folder
  method! fold_program_unit p =
    acc_result @@ build_config p.program_env
  method! fold_function_unit f =
    acc_result @@ build_config f.function_env
  method! fold_method_definition m =
    acc_result @@ build_config m.method_env
  method! fold_class_definition' c =
    acc_result @@ build_config ~&c.class_env
  method! fold_interface_definition' i =
    acc_result @@ build_config ~&i.interface_env
  method! fold_factory_definition c =
    acc_result @@ build_config c.factory_env
  method! fold_instance_definition i =
    acc_result @@ build_config i.instance_env
end

let of_compilation_unit ?parent_config cu' =
  DIAGS.result parent_config |>
  Cobol_ptree.Visitor.fold_compilation_unit' config_builder cu' |>
  DIAGS.map_result ~f:begin function
    | Some r -> r
    | None -> ensure_currency_sign_is_defined base_config
  end
