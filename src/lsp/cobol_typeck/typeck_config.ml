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

module Visitor = Cobol_common.Visitor
module CharSet = Cobol_common.Basics.CharSet
module DIAGS = Cobol_common.Diagnostics

type output = Cobol_unit.Types.unit_config

type acc =
  {
    config: output;
    diags: Typeck_diagnostics.t;
  }

let default_config =
  Cobol_unit.Types.{
    unit_decimal_point = '.';
    unit_currency_signs = CharSet.empty
  }

let init config =
  {
    config;
    diags = Typeck_diagnostics.none;
  }

let error acc e = { acc with diags = Config_error e :: acc.diags }
let new_config acc config = { acc with config }

let valid_picture_symbol = function
  | '0'..'9' | 'a'..'e' | 'A'..'E' | 'N' | 'P' | 'R' | 'S' | 'V'
  | 'X' | 'Z' | 'n' | 'p' | 'r' | 's' | 'v' | 'x' | 'z'
  | '+' | '-' | ',' | '.' | '/' | ';' | '(' | ')' | '='
  | '\'' |'"' | ' ' -> false
  | 'L' | 'G' | 'l' | 'g'                 (* <- TODO: Check dialect for those *)
  | _ -> true

let ensure_currency_sign_is_defined ({ config; _ } as acc) =
  (* Currency sign defaults to '$' *)
  if CharSet.is_empty config.unit_currency_signs then
    new_config acc
      { config with unit_currency_signs = CharSet.singleton '$' }
  else
    acc

let special_names_clause_folder = object
  inherit [acc] Cobol_ptree.Visitor.folder

  method! fold_special_names_clause' { loc; payload = clause }
      ({ config; _ } as acc) =
    Visitor.skip @@ match clause with
    | DecimalPointIsComma ->
        new_config acc
          { config with unit_decimal_point = ',' }
    (* CHECKME: are hexadecimal-alphanums allowed? *)
    | CurrencySign { sign = (Alphanum { str; _ } | National str);
                     picture_symbol = None }
    | CurrencySign { picture_symbol = Some (Alphanum { str; _ } |
                                            National str); _ }
      when String.length str != 1 ->
        error acc (Invalid_picture_symbol (str &@ loc))
    | CurrencySign { sign = Alphanum { str; _ } | National str;
                     picture_symbol = None }
    | CurrencySign { picture_symbol = Some (Alphanum { str; _ } |
                                            National str); _ }
      when not (valid_picture_symbol str.[0]) ->
        error acc (Invalid_picture_symbol (String.sub str 0 1 &@ loc))
    | CurrencySign { sign = Alphanum { str; _ } | National str;
                     picture_symbol = None }
    | CurrencySign { picture_symbol = Some (Alphanum { str; _ } |
                                            National str); _ } ->
        new_config acc
          { config with unit_currency_signs =
                          CharSet.add str.[0] config.unit_currency_signs }
    | _ ->                                             (* TODO: other clauses? *)
        acc
end

let initialize_config environment_division =
  init default_config |>
  Cobol_ptree.Visitor.fold_environment_division'_opt
    special_names_clause_folder environment_division |>
  ensure_currency_sign_is_defined

let build_config environment_division parent_config =
  Visitor.skip @@ match parent_config with
  | None ->
      Some (initialize_config environment_division)
  | Some config ->
      (* TODO: Check no configuration section for nested/contained units. *)
      Some config

let config_builder = object
  inherit [_] Cobol_ptree.Visitor.folder
  method! fold_program_unit p = build_config p.program_env
  method! fold_function_unit f = build_config f.function_env
  method! fold_method_definition m = build_config m.method_env
  method! fold_class_definition' c = build_config ~&c.class_env
  method! fold_interface_definition' i = build_config ~&i.interface_env
  method! fold_factory_definition c = build_config c.factory_env
  method! fold_instance_definition i = build_config i.instance_env
end

let of_compilation_unit ?parent_config cu' =
  Option.map init parent_config |>
  Cobol_ptree.Visitor.fold_compilation_unit' config_builder cu' |>
  (function
    | Some r -> r
    | None -> ensure_currency_sign_is_defined (init default_config)) |>
  (function { config; diags } -> config, diags)
