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

module Cobol_data = Cobol_data.OLD

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

let initialize_prog_env =
  let valid_picture_symbol = function
    | '0'..'9' | 'a'..'e' | 'A'..'E' | 'N' | 'P' | 'R' | 'S' | 'V'
    | 'X' | 'Z' | 'n' | 'p' | 'r' | 's' | 'v' | 'x' | 'z'
    | '+' | '-' | ',' | '.' | '/' | ';' | '(' | ')' | '='
    | '\'' |'"' | ' ' -> false
    | 'L' | 'G' | 'l' | 'g'           (* <- TODO: Check dialect for those *)
    | _ -> true
  in
  let special_names_clause_folder = object
    inherit [Cobol_data.PROG_ENV.t with_diags] Cobol_ptree.Visitor.folder

    method! fold_special_names_clause' { loc; payload = clause } =
      acc_result @@ fun env -> match clause with
      | DecimalPointIsComma ->
          Visitor.skip @@
          DIAGS.result Cobol_data.PROG_ENV.{ env with decimal_point = ',' }
      | CurrencySign { sign = (Alphanum (s, _) | National s);
                       picture_symbol = None }
      | CurrencySign { picture_symbol = Some (Alphanum (s, _) | National s); _ }
        when String.length s != 1 ->
          Visitor.skip @@
          DIAGS.error_result env ~loc "%s@ is@ not@ a@ valid@ PICTURE@ symbol." s
      | CurrencySign { sign = Alphanum (s, _) | National s;
                       picture_symbol = None }
      | CurrencySign { picture_symbol = Some (Alphanum (s, _) | National s); _ }
        when not (valid_picture_symbol s.[0]) ->
          Visitor.skip @@
          DIAGS.error_result env ~loc "%c@ is@ not@ a@ valid@ PICTURE@ symbol."
            s.[0]
      | CurrencySign { sign = Alphanum (s, _) | National s;
                       picture_symbol = None }
      | CurrencySign { picture_symbol = Some (Alphanum (s, _) | National s); _ } ->
          Visitor.skip @@
          DIAGS.result { env with
                         currency_signs = CharSet.add s.[0] env.currency_signs }
      | _ ->                       (* TODO: other clauses? *)
          Visitor.proceed @@      (* may report unfinished visitor warnings *)
          DIAGS.result env
  end in
  let ensure_currency_sign_is_defined env =
    (* Currency sign defaults to '$' *)
    if CharSet.is_empty env.Cobol_data.PROG_ENV.currency_signs
    then Cobol_data.PROG_ENV.{ env with currency_signs = CharSet.singleton '$' }
    else env
  in
  fun env_div base_env ->
    DIAGS.map_result ~f:ensure_currency_sign_is_defined @@
    Cobol_ptree.Visitor.fold_environment_division'_opt
      special_names_clause_folder env_div @@
    DIAGS.result base_env

(* TODO: avoid returning `options with_diags` *)
let for_compilation_unit_old =
  let build_env ?parent_env name env =
    let prog_env = Cobol_data.PROG_ENV.make ?parent:parent_env ~&name in
    Visitor.skip @@
    DIAGS.map_result ~f:Option.some (initialize_prog_env env prog_env)
  in
  let env_builder = object
    inherit [_] Cobol_ptree.Visitor.folder
    method! fold_program_unit { program_name = name; program_env = env; _ } =
      acc_result @@ fun parent_env -> build_env name env ?parent_env
    method! fold_function_unit f =
      acc_result @@ fun _ -> build_env f.function_name f.function_env
    method! fold_method_definition m =
      acc_result @@ fun _ -> build_env m.method_name m.method_env
    method! fold_class_definition' { payload = c; _ } =
      acc_result @@ fun _ -> build_env c.class_name c.class_env
    method! fold_interface_definition' { payload = i; _ } =
      acc_result @@ fun _ -> build_env i.interface_name i.interface_env
    method! fold_factory_definition _ =
      Visitor.skip       (* NOTE: only lacks a name (but we shouldn't need it *)
    method! fold_instance_definition _ =
      Visitor.skip       (* NOTE: only lacks a name (but we shouldn't need it *)
  end in
  fun ~parents cu' ->
    Cobol_ptree.Visitor.fold_compilation_unit' env_builder cu' @@
    DIAGS.result (match parents with h :: _ -> Some h | [] -> None)
