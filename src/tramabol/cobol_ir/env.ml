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

open Cobol_ptree                                                 (* for terms *)
open Cobol_unit.Types
open Types

open Syntax

(* --- *)

let error e = Error (NEL.one e)

module TYPES = struct
  module CONST_REF = struct
    type t = literal
    let equal a b = Cobol_ptree.compare_literal a b = 0
    let hash a = Hashtbl.hash a
  end
  module CONST_TABLE = Ephemeron.K1.Make (CONST_REF)

  type ('f, 'r, 'module_memory) env =
    {
      named_fields: 'f fields_map;
      const_fields: 'f immutable_field CONST_TABLE.t;
      vm: ('f, 'r, 'module_memory) value_manager;
    }
end
open TYPES

let lookup_named_field (qn: qualname) env =
  FIELDS_MAP.find qn env.named_fields

let literal_field (lit: literal with_loc) env : (_ immutable_field, _) result =
  match CONST_TABLE.find_opt env.const_fields ~&lit with
  | Some f ->
      Ok f
  | None ->
      let* f = env.vm.create_field_from_literal lit in
      CONST_TABLE.add env.const_fields ~&lit f;
      Ok f

let make_literal env lit =
  let* f = literal_field lit env in
  Ok (Field_constant f)

let resolve_qualname env qn =
  try
    Ok (Field_in_memory (lookup_named_field ~&qn env))
  with
  | Not_found ->
      error @@ Undefined { stuff = Data_reference ~&qn; loc = ~@qn }
  | Cobol_unit.Resolver_map.Ambiguous candidates ->
      error @@ Ambiguous { stuff = Data_reference ~&qn; loc = ~@qn;
                           candidates = Lazy.force candidates }

let resolve_qualident env qi =
  match ~&qi with
  | { ident_name = qn; ident_subscripts = [] } ->
      resolve_qualname env qn
  | _ ->
      error @@ Unsupported { stuff = Term (QualIdent ~&qi); loc = ~@qi }

let resolve_term: type k. _ env -> k term with_loc -> _ = fun env t ->
  match ~&t with
  | Name _
  | Qual _ as qn ->
      resolve_qualname env (qn &@<- t)
  | QualIdent qi ->
      resolve_qualident env (qi &@<- t)
  | Alphanum _
  | Boolean _
  | Fixed _
  | Floating _
  | Integer _
  | National _ as lit ->
      make_literal env (lit &@<- t)
  | NumFig _
  | Fig _
  | Address _
  | Counter _
  | InlineCall _
  | InlineInvoke _
  | LengthOf _
  | ObjectView _
  | ObjectRef _
  | RefMod _
  | ScalarRefMod _
  | StrConcat _
  | Concat _ ->
      error @@ Unsupported { stuff = Term ~&t; loc = ~@t }
