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
open Cir_types
open Types

open Syntax

(* --- *)

let error e = Error (NEL.one e)

let rec resolve_data_reference ~loc ?(subscripts = []) env qn =
  match Env.lookup_named_field ~&qn env, subscripts with
  | Direct_access f, [] ->
      Ok { field_ref = Field_in_memory { field = Fixed_field f.fixed_field;
                                         field_info = f.fixed_field_info };
           field_ref_loc = loc }
  | Direct_access _, subscripts ->
      let locs = NEL.of_rev_list @@ List.rev_map (~@) subscripts in
      Error.extra ~locs @@
      Data_reference_subscripts { qn = ~&qn;
                                  amount = List.length subscripts }
  | Indirect_access { ranges; base_field }, subscripts ->
      let expected = NEL.length ranges and given = List.length subscripts in
      if given < expected then
        Error.missing ~loc:~@qn @@
        Data_reference_subscripts { qn = ~&qn; amount = expected - given }
      else if given > expected then
        let extra_subscripts = EzList.drop expected subscripts in
        let locs = NEL.of_rev_list @@ List.rev_map (~@) extra_subscripts in
        Error.extra ~locs @@
        Data_reference_subscripts { qn = ~&qn; amount = given - expected }
      else
        resolve_indirect_access_subscripts ~loc env ranges subscripts base_field
  | exception Not_found ->
      error @@ Undefined { stuff = Data_reference ~&qn; loc = ~@qn }
  | exception Cobol_unit.Resolver_map.Ambiguous candidates ->
      error @@ Ambiguous { stuff = Data_reference ~&qn; loc = ~@qn;
                           candidates = Lazy.force candidates }

(* Assumes [ranges] and [subscripts] have the same length. *)
and resolve_indirect_access_subscripts ~loc env ranges subscripts base_field =
  let rev_ranges = NEL.rev_to_list ranges
  and rev_subscripts = List.rev subscripts in
  let* field, _ =
    List.fold_left2 begin fun acc range (subscript: subscript with_loc) ->
      let* (inner_field, inner_stride) as acc = acc in
      match range with
      | Fixed_range { max } ->
          let* index = resolve_subscript env subscript in
          Ok (Table_field { cell_first_field = inner_field;
                            cell_index_field = index;
                            cell_stride = inner_stride;
                            cell_index_max = max }, inner_stride * max)
      | _ ->                                                           (* TODO *)
          Ok acc
    end (Ok (Fixed_field base_field.fixed_field, 1)) rev_ranges rev_subscripts
  in
  Ok { field_ref = Field_in_memory { field;
                                     field_info = base_field.fixed_field_info };
       field_ref_loc = loc }

and resolve_subscript env s =
  match ~&s with
  | SubSExpr e ->
      resolve_expr env e
  | _ ->
      error @@ Unsupported { stuff = Subscript s; loc = ~@s }

and resolve_expr env e =
  match ~&e with
  | Atom term ->
      resolve_term env (term &@<- e)
  | _ ->
      error @@ Unsupported { stuff = Expression e; loc = ~@e }

and resolve_qualident env qi =
  resolve_data_reference ~subscripts:~&qi.ident_subscripts env ~&qi.ident_name
    ~loc:~@qi

and resolve_refmod env (r: Cobol_ptree.refmod) =
  let* refmod_left = resolve_expr env r.refmod_left
  and* refmod_length =
    match r.refmod_length with
    | Some l -> Result.map Option.some @@ resolve_expr env l
    | None -> Ok None
  in
  Ok { refmod_left; refmod_length }

and resolve_term
  : type k. (('f, _, _) env -> k term with_loc ->
             ('f field_reference, errors) result) = fun env t ->
  match ~&t with
  | Name _
  | Qual _ as qn ->
      resolve_data_reference ~loc:~@t env (qn &@<- t)
  | QualIdent qi ->
      resolve_qualident env (qi &@<- t)
  | Alphanum _
  | Boolean _
  | Fixed _
  | Floating _
  | Integer _
  | NumFig _
  | Fig _
  | StrConcat _
  | Concat _
  | National _ as lit ->
      Literal.create ~builder:env.builder (lit &@<- t)
  | RefMod _
  | ScalarRefMod _ ->
      error @@ Unexpected { stuff = Reference_modification; loc = ~@t }
  | Address _
  | Counter _
  | InlineCall _
  | InlineInvoke _
  | LengthOf _
  | ObjectView _
  | ObjectRef _ ->
      error @@ Unsupported { stuff = Term ~&t; loc = ~@t }

let resolve_field_reference = resolve_term                           (* alias *)

let resolve_data_reference
  : type k. (('f, _, _) env -> k term with_loc ->
             ('f data_reference, errors) result) = fun env t ->
  match ~&t with
  | RefMod (ident, refmod) ->
      let* { field_ref; field_ref_loc } = resolve_term env ident
      and* refmod = resolve_refmod env refmod in
      Ok { data_field = field_ref;
           data_ref_loc = field_ref_loc;
           data_refmod = Some refmod }
  | ScalarRefMod (ident, refmod) ->
      let* { field_ref; field_ref_loc } = resolve_term env ident
      and* refmod = resolve_refmod env refmod in
      Ok { data_field = field_ref;
           data_ref_loc = field_ref_loc;
           data_refmod = Some refmod }
  | _ ->
      let* { field_ref; field_ref_loc } = resolve_term env t in
      Ok { data_field = field_ref;
           data_ref_loc = field_ref_loc;
           data_refmod = None }
