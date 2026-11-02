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

open Cobol_procedure.Types                                       (* for terms *)
open Cobol_common.Srcloc.TYPES
open Cir_types
open Types

open Syntax

(* --- *)

let error e = Error (NEL.one e)

let rec resolve_data_reference ~loc ?(subscripts = []) env qn =
  let* access = Env.lookup_named_field qn env in
  let f = access.access_field in
  match access.access_ranges, subscripts with
  | [], [] ->
      Ok { field_ref = Field_in_memory { field = Fixed_field f.fixed_field;
                                         field_info = f.fixed_field_info };
           field_ref_loc = loc }
  | [], subscripts ->
      let locs = NEL.of_rev_list @@ List.rev_map (~@) subscripts in
      Error.extra ~locs @@
      Data_reference_subscripts { qn = ~&qn;
                                  amount = List.length subscripts }
  | ranges, subscripts ->
      let expected = List.length ranges and given = List.length subscripts in
      if given < expected then
        Error.missing ~loc:~@qn @@
        Data_reference_subscripts { qn = ~&qn; amount = expected - given }
      else if given > expected then
        let extra_subscripts = EzList.drop expected subscripts in
        let locs = NEL.of_rev_list @@ List.rev_map (~@) extra_subscripts in
        Error.extra ~locs @@
        Data_reference_subscripts { qn = ~&qn; amount = given - expected }
      else
        resolve_indirect_access_subscripts ~loc env ranges subscripts f

(* Assumes [ranges] and [subscripts] have the same length. *)
and resolve_indirect_access_subscripts ~loc env ranges subscripts base_field =
  let rev_ranges = List.rev ranges
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
  | Atom s ->
      resolve_scalar env (s &@<- e)
  | _ ->
      error @@ Unsupported { stuff = Expression e; loc = ~@e }

and resolve_qualident env (qi: qualident with_loc) =
  resolve_data_reference env ~&qi.ident_data_ref.resolved_name
    ~subscripts:~&qi.ident_subscripts ~loc:~@qi

and resolve_refmod env (r: Cobol_procedure.Types.refmod) =
  let* refmod_left = resolve_expr env r.refmod_left
  and* refmod_length =
    match r.refmod_length with
    | Some l -> Result.map Option.some @@ resolve_expr env l
    | None -> Ok None
  in
  Ok { refmod_left; refmod_length }

and resolve_base_ident env (i: base_ident with_loc) =
  match ~&i with
  | QualIdent qi ->
      resolve_qualident env (qi &@<- i)
  | Address _
  | Counter _
  | InlineCall _
  | InlineInvoke _
  | ObjectView _
  | ObjectRef _ ->
      error @@ Unsupported { stuff = Base_ident ~&i; loc = ~@i }

and resolve_ident env (i: ident with_loc) =
  match ~&i with
  | { ident_base; ident_refmod = None } ->
      resolve_base_ident env (ident_base &@<- i)
  | { ident_refmod = Some _; _ } ->
      error @@ Unexpected { stuff = Reference_modification; loc = ~@i }

and resolve_scalar env (s: scalar with_loc) =
  match ~&s with
  | ScalarIdent i ->
      resolve_ident env (i &@<- s)
  | ScalarLiteral Ok lit ->
      Literal.create ~builder:env.builder (lit &@<- s)
  | ScalarLiteral Error ()                 (* invalid literal, already reported *)
  | LengthOf _ ->
      error @@ Unsupported { stuff = Scalar ~&s; loc = ~@s }

let resolve_ident_or_literal env (t: ident_or_literal with_loc) =
  match ~&t with
  | Ident i ->
      resolve_ident env (i &@<- t)
  | Literal Ok lit ->
      Literal.create ~builder:env.builder (lit &@<- t)
  | Literal Error () ->                  (* invalid literal, already reported *)
      error @@ Unsupported { stuff = Scalar (ScalarLiteral (Error ()));
                             loc = ~@t }

let resolve_field_reference = resolve_ident_or_literal               (* alias *)

let resolve_data_reference env (t: ident_or_literal with_loc) =
  match ~&t with
  | Ident { ident_base; ident_refmod = Some refmod } ->
      let* { field_ref; field_ref_loc } =
        resolve_base_ident env (ident_base &@<- t)
      and* refmod = resolve_refmod env refmod in
      Ok { data_field = field_ref;
           data_ref_loc = field_ref_loc;
           data_refmod = Some refmod }
  | _ ->
      let* { field_ref; field_ref_loc } = resolve_ident_or_literal env t in
      Ok { data_field = field_ref;
           data_ref_loc = field_ref_loc;
           data_refmod = None }

let resolve_condition env
    (c: Cobol_procedure.Types.condition or_error with_loc) =
  match ~&c with
  | Ok (Relation (subject, Eq, expr)) ->
      let* subject = resolve_expr env subject
      and* expr = resolve_expr env expr in
      Ok (false, subject, expr)
  | _ ->
      error @@ Unsupported { stuff = Condition c; loc = ~@c }
