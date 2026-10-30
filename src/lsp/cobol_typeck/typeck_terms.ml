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

(** Typing of terms in PROCEDURE DIVISION: references to data items are
    resolved, and literals are evaluated. *)

open Cobol_procedure.Types
open Typeck_procedure_monad
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
module NEL = Cobol_common.Basics.NEL

(* --- Data references *)

(** Registers the (indirect) references to the qualifiers of a qualified name,
    e.g, [A] in [B IN A] (see {!Typeck_outputs.references_in_unit}). *)
let rec register_qualifiers ~data_definitions (qn : Cobol_ptree.qualname) acc =
  match qn with
  | Name _ -> acc
  | Qual (_, qn) ->
      Typeck_references.register_data_qualname ~data_definitions
        (qn &@ Typeck_references.baseloc_of_qualname qn)
        acc
      |> register_qualifiers ~data_definitions qn

let typeck_data_reference (qn : Cobol_ptree.qualname with_loc)
  : data_reference Typeck_procedure_monad.t =
  fun env acc ->
  let r, acc =
    (* TODO: report undefined data-names (see
       {!Typeck_references.resolve_record_name})
       by removing the assume_partial_data_definitions flag *)
    Typeck_references.resolve_data_qualname qn acc
      ~data_definitions:env.data_definitions
      ~assume_partial_data_definitions:true
  in
  r, register_qualifiers ~&qn acc ~data_definitions:env.data_definitions

let typeck_record_reference (n : Cobol_ptree.name with_loc)
  : record_reference Typeck_procedure_monad.t =
  fun env acc ->
  Typeck_references.resolve_record_name n acc
    ~data_definitions:env.data_definitions
    ~assume_partial_data_definitions:true

(* --- Literals *)

let literal_diagnostics errors =
  List.fold_left
    (fun diags e ->
       Typeck_diagnostics.Data_error
         (Typeck_data_diagnostics.Data_literal_error e)
       :: diags
    )
    [] (NEL.rev_to_list errors)

let literal_errors errors : 'a or_error Typeck_procedure_monad.t =
  return ~diags:(literal_diagnostics errors) (Error ())

let of_literal_result (r : ('a with_loc, Cobol_data.Types.errors) result)
  : 'a or_error Typeck_procedure_monad.t =
  match r with
  | Ok v -> return (Ok ~&v)
  | Error errors -> literal_errors errors

let typeck_literal (l : Cobol_ptree.literal with_loc)
  : literal or_error Typeck_procedure_monad.t =
  of_literal_result (Cobol_data.Literal.value l)

let typeck_intlit (l : Cobol_ptree.int_ Cobol_ptree.term with_loc)
  : Cobol_data.Types.integer_value or_error Typeck_procedure_monad.t =
  match ~&l with
  | Integer i -> of_literal_result (Cobol_data.Literal.integer_value (i &@<- l))
  | NumFig Zero -> return (Ok Z.zero)

let typeck_alphanum (l : Cobol_ptree.alnum_ Cobol_ptree.term with_loc)
  : Cobol_data.Types.alphanum_value or_error Typeck_procedure_monad.t =
  match ~&l with
  | Alphanum a ->
      of_literal_result (Cobol_data.Literal.alphanum_value (a &@<- l))

(** Integers given without location in the parse tree (subscript offsets,
    segment numbers).  As they only consist of digits, invalid integers are
    reported and replaced with zero. *)
let typeck_integer ~loc (i : Cobol_ptree.integer)
  : Cobol_data.Types.integer_value Typeck_procedure_monad.t =
  let lit, errors = Cobol_data.Literal.integer_with_dummy_fallback (i &@ loc) in
  let diags = Option.fold ~none:[] ~some:literal_diagnostics errors in
  return ~diags ~&lit.Cobol_data.Types.int_value

(** Names of entry points, given as alphanumeric literals.  Invalid
    (hexadecimal) literals are reported, and their text is kept as name. *)
let typeck_entry_name (a : Cobol_ptree.alphanum with_loc)
  : string with_loc Typeck_procedure_monad.t =
  let lit, errors = Cobol_data.Literal.alphanum_with_dummy_fallback a in
  let diags = Option.fold ~none:[] ~some:literal_diagnostics errors in
  return ~diags (~&lit.Cobol_data.Types.alphanum_value &@<- lit)

(* --- Identifiers and expressions *)

let rec typeck_ident (i : Cobol_ptree.ident with_loc)
  : ident Typeck_procedure_monad.t =
  match ~&i with
  | ( Address _ | Counter _ | InlineCall _ | InlineInvoke _ | ObjectView _
    | ObjectRef _ | QualIdent _ ) as b ->
      let+ ident_base = typeck_base_ident (b &@<- i) in
      { ident_base; ident_refmod = None }
  | RefMod (b, r) ->
      let+ ident_base = typeck_base_ident b
      and+ r = typeck_refmod r in
      { ident_base; ident_refmod = Some r }
  | ScalarRefMod (b, r) ->
      let+ ident_base = typeck_scalar_base_ident b
      and+ r = typeck_refmod r in
      { ident_base; ident_refmod = Some r }

and typeck_base_ident (i : Cobol_ptree.base_ident_ Cobol_ptree.term with_loc)
  : base_ident Typeck_procedure_monad.t =
  match ~&i with
  | Address a ->
      let+ a = typeck_address (a &@<- i) in
      Address a
  | Counter c -> return (Counter c)
  | InlineCall c ->
      let+ c = typeck_inline_call (c &@<- i) in
      InlineCall c
  | InlineInvoke v ->
      let+ v = typeck_inline_invocation (v &@<- i) in
      InlineInvoke v
  | ObjectView o ->
      let+ o = typeck_object_view (o &@<- i) in
      ObjectView o
  | ObjectRef o -> return (ObjectRef o)
  | QualIdent q ->
      let+ q = typeck_qualident q in
      QualIdent q

and typeck_scalar_base_ident
    (i : Cobol_ptree.scalar_base_ident_ Cobol_ptree.term with_loc)
  : base_ident Typeck_procedure_monad.t =
  match ~&i with
  | ( Address _ | Counter _ | InlineCall _ | InlineInvoke _ | ObjectRef _
    | QualIdent _ ) as b ->
      typeck_base_ident (b &@<- i)

and typeck_qualident
    ({ ident_name; ident_subscripts } : Cobol_ptree.qualident) =
  let+ ident_data_ref = typeck_data_reference ident_name
  and+ ident_subscripts = map_list typeck_subscript' ident_subscripts in
  { ident_data_ref; ident_subscripts }

and typeck_subscript (s : Cobol_ptree.subscript with_loc)
  : subscript Typeck_procedure_monad.t =
  match ~&s with
  | SubSAll -> return SubSAll
  | SubSExpr e ->
      let+ e = typeck_expr' e in
      SubSExpr e
  | SubSIdx (n, sign, i) ->
      let+ i = typeck_integer ~loc:~@s i in
      SubSIdx (n, match sign with SgnPositive -> i | SgnNegative -> Z.neg i)

and typeck_subscript' (s : Cobol_ptree.subscript with_loc)
  : subscript with_loc Typeck_procedure_monad.t =
  let+ s' = typeck_subscript s in
  s' &@<- s

and typeck_refmod ({ refmod_left; refmod_length } : Cobol_ptree.refmod) =
  let+ refmod_left = typeck_expr' refmod_left
  and+ refmod_length = map_option typeck_expr' refmod_length in
  { refmod_left; refmod_length }

and typeck_address (a : Cobol_ptree.address with_loc)
  : address Typeck_procedure_monad.t =
  match ~&a with
  | DataAddress i ->
      let+ i = typeck_ident (i &@<- a) in
      DataAddress i
  | ProgAddress i ->
      let+ i = typeck_ident_or_literal (i &@<- a) in
      ProgAddress i

and typeck_inline_call (c : Cobol_ptree.inline_call with_loc)
  : inline_call Typeck_procedure_monad.t =
  match ~&c with
  | CallFunc { func; args } ->
      let+ args = map_list typeck_effective_arg args in
      CallFunc { func; args }
  | CallGenericIntrinsic { func; args } ->
      let+ args = map_list typeck_effective_arg args in
      CallGenericIntrinsic { func; args }
  | CallTrim { arg; tip } ->
      let+ arg = typeck_effective_arg arg in
      CallTrim { arg; tip }
  | CallLength { arg; physical } ->
      let+ arg = typeck_ident_or_nonnum (arg &@<- c) in
      CallLength { arg; physical }
  | CallNumvalC args ->
      let+ args = map_list (fun a -> typeck_ident_or_nonnum (a &@<- c)) args in
      CallNumvalC args
  | CallLocaleDate args ->
      let+ args = typeck_locale_func_args args in
      CallLocaleDate args
  | CallLocaleTime args ->
      let+ args = typeck_locale_func_args args in
      CallLocaleTime args
  | CallLocaleTimeFromSeconds args ->
      let+ args = typeck_locale_func_args args in
      CallLocaleTimeFromSeconds args
  | CallFormattedDatetime args ->
      let+ args = typeck_formatted_func_args args in
      CallFormattedDatetime args
  | CallFormattedTime args ->
      let+ args = typeck_formatted_func_args args in
      CallFormattedTime args

and typeck_locale_func_args
    ({ locale_func_args; locale_func_locale } : Cobol_ptree.locale_func_args) =
  let+ locale_func_args = typeck_effective_arg locale_func_args in
  { locale_func_args; locale_func_locale }

and typeck_formatted_func_args
    ({ formatted_func_args; formatted_func_system_offset }
     : Cobol_ptree.formatted_func_args) =
  let+ formatted_func_args =
    map_list typeck_effective_arg formatted_func_args
  in
  { formatted_func_args; formatted_func_system_offset }

and typeck_effective_arg (a : Cobol_ptree.effective_arg)
  : effective_arg Typeck_procedure_monad.t =
  match a with
  | ArgExpr e ->
      let+ e = typeck_expr' e in
      ArgExpr e
  | ArgOmitted -> return ArgOmitted

and typeck_inline_invocation (v : Cobol_ptree.inline_invocation with_loc)
  : inline_invocation Typeck_procedure_monad.t =
  let { invoke_class; invoke_meth; invoke_args }
    : Cobol_ptree.inline_invocation =
    ~&v
  in
  let+ invoke_class = typeck_ident (invoke_class &@<- v)
  and+ invoke_meth = typeck_literal (invoke_meth &@<- v)
  and+ invoke_args = map_list typeck_effective_arg invoke_args in
  { invoke_class; invoke_meth; invoke_args }

and typeck_object_view (o : Cobol_ptree.object_view with_loc)
  : object_view Typeck_procedure_monad.t =
  let { object_view_ident; object_view_spec } : Cobol_ptree.object_view = ~&o in
  let+ object_view_ident = typeck_ident (object_view_ident &@<- o) in
  { object_view_ident; object_view_spec }

and typeck_expr' (e : Cobol_ptree.expr with_loc)
  : expr with_loc Typeck_procedure_monad.t =
  let+ e' =
    match ~&e with
    | Atom a ->
        let+ a = typeck_scalar (a &@<- e) in
        Atom a
    | Unop (o, e) ->
        let+ e = typeck_expr' e in
        Unop (o, e)
    | Binop (a, o, b) ->
        let+ a = typeck_expr' a
        and+ b = typeck_expr' b in
        Binop (a, o, b)
  in
  e' &@<- e

and typeck_scalar (s : Cobol_ptree.scalar with_loc)
  : scalar Typeck_procedure_monad.t =
  match ~&s with
  | ( Address _ | Counter _ | InlineCall _ | InlineInvoke _ | ObjectRef _
    | QualIdent _ | RefMod _ | ScalarRefMod _ ) as i ->
      let+ i = typeck_ident (i &@<- s) in
      ScalarIdent i
  | ( Alphanum _ | Boolean _ | Fixed _ | Floating _ | Integer _ | National _
    | NumFig _ | Fig _ | StrConcat _ | Concat _ ) as l ->
      let+ l = typeck_literal (l &@<- s) in
      ScalarLiteral l
  | LengthOf i ->
      let+ i = typeck_ident_or_literal (i &@<- s) in
      LengthOf i

and typeck_ident_or_literal (t : Cobol_ptree.ident_or_literal with_loc)
  : ident_or_literal Typeck_procedure_monad.t =
  match ~&t with
  | ( Address _ | Counter _ | InlineCall _ | InlineInvoke _ | ObjectView _
    | ObjectRef _ | QualIdent _ | RefMod _ | ScalarRefMod _ ) as i ->
      let+ i = typeck_ident (i &@<- t) in
      Ident i
  | ( Alphanum _ | Boolean _ | Fixed _ | Floating _ | Integer _ | National _
    | NumFig _ | Fig _ | StrConcat _ | Concat _ ) as l ->
      let+ l = typeck_literal (l &@<- t) in
      Literal l

and typeck_ident_or_nonnum (t : Cobol_ptree.ident_or_nonnum with_loc)
  : ident_or_nonnum Typeck_procedure_monad.t =
  match ~&t with
  | ( Address _ | Counter _ | InlineCall _ | InlineInvoke _ | ObjectView _
    | ObjectRef _ | QualIdent _ | RefMod _ | ScalarRefMod _ ) as i ->
      let+ i = typeck_ident (i &@<- t) in
      Ident i
  | (Alphanum _ | Boolean _ | National _ | Fig _ | StrConcat _ | Concat _) as l
    ->
      let+ l = typeck_literal (l &@<- t) in
      Literal l

let typeck_ident_or_strlit (t : Cobol_ptree.ident_or_strlit with_loc)
  : ident_or_strlit Typeck_procedure_monad.t =
  match ~&t with
  | ( Address _ | Counter _ | InlineCall _ | InlineInvoke _ | ObjectView _
    | ObjectRef _ | QualIdent _ | RefMod _ | ScalarRefMod _ ) as i ->
      let+ i = typeck_ident (i &@<- t) in
      Ident i
  | (Alphanum _ | National _ | Fig _ | StrConcat _) as l ->
      let+ l = typeck_literal (l &@<- t) in
      Literal l

let typeck_ident_or_intlit (t : Cobol_ptree.ident_or_intlit with_loc)
  : ident_or_intlit Typeck_procedure_monad.t =
  match ~&t with
  | ( Address _ | Counter _ | InlineCall _ | InlineInvoke _ | ObjectView _
    | ObjectRef _ | QualIdent _ | RefMod _ | ScalarRefMod _ ) as i ->
      let+ i = typeck_ident (i &@<- t) in
      Ident i
  | (Integer _ | NumFig _) as l ->
      let+ l = typeck_intlit (l &@<- t) in
      Literal l

let typeck_ident_or_alphanum (t : Cobol_ptree.ident_or_alphanum with_loc)
  : ident_or_alphanum Typeck_procedure_monad.t =
  match ~&t with
  | ( Address _ | Counter _ | InlineCall _ | InlineInvoke _ | ObjectView _
    | ObjectRef _ | QualIdent _ | RefMod _ | ScalarRefMod _ ) as i ->
      let+ i = typeck_ident (i &@<- t) in
      Ident i
  | Alphanum _ as l ->
      let+ l = typeck_alphanum (l &@<- t) in
      Literal l

let typeck_ident' (i : Cobol_ptree.ident with_loc)
  : ident with_loc Typeck_procedure_monad.t =
  let+ i' = typeck_ident i in
  i' &@<- i
