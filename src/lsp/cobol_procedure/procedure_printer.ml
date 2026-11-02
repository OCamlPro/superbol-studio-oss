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

open Procedure_types
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Fmt
module Expression = Cobol_ptree.Unparse.Expression

(** [pp_or_error ~err pp] calls [pp] on [Ok _] or prints ["<err>"] on [Error ()]. *)
let pp_or_error ~err pp ppf = function
  | Ok x -> pp ppf x
  | Error () -> fmt "<%s>" ppf err

let pp_resolved_name pp_resolved : _ resolved_name Pretty.printer =
  Pretty.record
    [
      field "resolved-name" (fun x -> x.resolved_name) Cobol_ptree.pp_name';
      field "resolved" (fun x -> x.resolved) (pp_or_error ~err:"unresolved" pp_resolved);
    ]

let pp_resolved_qualname pp_resolved : _ resolved_qualname Pretty.printer =
  Pretty.record
    [
      field "resolved-name" (fun x -> x.resolved_name) Cobol_ptree.pp_qualname';
      field "resolved" (fun x -> x.resolved) (pp_or_error ~err:"unresolved" pp_resolved);
    ]

(* Data references are printed as written in the source *)
let pp_data_reference : data_reference Pretty.printer =
  fun ppf r -> Cobol_ptree.pp_qualname' ppf r.resolved_name

let pp_record_reference : record_reference Pretty.printer =
  fun ppf r -> Cobol_ptree.pp_name' ppf r.resolved_name

let rec pp_ident ppf { ident_base; ident_refmod } =
  match ident_refmod with
  | None -> pp_base_ident ppf ident_base
  | Some r -> fmt "@[%a@ %a@]" ppf pp_base_ident ident_base pp_refmod r

and pp_base_ident ppf = function
  | Address i -> pp_address ppf i
  | Counter c -> Cobol_ptree.pp_counter ppf c
  | InlineCall i -> pp_inline_call ppf i
  | InlineInvoke i -> pp_inline_invocation ppf i
  | ObjectView o -> pp_object_view ppf o
  | ObjectRef o -> Cobol_ptree.pp_object_ref ppf o
  | QualIdent i -> pp_qualident ppf i

and pp_or_ident : 'l. 'l Pretty.printer -> 'l or_ident Pretty.printer =
  fun pp_literal ppf x ->
  match x with
  | Ident i -> pp_ident ppf i
  | Literal l -> pp_or_error ~err:"invalid literal value" pp_literal ppf l

and pp_ident_or_literal ppf = pp_or_ident Cobol_data.Printer.pp_value ppf

and pp_literal_value_or_error ppf =
  pp_or_error ~err:"invalid literal value" Cobol_data.Printer.pp_value ppf

and pp_scalar ppf = function
  | ScalarIdent i -> pp_ident ppf i
  | ScalarLiteral l -> pp_literal_value_or_error ppf l
  | LengthOf i -> fmt "@[LENGTH OF@ %a@]" ppf pp_ident_or_literal i

and pp_subscript ppf : subscript -> unit = function
  | SubSAll -> string ppf "ALL"
  | SubSExpr e -> pp_expr' ppf e
  | SubSIdx (n, i) ->
      fmt "%a@ %s@ %a" ppf Cobol_ptree.pp_name' n
        (if Z.sign i < 0 then "-" else "+") Z.pp_print (Z.abs i)

and pp_refmod ppf { refmod_left; refmod_length } =
  fmt "@[<1>(%a:%a)@]" ppf pp_expr' refmod_left (option pp_expr') refmod_length

and pp_qualident ppf { ident_data_ref; ident_subscripts } =
  pp_data_reference ppf ident_data_ref;
  if ident_subscripts <> [] then
    fmt "@[<1>(%a)@]" ppf
      (list ~sep:comma (pp_with_loc pp_subscript))
      ident_subscripts

and pp_address ppf = function
  | DataAddress i -> fmt "ADDRESS@ OF@ %a" ppf pp_ident i
  | ProgAddress i -> fmt "ADDRESS@ OF@ PROGRAM@ %a" ppf pp_ident_or_literal i

and pp_inline_call ppf = function
  | CallFunc { func; args } ->
      fmt "FUNCTION@ %a@ @[<1>(%a)@]" ppf Cobol_ptree.pp_name' func
        (list ~sep:sp pp_effective_arg)
        args
  | CallGenericIntrinsic { func; args } ->
      fmt "FUNCTION@ %a@ @[<1>(%a)@]" ppf
        (pp_with_loc Cobol_ptree.pp_intrinsic_name)
        func
        (list ~sep:sp pp_effective_arg)
        args
  | CallTrim { arg; tip } ->
      fmt "FUNCTION@ TRIM@ @[<1>(%a, %a)@]" ppf pp_effective_arg arg
        (option Cobol_ptree.pp_trimming_tip)
        tip
  | CallLength { arg; physical } ->
      fmt "FUNCTION@ LENGTH@ @[<1>(%a%t)@]" ppf pp_ident_or_literal arg
        (if physical then fmt ",@ PHYSICAL" else ignore)
  | CallNumvalC args ->
      fmt "FUNCTION@ NUMVAL-C@ @[<1>(%a)@]" ppf
        (list ~sep:sp pp_ident_or_literal)
        args
  | CallLocaleDate { locale_func_args = dt; locale_func_locale = l } ->
      fmt "FUNCTION@ LOCALE-DATE@ @[<1>(%a%a)@]" ppf pp_effective_arg dt
        (option ~none:nop (fun ppf -> fmt ",@ %a" ppf Cobol_ptree.pp_term))
        l
  | CallLocaleTime { locale_func_args = dt; locale_func_locale = l } ->
      fmt "FUNCTION@ LOCALE-TIME@ @[<1>(%a,@ %a)@]" ppf pp_effective_arg dt
        (option ~none:nop (fun ppf -> fmt ",@ %a" ppf Cobol_ptree.pp_term))
        l
  | CallLocaleTimeFromSeconds { locale_func_args = dt; locale_func_locale = l }
    ->
      fmt "FUNCTION@ LOCALE-TIME-FROM-SECONDS@ @[<1>(%a, %a)@]" ppf
        pp_effective_arg dt
        (option ~none:nop (fun ppf -> fmt ",@ %a" ppf Cobol_ptree.pp_term))
        l
  | CallFormattedDatetime
      { formatted_func_args = a; formatted_func_system_offset = so } ->
      fmt "FUNCTION@ FORMATTED-DATETIME@ @[<1>(%a%t)@]" ppf
        (list pp_effective_arg) a
        (if so then fmt ",@ SYSTEM-OFFSET" else ignore)
  | CallFormattedTime
      { formatted_func_args = a; formatted_func_system_offset = so } ->
      fmt "FUNCTION@ FORMATTED-TIME@ @[<1>(%a%t)@]" ppf (list pp_effective_arg) a
        (if so then fmt ",@ SYSTEM-OFFSET" else ignore)

and pp_inline_invocation ppf { invoke_class; invoke_meth; invoke_args } =
  fmt "%a::%a@ @[<1>(%a)@]" ppf pp_ident invoke_class
    pp_literal_value_or_error
    invoke_meth
    (list ~sep:sp pp_effective_arg)
    invoke_args

and pp_effective_arg ppf = function
  | ArgOmitted -> string ppf "OMITTED"
  (* Parenthesized as in {!Cobol_ptree.pp_effective_arg} *)
  | ArgExpr e -> parens pp_expr' ppf e

and pp_object_view ppf { object_view_ident; object_view_spec } =
  fmt "%a@ AS@ " ppf pp_ident object_view_ident;
  match object_view_spec with
  | ObjViewAmbiguous n -> Cobol_ptree.pp_name' ppf n
  | ObjViewOnly n -> fmt "%a@ ONLY" ppf Cobol_ptree.pp_name' n
  | ObjViewFactory n -> fmt "FACTORY@ OF@ %a" ppf Cobol_ptree.pp_name' n
  | ObjViewFactoryOnly n ->
      fmt "FACTORY@ OF@ %a@ ONLY" ppf Cobol_ptree.pp_name' n
  | ObjViewUniversal -> string ppf "UNIVERSAL"

and pp_expr ppf e = Expression.pp ppf (pretty_expression e)

and pp_expr' ppf = pp_with_loc pp_expr ppf

and pretty_expression = function
  | Atom a -> Expression.atom pp_scalar a
  | Unop (o, e) ->
      Expression.unary (Cobol_ptree.pretty_unop o) (pretty_expression ~&e)
  | Binop (a, o, b) ->
      Expression.binary (pretty_expression ~&a)
        (Cobol_ptree.pretty_binop o)
        (pretty_expression ~&b)

let rec pp_condition : condition Pretty.printer =
  fun ppf c ->
  match c with
  | Expr e -> pp_expr' ppf e
  | Relation (a, o, b) ->
      fmt "%a@ %a@ %a" ppf pp_expr' a Cobol_ptree.pp_relop o pp_expr' b
  | ClassCond (e, c) -> fmt "%a@ %a" ppf pp_expr' e Cobol_ptree.pp_class_ c
  | SignCond (e, s) -> fmt "%a@ %a" ppf pp_expr' e Cobol_ptree.pp_signz s
  | Omitted e -> fmt "%a@ OMITTED" ppf pp_expr' e
  | Not c -> fmt "NOT@ @[<1>(%a)@]" ppf pp_condition_or_error' c
  | Combined (a, o, b) ->
      fmt "@[<1>(%a)@]@ %a@ @[<1>(%a)@]" ppf pp_condition_or_error' a
        Cobol_ptree.pp_logop o pp_condition_or_error' b

and pp_condition_or_error' : condition or_error with_loc Pretty.printer =
  fun ppf -> pp_with_loc (pp_or_error ~err:"invalid condition" pp_condition) ppf

let pp_selection_range ppf
    { sel_range_negated; sel_range_start; sel_range_stop; sel_range_alphabet } =
  if sel_range_negated then fmt "NOT@ " ppf;
  fmt "%a@ THROUGH@ %a%a" ppf pp_expr' sel_range_start pp_expr' sel_range_stop
    (option (any "@ IN@ " ++ Cobol_ptree.pp_name'))
    sel_range_alphabet

let pp_selection_subject : selection_subject Pretty.printer =
  fun ppf -> function
    | SubjectValue e -> pp_expr' ppf e
    | SubjectCond c -> pp_condition ppf c
    | SubjectConst b -> string ppf (if b then "TRUE" else "FALSE")

let pp_selection_object : selection_object Pretty.printer =
  fun ppf -> function
    | SelCond c -> pp_condition ppf c
    | SelValue { negated; value } ->
        Pretty.print ppf "%s%a" (if negated then "NOT " else "") pp_expr' value
    | SelRange r -> pp_selection_range ppf r
    | SelConst b -> string ppf (if b then "TRUE" else "FALSE")
    | SelAny -> string ppf "ANY"

let pp_arg_passing_style ppf = function
  | Arg_by_value ->
      Pretty.print ppf "by@ value"
  | Arg_by_reference { optional = false } ->
      Pretty.print ppf "by@ reference"
  | Arg_by_reference { optional = true } ->
      Pretty.print ppf "by@ reference,@ optional"

let pp_procedure_arg =
  Pretty.record [
    field "arg-name-ref" (fun x -> x.arg_name_ref)
      (pp_resolved_name Cobol_data.Printer.pp_record);
    field "passing-style" (fun x -> x.arg_passing_style)
      pp_arg_passing_style;
  ]
