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

open Unit_types

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

let pp_resolved_name pp_resolved : _ resolved_name Fmt.t =
  Pretty.record [
    Fmt.(field "resolved-name" (fun x -> x.resolved_name) Cobol_ptree.pp_name');
    Fmt.(field "resolved" (fun x -> x.resolved) pp_resolved);
  ]

let pp_resolved_qualname pp_resolved : _ resolved_qualname Fmt.t =
  Pretty.record [
    Fmt.(field "resolved-name" (fun x -> x.resolved_name) Cobol_ptree.pp_qualname');
    Fmt.(field "resolved" (fun x -> x.resolved) pp_resolved);
  ]

let pp_arg_passing_style ppf = function
  | Arg_by_value ->
      Pretty.print ppf "by@ value"
  | Arg_by_reference { optional = false } ->
      Pretty.print ppf "by@ reference"
  | Arg_by_reference { optional = true } ->
      Pretty.print ppf "by@ reference,@ optional"

let pp_procedure_arg =
  Pretty.record [
    Fmt.(field "arg-definition" (fun x -> x.arg_data_definition)
           (pp_resolved_name Cobol_data.Printer.pp_data_definition));
    Fmt.(field "passing-style" (fun x -> x.arg_passing_style)
           pp_arg_passing_style);
  ]
  
let rec pp_expanded_cond: expanded_cond Pretty.printer = 
  fun ppf xc -> 
    let open Cobol_ptree in
    let open Fmt in
    match xc with
    | Expr e ->
        pp_expr' ppf e
    | Relation (a, o, b) ->
        fmt "%a@ %a@ %a" ppf pp_expr' a pp_relop o pp_expr' b
    | ClassCond (e, c) ->
        fmt "%a@ %a" ppf pp_expr' e pp_class_ c
    | SignCond (e, s) ->
        fmt "%a@ %a" ppf pp_expr' e pp_signz s
    | Omitted e ->
        fmt "%a@ OMITTED" ppf pp_expr' e
    | Not c ->
        fmt "NOT@ @[<1>(%a)@]" ppf pp_expanded_cond' c
    | Combined (a, o, b) ->
        fmt "@[<1>(%a)@]@ %a@ @[<1>(%a)@]" ppf
          pp_expanded_cond' a pp_logop o pp_expanded_cond' b

and pp_expanded_cond': expanded_cond with_loc Pretty.printer = fun ppf ->
  pp_with_loc pp_expanded_cond ppf

let pp_expanded_selection_subject: expanded_selection_subject Pretty.printer =
  fun ppf -> function
    | SubjectValue e -> Cobol_ptree.pp_expr' ppf e
    | SubjectCond c -> pp_expanded_cond ppf c
    | SubjectConst b -> Fmt.string ppf (if b then "TRUE" else "FALSE")

let pp_expanded_selection_subject'
  : expanded_selection_subject with_loc Pretty.printer = fun ppf ->
  pp_with_loc pp_expanded_selection_subject ppf

let pp_expanded_selection_object: expanded_selection_object Pretty.printer =
  fun ppf -> function
    | SelCond c -> pp_expanded_cond ppf c
    | SelValue { negated; value } ->
        Pretty.print ppf "%s%a"
          (if negated then "NOT " else "") Cobol_ptree.pp_expr' value
    | SelRange r -> Cobol_ptree.pp_selection_range ppf r
    | SelConst b -> Fmt.string ppf (if b then "TRUE" else "FALSE")
    | SelAny -> Fmt.string ppf "ANY"

let pp_expanded_selection_object'
  : expanded_selection_object with_loc Pretty.printer = fun ppf ->
  pp_with_loc pp_expanded_selection_object ppf

let pp_cobol_unit ?(show_items = false) =
  Pretty.record_with_conditional_fields [
    T Fmt.(field "name" (fun x -> ~&(x.unit_name)) string);
    T (Pretty.vfield "records" (fun x -> x.unit_data.data_records)
         (Fmt.(list ~sep:nop) Cobol_data.Printer.pp_record));
    C'(show_items,
       Pretty.vfield "items" (fun x -> x.unit_data.data_items.named)
         (Unit_resolver_map.pp Cobol_data.Printer.pp_data_definition));
  ]

let pp_cobol_unit' ppf u = pp_cobol_unit ppf ~&u

let pp_group ppf group =
  Unit_collections.SET.iter begin fun u ->
    Fmt.(vbox @@ (styled `Yellow @@ any "unit") ++ any ": " ++
                 pp_cobol_unit' ++ any "@\n") ppf u
  end group
