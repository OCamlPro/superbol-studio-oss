(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Types

let printers_for_extended_type type_name =
  let l = ref [] in
  (fun pp -> l := pp :: !l),
  (fun ppf e ->
     let rec aux = function
       | [] -> Pretty.print ppf "<%s>" type_name
       | pp :: tl -> try pp ppf e with Exit -> aux tl
     in
     aux !l)

let register_unsupported_stuff_printer, pp_unsupported_stuff =
  printers_for_extended_type "Cobol_ir.Types.unsupported_stuff"

let register_error_printer, pp_error =
  printers_for_extended_type "Cobol_ir.Types.error"

let pp_undefined_stuff: undefined_stuff Pretty.printer = fun ppf -> function
  | Data_reference qn ->
      Pretty.print ppf "data-name@ %a" Cobol_ptree.pp_qualname qn

let pp_ambiguous_stuff: ambiguous_stuff Pretty.printer = fun ppf -> function
  | Data_reference qn ->
      Pretty.print ppf "data-name@ %a" Cobol_ptree.pp_qualname qn

let register_printers () =
  register_unsupported_stuff_printer begin fun ppf -> function
    | Statement _ ->
        Pretty.print ppf "statement"
    | Term t ->
        Pretty.print ppf "term@ %a" Cobol_ptree.pp_term t
    | Field_in_occurs ->
        Pretty.print ppf "field@ in@ OCCURS"
    | Variable_length_field ->
        Pretty.print ppf "variable-length field"
    | _ ->
        raise Exit
  end;

  register_error_printer begin fun ppf -> function
    | Unsupported { stuff; _ } ->
        Pretty.print ppf "Unsupported@ %a" pp_unsupported_stuff stuff
    | Undefined { stuff; _ } ->
        Pretty.print ppf "Undefined@ %a" pp_undefined_stuff stuff
    | Ambiguous { stuff; _ } ->
        Pretty.print ppf "Ambiguous@ %a" pp_ambiguous_stuff stuff
    | _ ->
        raise Exit
  end
