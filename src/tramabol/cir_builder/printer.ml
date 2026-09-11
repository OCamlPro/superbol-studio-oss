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

open Cobol_common.Srcloc.INFIX
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

let register_unexpected_stuff_printer, pp_unexpected_stuff =
  printers_for_extended_type "Cir_builder.Types.unexpected_stuff"

let register_unsupported_stuff_printer, pp_unsupported_stuff =
  printers_for_extended_type "Cir_builder.Types.unsupported_stuff"

let register_error_printer, pp_error =
  printers_for_extended_type "Cir_builder.Types.error"

let pp_ambiguous_stuff: ambiguous_stuff Pretty.printer = fun ppf -> function
  | Data_reference qn ->
      Pretty.print ppf "data-name@ '%a'" Cobol_ptree.pp_qualname qn

let pp_extraneous_stuff: extraneous_stuff Pretty.printer = fun ppf -> function
  | Data_reference_subscripts { qn; amount } ->
      Pretty.print ppf "subscript%s@ for@ data-name@ '%a'"
        (if amount = 1 then "" else "s") Cobol_ptree.pp_qualname qn

let pp_missing_stuff: missing_stuff Pretty.printer = fun ppf -> function
  | Data_reference_subscripts { qn; amount } ->
      Pretty.print ppf "%u@ subscript%s@ for@ data-name@ '%a'"
        amount (if amount = 1 then "" else "s") Cobol_ptree.pp_qualname qn

let pp_undefined_stuff: undefined_stuff Pretty.printer = fun ppf -> function
  | Data_reference qn ->
      Pretty.print ppf "data-name@ '%a'" Cobol_ptree.pp_qualname qn

let pp_errors ?platform ppf errors =
  NEL.iter ~f:begin fun e ->
    Option.iter begin fun loc ->
      Cobol_common.Srcloc.pp_srcloc_with_optional_caret ?platform ppf loc;
    end (Error.loc e);
    Pretty.print ppf "Error: @[%a@]@\n" pp_error e
  end errors

(* --- *)

let register_printers () =

  register_unexpected_stuff_printer begin fun ppf -> function
    | Reference_modification ->
        Pretty.print ppf "reference@ modification"
    | _ ->
        raise Exit
  end;

  register_unsupported_stuff_printer begin fun ppf -> function
    | Condition _ ->
        Pretty.print ppf "condition"
    | Dynamic_table ->
        Pretty.print ppf "dynamic-capacity@ table"
    | Expression _ ->
        Pretty.print ppf "expression"
    | Statement _ ->
        Pretty.print ppf "statement"
    | Subscript s ->
        Pretty.print ppf "subscript@ %a" Cobol_ptree.pp_subscript ~&s
    | Term t ->
        Pretty.print ppf "term@ %a" Cobol_ptree.pp_term t
    | Variable_length_field ->
        Pretty.print ppf "variable-length field"
    | _ ->
        raise Exit
  end;

  register_error_printer begin fun ppf -> function
    | Ambiguous { stuff; _ } ->
        Pretty.print ppf "ambiguous@ %a" pp_ambiguous_stuff stuff
    | Extraneous { stuff; _ } ->
        Pretty.print ppf "extraneous@ %a" pp_extraneous_stuff stuff
    | Missing { stuff; _ } ->
        Pretty.print ppf "missing@ %a" pp_missing_stuff stuff
    | Unexpected { stuff; _ } ->
        Pretty.print ppf "unexpected@ %a" pp_unexpected_stuff stuff
    | Unsupported { stuff; _ } ->
        Pretty.print ppf "unsupported@ %a" pp_unsupported_stuff stuff
    | Undefined { stuff; _ } ->
        Pretty.print ppf "undefined@ %a" pp_undefined_stuff stuff
    | Data_error e ->
        Cobol_data.Printer.pp_error ppf e
    | _ ->
        raise Exit
  end
