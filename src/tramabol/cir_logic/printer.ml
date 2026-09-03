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

let (register_runtime_error_printer: runtime_error Pretty.printer -> unit),
    pp_runtime_error =
  printers_for_extended_type "Cir_logic.Types.runtime_error"

let (register_runtime_operation_printer: runtime_operation Pretty.printer -> unit),
    pp_runtime_operation =
  printers_for_extended_type "Cir_logic.Types.runtime_operation"

let pp_runtime_errors ?platform ppf errors =
  NEL.iter ~f:begin fun e ->
    Option.iter begin fun loc ->
      Cobol_common.Srcloc.pp_srcloc_with_optional_caret ?platform ppf loc;
    end (Error.loc e);
    Pretty.print ppf "Error: @[%a@]@\n"
      pp_runtime_error e
  end errors

let pp_localized_runtime_errors ?platform ppf errors =
  NEL.iter ~f:begin fun { loc; error } ->
    Option.iter begin fun loc ->
      Cobol_common.Srcloc.pp_srcloc_with_optional_caret ?platform ppf loc;
    end loc;
    Pretty.print ppf "Error: @[%a@]@\n"
      pp_runtime_error error
  end errors

(* --- *)

let register_printers () =

  register_runtime_error_printer begin fun ppf -> function
    | Unsupported_runtime_operation o ->
        Pretty.print ppf "Unsupported@ runtime@ operation:@;%a"
          pp_runtime_operation o
    | _ ->
        raise Exit
  end
