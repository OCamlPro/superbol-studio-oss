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

let register_printers () =

  Cobol_ir.Printer.register_unsupported_stuff_printer begin fun ppf -> function
    | Literal l ->
        Pretty.print ppf "literal@ %a" Cobol_ptree.pp_literal l
    | Field_usage ->
        Pretty.print ppf "field@ usage"
    | _ ->
        raise Exit
  end;

  Cobol_ir.Printer.register_error_printer begin fun ppf -> function
    | Invalid_compilation_group { reason = `empty_group } ->
        Pretty.print ppf "Empty@ compilation@ group@ given"
    | Invalid_compilation_group { reason = `non_singleton_group } ->
        Pretty.print ppf "Multiple@ units@ found@ in@ compilation@ group"
    | Ezlibcob_error IntegerOverflow ->
        Pretty.print ppf "integer@ overflow"
    | _ ->
        raise Exit
  end
