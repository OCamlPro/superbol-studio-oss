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

open Cobol_common.Srcloc.TYPES

type error =
  | Copybook_lookup_error of { copyloc: srcloc option;
                               lnf: Cobol_common.Copybook.lookup_info }
  | Cyclic_copy of { copyloc: srcloc; filename: string }
  | Feature_error of Cobol_config.DIAG.error
  | Forbidden of { loc: srcloc; stuff: forbidden_stuff }
  | Malformed_or_unknown_compiler_directive of srcloc
  | Malformed_statement of { loc: srcloc; stmt: [ `COPY | `REPLACE ] }
  | Missing of { loc: srcloc; stuff: missing_stuff }
  | Src_error of Src_diagnostics.error
  | Unexpected of { loc: srcloc; stuff: unexpected_stuff }
  | Unknown_source_format of string * srcloc

and forbidden_stuff =
  | Change_of_source_format
  | Multiword_operand
  | Operand_with_spaces

and missing_stuff =
  | At_least_one_text_word
  | At_most_one_text_word
  | One_text_word

and unexpected_stuff =
  | Alphanumeric_literal

let error_loc = function
  | Feature_error e ->
      Some (Cobol_config.DIAG.error_loc e)
  | Src_error e ->
      Some (Src_diagnostics.error_loc e)
  | Cyclic_copy { copyloc = loc; _ }
  | Forbidden { loc; _ }
  | Malformed_or_unknown_compiler_directive loc
  | Malformed_statement { loc; _ }
  | Missing { loc; _ }
  | Unexpected { loc; _ }
  | Unknown_source_format (_, loc) ->
      Some loc
  | Copybook_lookup_error { copyloc = loc; _ } ->
      loc

let pp_statement ppf = function
  | `COPY -> Pretty.string ppf "COPY"
  | `REPLACE -> Pretty.string ppf "REPLACE"

let pp_forbidden_stuff ppf = function
  | Change_of_source_format ->
      Pretty.print ppf "change@ of@ source@ format"
  | Multiword_operand ->
      Pretty.print ppf "multi-word@ operand"
  | Operand_with_spaces ->
      Pretty.print ppf "operand@ with@ spaces"

let pp_missing_stuff ppf = function
  | At_least_one_text_word ->
      Pretty.print ppf "at@ least@ one@ text-word"
  | At_most_one_text_word ->
      Pretty.print ppf "at@ most@ one@ text-word"
  | One_text_word ->
      Pretty.print ppf "one@ text-word"

let pp_unexpected_stuff ppf = function
  | Alphanumeric_literal ->
      Pretty.print ppf "alphanumeric@ literal"

let pp_error ppf = function
  | Copybook_lookup_error { lnf; _ } ->
      Cobol_common.Copybook.pp_lookup_error ppf lnf
  | Cyclic_copy { filename; _ } ->
      Pretty.print ppf "Cyclic@ COPY@ of@ `%s'" filename
  | Feature_error e ->
      Cobol_config.DIAG.pp_error ppf e
  | Forbidden { stuff; _ } ->
      Pretty.print ppf "Forbidden@ %a" pp_forbidden_stuff stuff
  | Malformed_or_unknown_compiler_directive _ ->
      Pretty.print ppf "Malformed@ or@ unknown@ compiler@ directive"
  | Malformed_statement { stmt; _ } ->
      Pretty.print ppf "Malformed@ %a@ statement" pp_statement stmt
  | Missing { stuff; _ } ->
      Pretty.print ppf "Expected@ %a" pp_missing_stuff stuff
  | Src_error e ->
      Src_diagnostics.pp_error ppf e
  | Unexpected { stuff; _ } ->
      Pretty.print ppf "Unexpected@ %a" pp_unexpected_stuff stuff
  | Unknown_source_format (f, _) ->
      Pretty.print ppf "Unknown@ source@ format@ `%s'" f

(* --- *)

type ignored_item =
  | Compiler_directive

let pp_ignored_item ppf = function
  | Compiler_directive ->
      Pretty.print ppf "compiler@ directive"

type warning =
  | Feature_warning of Cobol_config.DIAG.warning
  | Ignored of { loc: srcloc; item: ignored_item }
  | Src_warning of Src_diagnostics.warning

let warning_loc = function
  | Feature_warning w ->
      Cobol_config.DIAG.warning_loc w
  | Ignored { loc; _ } ->
      loc
  | Src_warning e ->
      Src_diagnostics.warning_loc e

let pp_warning ppf = function
  | Feature_warning w ->
      Cobol_config.DIAG.pp_warning ppf w
  | Ignored { item; _ } ->
      Pretty.print ppf "Ignored@ %a" pp_ignored_item item
  | Src_warning e ->
      Src_diagnostics.pp_warning ppf e

type diagnostics =
  {
    errors: error list;
    warnings: warning list;
  }
type t = diagnostics
let none = { errors = []; warnings = [] }
let union d1 d2 =
  { errors = d1.errors @ d2.errors;
    warnings = d1.warnings @ d2.warnings }
let add_error e diags = { diags with errors = e :: diags.errors }
let add_warning w diags = { diags with warnings = w :: diags.warnings }
let has_errors diags = diags.errors <> []

let add_src_diagnostics Src_diagnostics.{ errors; warnings } diags =
  diags |>
  List.fold_right (fun e -> add_error (Src_error e)) errors |>
  List.fold_right (fun w -> add_warning (Src_warning w)) warnings

let translate ({ warnings; errors }: t) =
  let module DIAGS = Cobol_common.Diagnostics in
  List.fold_left begin fun diags w ->
    DIAGS.Acc.warn diags ~loc:(warning_loc w) "%a" pp_warning w
  end DIAGS.Set.none warnings |> fun diags ->
  List.fold_left begin fun diags e ->
    DIAGS.Acc.error diags ?loc:(error_loc e) "%a" pp_error e
  end diags errors
