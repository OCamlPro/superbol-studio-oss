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
open Cobol_common.Srcloc.INFIX

type error =
  | Copybook_lookup_error of { copyloc: srcloc option;
                               lnf: Cobol_common.Copybook.lookup_info }
  | Cyclic_copy of { copyloc: srcloc; filename: string }
  | Feature_error of Cobol_config.DIAG.error
  | Forbidden of { loc: srcloc; stuff: forbidden_stuff }
  | Invalid of { loc: srcloc; stuff: invalid_stuff }
  | Literal_error of Cobol_data.Diagnostics.error
  | Malformed of { loc: srcloc; stuff: malformed_stuff }
  | Missing of { loc: srcloc; stuff: missing_stuff }
  | Src_error of Src_diagnostics.error
  | Unexpected of { loc: srcloc; stuff: unexpected_stuff }
  | Unterminated of { loc: srcloc; stuff: unterminated_stuff }

and forbidden_stuff =
  | Change_of_source_format
  | Multiword_operand
  | Operand_with_spaces

and malformed_stuff =
  | Compiler_directive
  | Preproc_statement of [`COPY | `REPLACE | `EXEC_BLOCK]

and missing_stuff =
  | At_least_one_text_word
  | At_most_one_text_word
  | One_text_word

and unexpected_stuff =
  | Alphanumeric_literal
  | Elif_compiler_directive of { suggestion: suggested_missing option }
  | Else_compiler_directive of { suggestion: suggested_missing option }
  | EndIf_compiler_directive
  | Pseudotext

and invalid_stuff =
  | Compiler_directive_word of string
  | Source_format of string

and suggested_missing =
  | EndIf_compiler_directive_missing of { initial_if_loc: srcloc }

and unterminated_stuff =
  | If_compiler_directive of { suggested_endif_loc: srcloc }
  | Exec_block

let error_loc = function
  | Feature_error e ->
      Some (Cobol_config.DIAG.error_loc e)
  | Src_error e ->
      Some (Src_diagnostics.error_loc e)
  | Literal_error e ->
      Some (Cobol_data.Diagnostics.error_loc e)
  | Cyclic_copy { copyloc = loc; _ }
  | Forbidden { loc; _ }
  | Invalid { loc; _ }
  | Malformed { loc; _ }
  | Missing { loc; _ }
  | Unexpected { loc; _ }
  | Unterminated { loc; _ } ->
      Some loc
  | Copybook_lookup_error { copyloc = loc; _ } ->
      loc

let pp_preproc_statement ppf = function
  | `COPY -> Pretty.string ppf "COPY"
  | `REPLACE -> Pretty.string ppf "REPLACE"
  | `EXEC_BLOCK -> Pretty.string ppf "EXEC/END-EXEC block"

let pp_forbidden_stuff ppf = function
  | Change_of_source_format ->
      Pretty.print ppf "change@ of@ source@ format"
  | Multiword_operand ->
      Pretty.print ppf "multi-word@ operand"
  | Operand_with_spaces ->
      Pretty.print ppf "operand@ with@ spaces"

let pp_invalid_stuff ppf = function
  | Compiler_directive_word f ->
      Pretty.print ppf "%s@ compiler@ directive" f
  | Source_format f ->
      Pretty.print ppf "source@ format@ `%s'" f

let pp_suggestion ppf = function
  | EndIf_compiler_directive_missing _ ->
      Pretty.print ppf ">>END-IF@ expected"

let pp_missing_stuff ppf = function
  | At_least_one_text_word ->
      Pretty.print ppf "at@ least@ one@ text-word"
  | At_most_one_text_word ->
      Pretty.print ppf "at@ most@ one@ text-word"
  | One_text_word ->
      Pretty.print ppf "one@ text-word"
  (* | End_of_compiler_directive { suggestion } -> *)
  (*     Pretty.print ppf "end@ of@ compiler@ directive;@ %a" *)
  (*       pp_suggestion suggestion *)

let pp_unexpected_stuff ppf = function
  | Alphanumeric_literal ->
      Pretty.print ppf "alphanumeric@ literal"
  | Elif_compiler_directive { suggestion } ->
      Pretty.print ppf ">>ELIF@ compiler@ directive";
      Fmt.(option (any ";@ " ++ pp_suggestion)) ppf suggestion
  | Else_compiler_directive { suggestion } ->
      Pretty.print ppf ">>ELSE@ compiler@ directive";
      Fmt.(option (any ";@ " ++ pp_suggestion)) ppf suggestion
  | EndIf_compiler_directive ->
      Pretty.print ppf ">>END-IF@ compiler@ directive"
  | Pseudotext ->
      Pretty.print ppf "pseudotext"

let pp_unterminated ppf = function
  | If_compiler_directive _ ->
      Pretty.print ppf ">>IF@ compiler@ directive"
  | Exec_block ->
      Pretty.print ppf "EXEC/END-EXEC@ block"

let pp_error ppf = function
  | Copybook_lookup_error { lnf; _ } ->
      Cobol_common.Copybook.pp_lookup_error ppf lnf
  | Cyclic_copy { filename; _ } ->
      Pretty.print ppf "Cyclic@ COPY@ of@ `%s'" filename
  | Feature_error e ->
      Cobol_config.DIAG.pp_error ppf e
  | Forbidden { stuff; _ } ->
      Pretty.print ppf "Forbidden@ %a" pp_forbidden_stuff stuff
  | Invalid { stuff; _ } ->
      Pretty.print ppf "Invalid@ %a" pp_invalid_stuff stuff
  | Literal_error e ->
      Cobol_data.Diagnostics.pp_error ppf e
  | Malformed { stuff = Compiler_directive; _ } ->
      Pretty.print ppf "Malformed@ compiler@ directive"
  | Malformed { stuff = Preproc_statement stmt; _ } ->
      Pretty.print ppf "Malformed@ %a@ statement" pp_preproc_statement stmt
  (* | Missing { stuff = End_of_compiler_directive _ as stuff; _ } -> *)
  (*     Pretty.print ppf "Missing@ %a" pp_missing_stuff stuff *)
  | Missing { stuff; _ } ->
      Pretty.print ppf "Expected@ %a" pp_missing_stuff stuff
  | Src_error e ->
      Src_diagnostics.pp_error ppf e
  | Unexpected { stuff; _ } ->
      Pretty.print ppf "Unexpected@ %a" pp_unexpected_stuff stuff
  | Unterminated { stuff; _ } ->
      Pretty.print ppf "Unterminated@ %a" pp_unterminated stuff

(* --- *)

type warning =
  | Feature_warning of Cobol_config.DIAG.warning
  | Ignored of { loc: srcloc; item: ignored_item }
  | Src_warning of Src_diagnostics.warning
  | Undefine_of_unknown_env_variable of
      {
        loc: srcloc;
        var: string with_loc;
      }
  | Redefinition_of_env_variable of
      {
        loc: srcloc;
        var: string with_loc;
        prev_def_loc: Preproc_env.definition_loc;
      }
  (* | Compdir_warning of *)
  (*     { *)
  (*       loc: srcloc; *)
  (*       compdir: Preproc_directives.compiler_directive with_loc; *)
  (*       cause: compdir_warning_cause; *)
  (*     } *)

and ignored_item =
  | Compiler_directive

let warning_loc = function
  | Feature_warning w ->
      Cobol_config.DIAG.warning_loc w
  | Ignored { loc; _ }
  | Undefine_of_unknown_env_variable { loc; _ }
  | Redefinition_of_env_variable { loc; _ } ->
      loc
  | Src_warning e ->
      Src_diagnostics.warning_loc e

let pp_ignored_item ppf = function
  | Compiler_directive ->
      Pretty.print ppf "compiler@ directive"

let pp_warning ppf = function
  | Feature_warning w ->
      Cobol_config.DIAG.pp_warning ppf w
  | Ignored { item; _ } ->
      Pretty.print ppf "Ignored@ %a" pp_ignored_item item
  | Src_warning e ->
      Src_diagnostics.pp_warning ppf e
  | Undefine_of_unknown_env_variable { var; _ } ->
      Pretty.print ppf "DEFINE@ OFF@ of@ %s,@ which@ is@ not@ defined@ (yet)"
        ~&var
  | Redefinition_of_env_variable { var; prev_def_loc; _ } ->
      Pretty.print ppf "Redefinition@ of@ %s;@ previous@ definition@ was@ from@ \
                        %t" ~&var
        (fun ppf -> match prev_def_loc with
           | Source_location l -> Cobol_common.Srcloc.pp_file_loc ppf l
           | Process_parameter -> Pretty.print ppf "process@ parameters"
           | Process_environment -> Pretty.print ppf "process@ environment")

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

let add_literal_diagnostics Cobol_data.Diagnostics.{ errors; _ } diags =
  List.fold_left begin fun diags error ->
    add_error (Literal_error error) diags
  end diags errors

let translate ({ warnings; errors }: t) =
  let module DIAGS = Cobol_common.Diagnostics in
  List.fold_left begin fun diags w ->
    DIAGS.Acc.warn diags ~loc:(warning_loc w) "%a" pp_warning w
  end DIAGS.Set.none warnings |> fun diags ->
  List.fold_left begin fun diags e ->
    DIAGS.Acc.error diags ?loc:(error_loc e) "%a" pp_error e
  end diags errors
