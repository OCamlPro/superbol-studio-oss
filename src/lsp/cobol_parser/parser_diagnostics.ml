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
  | Caught_exception of { msg: string }
  | Malformed of { loc: srcloc; stuff: malformed_stuff }
  | Missing of { loc: srcloc; stuff: missing_stuff }
  | Unexpected of { loc: srcloc; stuff: unexpected_stuff }
  | Unterminated of { loc: srcloc; stuff: unterminated_stuff }

and malformed_stuff =
  | Alphanumeric_literal

and missing_stuff =
  | Continuation_of of string

and unexpected_stuff =
  | Pseudotext
  | Character_in_symbolic_EBCDIC of char
  | Symbolic_EBCDIC_orginal of int

and unterminated_stuff =
  | Comment_entry

let pp_malformed_stuff ppf = function
  | Alphanumeric_literal ->
      Pretty.print ppf "alphanumeric@ literal"

let pp_missing_stuff ppf = function
  | Continuation_of str ->
      Pretty.print ppf "continuation@ of@ `%s'" str

let pp_unexpected_stuff ppf = function
  | Pseudotext ->
      Pretty.string ppf "pseudotext"
  | Character_in_symbolic_EBCDIC c ->
      Pretty.print ppf "character:@ `%c'" c
  | Symbolic_EBCDIC_orginal i ->
      Pretty.print ppf "symbolic@ character@ ordinal@ %d@ (expected@ range@ is@ \
                        {1, ..., 256})" i

let pp_unterminated_stuff ppf = function
  | Comment_entry ->
      Pretty.string ppf "comment@ entry"

let error_loc = function
  | Caught_exception _ ->
      None
  | Malformed { loc; _ }
  | Missing { loc; _ }
  | Unexpected { loc; _ }
  | Unterminated { loc; _ } ->
      Some loc

let pp_error ppf = function
  | Caught_exception { msg } ->
      Pretty.print ppf "%a" Pretty.text msg
  | Malformed { stuff; _ } ->
      Pretty.print ppf "Malformed@ %a" pp_malformed_stuff stuff
  | Missing { stuff; _ } ->
      Pretty.print ppf "Missing@ %a" pp_missing_stuff stuff
  | Unexpected { stuff; _ } ->
      Pretty.print ppf "Unexpected@ %a" pp_unexpected_stuff stuff
  | Unterminated { stuff; _ } ->
      Pretty.print ppf "Unterminated@ %a" pp_unterminated_stuff stuff

type customizable_diagnostic =
  | Implementation_pending of string
  | Missing_tokens of Pretty.delayed     (* TODO: avoid this functional value *)
  | Invalid_syntax

let pp_customizable_diagnostic ppf = function
  | Implementation_pending descr ->
      Pretty.print ppf "Ignored@ %a@ (implementation@ pending)"
        Pretty.text descr
  | Missing_tokens pp_assumed ->
      Pretty.print ppf "Missing@ %t" pp_assumed
  | Invalid_syntax ->
      Pretty.print ppf "Invalid@ syntax"

type custom =
  {
    severity: Cobol_common.Diagnostics.severity;
    loc: srcloc option;
    diag: customizable_diagnostic;
  }
type diagnostics =
  {
    errors: error list;
    customs: custom list;
  }
type t = diagnostics
let none =
  {
    errors = [];
    customs = [];
  }
let union d1 d2 =
  {
    errors = d1.errors @ d2.errors;
    customs = d1.customs @ d2.customs;
  }
let add_error e diags =
  { diags with errors = e :: diags.errors }
let error e = add_error e none

let has_errors diags =
  diags.errors <> [] ||
  List.exists (fun { severity; _ } -> severity = Error) diags.customs

let add_diag ~severity ?loc diag diags =
  { diags with customs = { severity; loc; diag } :: diags.customs }

let add_exn exn diags =
  match exn with
  | Failure msg
  | Sys_error msg ->
      add_error (Caught_exception { msg }) diags
  | e ->
      raise e                                                          (* ??? *)

let translate ({ errors; customs }: t) =
  let module DIAGS = Cobol_common.Diagnostics in
  DIAGS.Set.none |> fun diags ->
  List.fold_left begin fun diags e ->
    DIAGS.Acc.error diags ?loc:(error_loc e) "%a" pp_error e
  end diags errors |> fun diags ->
  List.fold_left begin fun diags { severity; loc; diag } ->
    DIAGS.Acc.diag severity diags ?loc "%a" pp_customizable_diagnostic diag
  end diags customs

module Accumulator = struct
  include Cobol_common.Diagnostics_accumulator.MAKE
      (struct
        type t = diagnostics
        let none = none
        let union = union
        let translate = translate
      end)
end

module ALL = struct                 (* combines preproc & parsing diagnostics *)
  type t =
    {
      preproc_diags: Cobol_preproc.Diagnostics.t;
      parser_diags: diagnostics;
    }
  let none =
    {
      preproc_diags = Cobol_preproc.Diagnostics.none;
      parser_diags = none;
    }
  let union d1 d2 =
    {
      preproc_diags =
        Cobol_preproc.Diagnostics.union d1.preproc_diags d2.preproc_diags;
      parser_diags =
        union d1.parser_diags d2.parser_diags;
    }
  let translate { preproc_diags; parser_diags } =
    let pp_diags = Cobol_preproc.Diagnostics.translate preproc_diags in
    let pa_diags = translate parser_diags in
    Cobol_common.Diagnostics.Set.union pp_diags pa_diags
  let has_errors { preproc_diags; parser_diags } =
    Cobol_preproc.Diagnostics.has_errors preproc_diags ||
    has_errors parser_diags
end
