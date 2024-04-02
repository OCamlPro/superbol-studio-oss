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
  | Invalid of { loc: srcloc; stuff: invalid_stuff }
  | Malformed of { loc: srcloc; stuff: malformed_stuff }
  | Overlong_literal of { loc: srcloc;
                          literal_string: string;
                          max_length: int }       (* TODO: +kind *)

and invalid_stuff =
  | Character_in_literal of { literal_class: literal_class; char: char }

and literal_class =
  | Boolean
  | Fixed
  | Floating
  | Hexadecimal
  | Integer

and malformed_stuff =
  | Boolean_literal of string

let error_loc = function
  | Invalid { loc; _ }
  | Malformed { loc; _ }
  | Overlong_literal { loc; _ } ->
      loc

let pp_literal_class ppf = function
  | Boolean ->
      Pretty.string ppf "Boolean"
  | Fixed ->
      Pretty.print ppf "fixed-point@ numeric"
  | Floating ->
      Pretty.print ppf "floating-point@ numeric"
  | Hexadecimal ->
      Pretty.string ppf "hexadecimal"
  | Integer ->
      Pretty.print ppf "Integer"

let pp_invalid_stuff ppf = function
  | Character_in_literal { literal_class; char } ->
      Pretty.print ppf "character@ `%c'@ in@ %a@ literal"
        char pp_literal_class literal_class

let pp_malformed_stuff ppf = function
  | Boolean_literal _ ->
      Pretty.print ppf "Boolean@ literal"

let pp_error ppf = function
  | Invalid { stuff; _ } ->
      Pretty.print ppf "Invalid@ %a" pp_invalid_stuff stuff
  | Malformed { stuff; _ } ->
      Pretty.print ppf "Malformed@ %a" pp_malformed_stuff stuff
  | Overlong_literal { max_length; literal_string; _ } ->
      Pretty.print ppf "Literal@ of@ length@ %u@ exceeds@ maximum@ allowed@ \
                        length@ %u" (String.length literal_string) max_length

type warning = |

(* let pp_warning ppf _ = () *)

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

let translate ({ warnings = _; errors }: t) =
  let module DIAGS = Cobol_common.Diagnostics in
  DIAGS.Set.none |> fun diags ->
  (* List.fold_left begin fun diags w -> *)
  (*   DIAGS.Acc.warn diags ~loc:(warning_loc w) "%a" pp_warning w *)
  (* end diags warnings |> fun diags -> *)
  List.fold_left begin fun diags e ->
    DIAGS.Acc.error diags ~loc:(error_loc e) "%a" pp_error e
  end diags errors

include Cobol_common.Diagnostics_accumulator.MAKE (struct
    type t = diagnostics
    let none = none
    let union = union
    let translate = translate
  end)
