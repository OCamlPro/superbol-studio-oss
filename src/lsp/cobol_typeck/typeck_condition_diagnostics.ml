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

open Cobol_common.Srcloc.INFIX
open Cobol_ptree

type error =
  | AbbrevMissingSubject of srcloc
  | AbbrevMissingRelOp of srcloc
  | ClassCondMissingSubject of srcloc
  | SignCondMissingSubject of srcloc
  | OmittedMissingSubject of srcloc
  | SelRangeWithoutSubjectValue of srcloc
  | MismatchingSelectionLength of { nb_sel_subjects: int ; sel_object_list: selection_object with_loc list; }

type warning =
  | UnusedSubject of expr with_loc
  | UnusedRelOp of relop with_loc

let error_loc = function
  | AbbrevMissingSubject loc 
  | AbbrevMissingRelOp loc
  | ClassCondMissingSubject loc 
  | SignCondMissingSubject loc 
  | OmittedMissingSubject loc 
  | SelRangeWithoutSubjectValue loc ->
      Some loc
  | MismatchingSelectionLength { sel_object_list; _ } ->
      Cobol_common.Srcloc.concat_locs sel_object_list

let pp_error ppf = function
  | AbbrevMissingSubject _ ->
      Pretty.print ppf "Missing subject for abbreviated condition."
  | AbbrevMissingRelOp _ ->
      Pretty.print ppf "Missing relation operator for abbreviated condition."
  | ClassCondMissingSubject _ ->
      Pretty.print ppf "Missing subject for class condition."
  | SignCondMissingSubject _ ->
      Pretty.print ppf "Missing subject for sign condition."
  | OmittedMissingSubject _ ->
      Pretty.print ppf "Missing subject for OMITTED condition."
  | SelRangeWithoutSubjectValue _ ->
      Pretty.print ppf "Invalid range in WHEN clause without an expression EVALUATE subject."
  | MismatchingSelectionLength { nb_sel_subjects; _ } ->
      Pretty.print ppf "Invalid number of selection objects (expecting %d)." nb_sel_subjects

let warning_loc = function
  | UnusedSubject e ->
      Some ~@e
  | UnusedRelOp r ->
      Some ~@r
  
let pp_warning ppf = function
  | UnusedSubject _ ->
      Pretty.print ppf "This abbreviated condition subject is never used."
  | UnusedRelOp _ ->
      Pretty.print ppf "This abbreviated condition relation operator is never used."
