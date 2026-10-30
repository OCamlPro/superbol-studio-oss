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

open Cobol_ptree
open Cobol_procedure.Types
open Typeck_procedure_monad
open Typeck_diagnostics
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

type abbrev_implied_parts =
  | Nothing
  | ImpliedRelOp
  | ImpliedSubjectAndRelOp

let add_implied_relop parts =
  match parts with
  | Nothing -> ImpliedRelOp
  | _ -> parts

(** [abbrev_condition_expansion_state] is used internally by
    [expand_abbrev_cond] to remember the previous subject and relational
    operator that are implied in the abbreviation. *)
type abbrev_condition_expansion_state = {
  subject: expr with_loc option;
  relop: relop with_loc option;
  unused: abbrev_implied_parts;
  changed_in_parens: abbrev_implied_parts;
}

let no_subject_state =
  {
    subject = None;
    relop = None;
    unused = Nothing;
    changed_in_parens = Nothing;
  }

let new_state_with_subject subj =
  {
    subject = Some subj;
    relop = None;
    unused = ImpliedSubjectAndRelOp;
    changed_in_parens = ImpliedSubjectAndRelOp;
  }

let state_with_relop state rel =
  {
    state with
    relop = Some rel;
    unused = add_implied_relop state.unused;
    changed_in_parens = add_implied_relop state.changed_in_parens;
  }

type object_bool_expr_disambiguation =
  | CondBoolExpr
  | CondObject
  | CondResolveError

let disambiguate_object_vs_bool_expr (e : expr) =
  match e with
  | Atom (ScalarIdent { ident_base = QualIdent q; _ }) ->
      begin match q.ident_data_ref.resolved with
        | Ok (Cobol_data.Types.Data_condition _) -> CondBoolExpr
        | Ok _ -> CondObject
        | Error () ->
            (* Typechecking of references already flagged this resolution
               failure: We use the special CondResolveError return value to avoid
               generating more errors related to this failure. *)
            CondResolveError
      end
  | _ -> CondObject

let warn_before_state_reset state =
  match state.unused with
  | Nothing -> []
  | ImpliedRelOp -> [Condition_warning (UnusedRelOp (Option.get state.relop))]
  | ImpliedSubjectAndRelOp ->
      [
        Condition_warning (UnusedSubject (Option.get state.subject));
        Condition_warning (UnusedRelOp (Option.get state.relop));
      ]

let use_and_reset_subject ~f ~missing_subject_error state =
  begin match state.subject with
    | Some subj ->
        return (f subj, { state with relop = None; unused = Nothing })
    | None ->
        return ~diags:[Condition_error missing_subject_error]
          (Error () &@ Typeck_condition_diagnostics.error_loc
             missing_subject_error, state)
  end

let rec expand_abbrev_cond (abbrev_cond: Cobol_ptree.condition with_loc) state
  : (condition or_error with_loc * _) Typeck_procedure_monad.t =
  match ~&abbrev_cond with
  | CondSubject (subj, c) ->
      let* subj = Typeck_terms.typeck_expr' subj in
      expand_abbrev_cond c (new_state_with_subject subj)
  | CondRelOp _ when state.subject = None ->
      (* This case throws an error early to improve error location *)
      return ~diags:[Condition_error (AbbrevMissingSubject ~@abbrev_cond)]
        (Error () &@<- abbrev_cond, no_subject_state)
  | CondRelOp (rel, c) ->
      expand_abbrev_cond c (state_with_relop state rel)
  | CondObjectOrExpr expr ->
      let* expr = Typeck_terms.typeck_expr' expr in
      begin match disambiguate_object_vs_bool_expr ~&expr with
        | CondBoolExpr ->
            return ~diags:(warn_before_state_reset state)
              (Ok (Expr expr) &@<- abbrev_cond, no_subject_state)
        | CondObject ->
            begin match state.subject, state.relop with
              | Some subj, Some relop ->
                  return (Ok (Relation (subj, ~&relop, expr)) &@<- abbrev_cond,
                          { state with unused = Nothing })
              | None, _ ->
                  return
                    ~diags:[Condition_error
                              (AbbrevMissingSubject ~@abbrev_cond)]
                    (Error () &@<- abbrev_cond, state)
              | Some _, None ->
                  return
                    ~diags:[Condition_error (AbbrevMissingRelOp ~@abbrev_cond)]
                    (Error () &@<- abbrev_cond, state)
            end
        | CondResolveError ->
            return (Error () &@<- abbrev_cond, state)
      end
  | CondParen c ->
      let+ exp_cond, new_state =
        expand_abbrev_cond c { state with changed_in_parens = Nothing }
      in
      let state =
        match new_state.changed_in_parens with
        | Nothing ->
            { new_state with changed_in_parens = state.changed_in_parens }
        | ImpliedRelOp ->
            {
              new_state with
              relop = None;
              changed_in_parens = state.changed_in_parens;
            }
        | ImpliedSubjectAndRelOp -> no_subject_state
      in
      exp_cond, state
  | CondNot c ->
      let+ exp_cond, state = expand_abbrev_cond c state in
      Ok (Not exp_cond) &@<- abbrev_cond, state
  | CondCombined (a, o, b) ->
      let* exp_a, state = expand_abbrev_cond a state in
      let+ exp_b, state = expand_abbrev_cond b state in
      Ok (Combined (exp_a, o, exp_b)) &@<- abbrev_cond, state
  | CondClass cl ->
      use_and_reset_subject state
        ~f:(fun subj -> Ok (ClassCond (subj, cl)) &@<- abbrev_cond)
        ~missing_subject_error:(ClassCondMissingSubject ~@abbrev_cond)
  | CondSign sign ->
      use_and_reset_subject state
        ~f:(fun subj -> Ok (SignCond (subj, sign)) &@<- abbrev_cond)
        ~missing_subject_error:(SignCondMissingSubject ~@abbrev_cond)
  | CondOmitted ->
      use_and_reset_subject state
        ~f:(fun subj -> Ok (Omitted subj) &@<- abbrev_cond)
        ~missing_subject_error:(OmittedMissingSubject ~@abbrev_cond)

let expand_condition (cond : Cobol_ptree.condition with_loc)
  : condition or_error with_loc Typeck_procedure_monad.t =
  let+ c, _ = expand_abbrev_cond cond no_subject_state in
  c

let expand_selection_subject (subj: Cobol_ptree.selection_subject with_loc)
  : selection_subject or_error with_loc Typeck_procedure_monad.t =
  match ~&subj with
  | Subject (CondObjectOrExpr subject) ->
      let+ subject = Typeck_terms.typeck_expr' subject in
      begin match disambiguate_object_vs_bool_expr ~&subject with
        | CondObject ->
            Ok (SubjectValue subject) &@<- subj
        | CondBoolExpr ->
            Ok (SubjectCond (Expr subject)) &@<- subj
        | CondResolveError ->
            Error () &@<- subj
      end
  | Subject s ->
      let+ c = expand_condition (s &@<- subj) in
      Result.map (fun c -> SubjectCond c) ~&c &@<- c
  | SubjectConst b -> return (Ok (SubjectConst b) &@<- subj)

(* WARNING: expanding using this function will potentially duplicate a SubjectValue
   expression inside the resulting SelCond.
   By COBOL standard, subject's evaluation side effects should be performed only once in
   the PERFORM statement.
   Consider replacing SubjectValue complex expressions with a temporary variable before
   expanding if the resulting expanded_selection_object should be used for evaluation.
   However note that both GnuCOBOL 3.3 and MF duplicate subject evaluation side-effects. *)
let expand_selection_object subj (obj: Cobol_ptree.selection_object with_loc)
  : selection_object or_error with_loc Typeck_procedure_monad.t =
  match ~&subj, ~&obj with
  | _, SelAny -> return (Ok SelAny &@<- obj)
  | _, SelConst oc ->
      return (Ok (SelConst oc) &@<- obj)
  | (SubjectConst _ | SubjectCond _), SelCond c ->
      let+ x = expand_condition (c &@<- obj) in
      Result.map (fun x -> SelCond x) ~&x &@<- x
  | SubjectValue _, SelCond (CondObjectOrExpr value) ->
      let+ value = Typeck_terms.typeck_expr' value in
      begin match disambiguate_object_vs_bool_expr ~&value with
        | CondObject -> Ok (SelValue { negated = false; value }) &@<- obj
        | CondBoolExpr -> Ok (SelCond (Expr value)) &@<- obj
        | CondResolveError -> Error () &@<- obj
      end
  | SubjectValue _, SelCond (CondNot ({ payload = CondObjectOrExpr value; _ }
                                      as c)) ->
      let+ value = Typeck_terms.typeck_expr' value in
      begin match disambiguate_object_vs_bool_expr ~&value with
        | CondObject -> Ok (SelValue { negated = true; value }) &@<- obj
        | CondBoolExpr -> Ok (SelCond (Not (Ok (Expr value) &@<- c))) &@<- obj
        | CondResolveError -> Error () &@<- obj
      end
  | SubjectValue subject, SelCond c ->
      let+ xc, _ =
        expand_abbrev_cond (c &@<- obj)
          {
            subject = Some subject;
            relop = Some (Eq &@<- subj);
            unused = Nothing;
            changed_in_parens = Nothing
          }
      in
      Result.map (fun x -> SelCond x) ~&xc &@<- xc
  | SubjectValue _, SelRange { sel_range_negated; sel_range_start;
                               sel_range_stop; sel_range_alphabet } ->
      let+ sel_range_start = Typeck_terms.typeck_expr' sel_range_start
      and+ sel_range_stop = Typeck_terms.typeck_expr' sel_range_stop in
      Ok (SelRange { sel_range_negated; sel_range_start;
                     sel_range_stop; sel_range_alphabet }) &@<- obj
  | _, SelRange _ ->
      return ~diags:[Condition_error (SelRangeWithoutSubjectValue ~@obj)]
        (Error () &@<- obj)


(* --- *)

let expand_selection_subjects subjects
  : selection_subject or_error with_loc list Typeck_procedure_monad.t =
  map_list expand_selection_subject subjects

(** Expands one list of selection objects of a WHEN clause, given the expanded
    EVALUATE subjects.  Objects are ill-typed when their subject is ill-typed
    (without any further error), or when there is no corresponding subject. *)
let expand_selection_objects subjects obj_list
  : selection_object or_error with_loc list Typeck_procedure_monad.t =
  let rec expand_objects subjects obj_list =
    match subjects, obj_list with
    | { payload = Ok subj; loc } :: subjects, obj :: obj_list ->
        let+ obj = expand_selection_object (subj &@ loc) obj
        and+ obj_list = expand_objects subjects obj_list in
        obj :: obj_list
    | { payload = Error (); _ } :: subjects, obj :: obj_list ->
        let+ obj_list = expand_objects subjects obj_list in
        (Error () &@<- obj) :: obj_list
    | [], obj_list ->
        return (List.map (fun obj -> Error () &@<- obj) obj_list)
    | _, [] ->
        return []
  in
  let+ () =
    if List.compare_lengths subjects obj_list <> 0 then
      report @@ Condition_error
        (MismatchingSelectionLength { nb_sel_subjects = List.length subjects;
                                      sel_object_list = obj_list })
    else
      return ()
  and+ obj_list = expand_objects subjects obj_list in
  obj_list
