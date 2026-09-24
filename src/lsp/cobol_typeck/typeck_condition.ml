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
open Cobol_common
open Cobol_unit.Types
open Typeck_results
open Typeck_diagnostics
open Srcloc.INFIX

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
  | CondUnknown

let disambiguate_object_vs_bool_expr data_items expr =
  match expr with
  | Atom a ->
    let rec is_bool_scalar : type a. a term -> object_bool_expr_disambiguation =
      function
      | QualIdent { ident_name; _ } ->
        begin match Cobol_unit.Resolver_map.find ~&ident_name data_items with
        | Cobol_data.Types.Data_condition _ -> CondBoolExpr
        | _ -> CondObject
        | exception Not_found ->
          (* Typechecking of references will already flag this resolution failure:
                 We use the special CondUnknown return value to avoid generating more
                 errors related to this failure. *)
          CondUnknown
        end
      | RefMod (a, _) -> is_bool_scalar a
      | ScalarRefMod (a, _) -> is_bool_scalar a
      | _ -> CondObject
    in
    is_bool_scalar a
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
    some_result (f subj), { state with relop = None; unused = Nothing }
  | None -> no_result ~diags:[Condition_error missing_subject_error], state
  end

let rec expand_abbrev_cond env abbrev_cond state :
    expanded_cond with_loc option with_diags * abbrev_condition_expansion_state
    =
  match ~&abbrev_cond with
  | CondSubject (subj, c) ->
    expand_abbrev_cond env c (new_state_with_subject subj)
  | CondRelOp _ when state.subject = None ->
    (* This case throws an error early to improve error location *)
    ( no_result ~diags:[Condition_error (AbbrevMissingSubject ~@abbrev_cond)],
      no_subject_state )
  | CondRelOp (rel, c) -> expand_abbrev_cond env c (state_with_relop state rel)
  | CondObjectOrExpr expr ->
    begin match disambiguate_object_vs_bool_expr env ~&expr with
    | CondBoolExpr ->
      ( some_result
          ~diags:(warn_before_state_reset state)
          (Expr expr &@<- abbrev_cond),
        no_subject_state )
    | CondObject ->
      begin match state.subject, state.relop with
      | Some subj, Some relop ->
        ( some_result (Relation (subj, ~&relop, expr) &@<- abbrev_cond),
          { state with unused = Nothing } )
      | None, _ ->
        ( no_result ~diags:[Condition_error (AbbrevMissingSubject ~@abbrev_cond)],
          state )
      | Some _, None ->
        ( no_result ~diags:[Condition_error (AbbrevMissingRelOp ~@abbrev_cond)],
          state )
      end
    | CondUnknown -> no_result ~diags:[], state
    end
  | CondParen c ->
    let exp_cond, new_state =
      expand_abbrev_cond env c { state with changed_in_parens = Nothing }
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
    let exp_cond, state = expand_abbrev_cond env c state in
    map_some_result ~f:(fun ec -> Not ec &@<- abbrev_cond) exp_cond, state
  | CondCombined (a, o, b) ->
    let exp_a, state = expand_abbrev_cond env a state in
    let exp_b, state = expand_abbrev_cond env b state in
    ( merge_results
        ~f:(fun a b ->
          match a, b with
          | Some a, Some b -> Some (Combined (a, o, b) &@<- abbrev_cond)
          | _ -> None
        )
        exp_a exp_b,
      state )
  | CondClass cl ->
    use_and_reset_subject
      ~f:(fun subj -> ClassCond (subj, cl) &@<- abbrev_cond)
      ~missing_subject_error:(ClassCondMissingSubject ~@abbrev_cond) state
  | CondSign sign ->
    use_and_reset_subject
      ~f:(fun subj -> SignCond (subj, sign) &@<- abbrev_cond)
      ~missing_subject_error:(SignCondMissingSubject ~@abbrev_cond) state
  | CondOmitted ->
    use_and_reset_subject
      ~f:(fun subj -> Omitted subj &@<- abbrev_cond)
      ~missing_subject_error:(OmittedMissingSubject ~@abbrev_cond) state

let expand_condition env (cond : condition with_loc) :
    expanded_cond with_loc option with_diags =
  let c, _ = expand_abbrev_cond env cond no_subject_state in
  c

let expand_selection_subject env subj =
  match ~&subj with
  | Subject (CondObjectOrExpr subject) ->
    begin match disambiguate_object_vs_bool_expr env ~&subject with
    | CondObject ->
      some_result (SubjectValue subject &@<- subj)
    | CondBoolExpr ->
      some_result (SubjectCond (Expr subject) &@<- subj)
    | CondUnknown ->
      no_result ~diags:[]
    end
  | Subject s -> 
    map_some_result ~f:(fun c -> SubjectCond ~&c &@<- c) (expand_condition env (s &@<- subj))
  | SubjectConst b -> some_result (SubjectConst b &@<- subj)

(* WARNING: expanding using this function will potentially duplicate a SubjectValue 
  expression inside the resulting SelCond.
  By COBOL standard, subject's evaluation side effects should be performed only once in 
  the PERFORM statement. 
  Consider replacing SubjectValue complex expressions with a temporary variable before 
  expanding if the resulting expanded_selection_object should be used for evaluation.
  However note that both GnuCOBOL 3.3 and MF duplicate subject evaluation side-effects. *)
let expand_selection_object env subj (obj: selection_object with_loc) =
  match ~&subj, ~&obj with
  | _, SelAny -> some_result (SelAny &@<- obj)
  | _, SelConst oc -> 
    some_result (SelConst oc &@<- obj)
  | (SubjectConst _ | SubjectCond _), SelCond c ->
    map_some_result ~f:(fun x -> SelCond ~&x &@<- x) (expand_condition env (c &@<- obj))
  | SubjectValue _, SelCond (CondObjectOrExpr value) when 
      disambiguate_object_vs_bool_expr env ~&value = CondObject ->
    some_result (SelValue { negated = false; value } &@<- obj)
  | SubjectValue _, SelCond (CondNot { payload = CondObjectOrExpr value; _ }) when 
      disambiguate_object_vs_bool_expr env ~&value = CondObject ->
    some_result (SelValue { negated = true; value } &@<- obj)
  | SubjectValue subject, SelCond c ->
    let xc, _ = 
      expand_abbrev_cond env (c &@<- obj) 
        { 
          subject = Some subject; 
          relop = Some (Eq &@<- subj); 
          unused = Nothing; 
          changed_in_parens = Nothing 
        }
    in
    map_some_result ~f:(fun x -> SelCond ~&x &@<- x) xc
  | SubjectValue _, SelRange r ->
    some_result (SelRange r &@<- obj)
  | _, SelRange _ -> 
    no_result ~diags:[Condition_error (SelRangeWithoutSubjectValue ~@obj)]

    
(* --- *)
  
let check_procedure ~env ~(diags: Typeck_diagnostics.t) procedure =
  let visitor = object
    inherit [Typeck_diagnostics.t] Cobol_unit.Visitor.folder

    method! fold_condition' c diags =
      let c = expand_condition env c in
      Visitor.skip_children @@ Typeck_diagnostics.union diags (forget_result c)

    method! fold_evaluate' eval_stmt diags =
      let diags, expanded_subjects = List.fold_left_map (fun diags s -> 
          let subj = expand_selection_subject env s in
          Typeck_diagnostics.union diags subj.diags, subj.result
        ) diags ~&eval_stmt.eval_subjects
      in
      let diags = List.fold_left (fun diags branch -> 
          List.fold_left (fun diags obj_list -> 
              try 
                List.fold_left2 (fun diags subj_opt obj ->
                    match subj_opt with
                    | Some subj ->
                      Typeck_diagnostics.union diags (forget_result (expand_selection_object env subj obj))
                    | None -> diags (* Do not add object error if the subject is already invalid *)
                  ) diags expanded_subjects obj_list
              with
              | Invalid_argument _ -> 
                  Typeck_diagnostics.union diags [Condition_error (MismatchingSelectionLength { 
                    nb_sel_subjects = List.length expanded_subjects; sel_object_list = obj_list })]
            ) diags branch.eval_selection
        ) diags ~&eval_stmt.eval_branches
      in
      Visitor.skip_children @@ diags
  end in

  Cobol_unit.Visitor.fold_procedure visitor procedure diags
