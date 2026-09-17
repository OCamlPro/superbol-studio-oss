(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Cobol_ptree                                            (* for statements *)
open Cobol_unit.Types
open Cir_types
open Types

open Syntax
open Syntax.INFIX

(* --- *)

let errors e = Error e
let error e = errors (NEL.one e)

let append_lists r r' =
  let* r and* r' in
  Ok (LIST.append ~loc:__LOC__ r r')

(* --- *)

type 'f code_block_builder =
  ('f Cir_types.code_block, error nel) result Cobol_unit.Visitor.folder

let translate_statements (visitor: _ code_block_builder) statements =
  List.rev =|< Cobol_ptree.Visitor.fold_statements visitor statements (Ok [])

let translate_if_statement env visitor stmt =
  let* condition = Expr.resolve_condition env ~&stmt.condition
  and* then_branch = translate_statements visitor ~&stmt.then_branch
  and* else_branch = translate_statements visitor ~&stmt.else_branch in
  Ok [IR_conditional { condition; then_branch; else_branch } &@<- stmt]

let translate_display_statement env stmt =
  let* rev_fields =
    List.fold_left begin fun acc { display_items; _ } ->
      List.fold_left begin fun acc term ->
        let* field = Expr.resolve_data_reference env term and* acc in
        Ok (field :: acc)
      end acc display_items
    end (Ok []) ~&stmt.display_items_clauses
  in
  Ok [IR_display { data_refs = Array.of_list @@ List.rev rev_fields;
                   advancing = not ~&stmt.no_advancing } &@<- stmt]

let translate_stop_statement env stmt =
  match ~&stmt with
  | StopRun None ->
      Ok [IR_stop { optional_status = None } &@<- stmt]
  | StopArg Some StopWithQualIdent ident ->
      let* f = Expr.resolve_data_reference env ident in
      Ok [IR_stop { optional_status = Some f } &@<- stmt]
  | StopRun Some _
  | StopArg _
  | StopError
  | StopThread _ ->
      error @@ Unsupported { stuff = Statement (Stop ~&stmt); loc = ~@stmt }

let translate_procedure visitor p =
  (* TODO: for now, assumes a sequence of statements *)
  List.rev =|< Cobol_unit.Visitor.fold_procedure visitor p (Ok [])

let statements_builder env =
  let append_statements acc r = append_lists r acc in
  object (visitor)
    inherit [_] Cobol_unit.Visitor.folder
    method! fold_statement' s acc =
      Cobol_common.Visitor.do_children_and_then acc begin fun acc' ->
        if acc == acc'                      (* Note: rely on physical equality *)
        then error @@ Unsupported { stuff = Statement ~&s; loc = ~@s }
        else acc'
      end

    method! fold_if' s acc =
      Cobol_common.Visitor.skip @@
      append_statements acc @@ translate_if_statement env visitor s

    method! fold_display' s acc =
      Cobol_common.Visitor.skip @@
      append_statements acc @@ translate_display_statement env s

    method! fold_stop' s acc =
      Cobol_common.Visitor.skip @@
      append_statements acc @@ translate_stop_statement env s
  end

let translate env (p: procedure) : (_ code_block, _) result =
  let visitor = statements_builder env in
  translate_procedure visitor p
