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
open Types

open Syntax

(* --- *)

let errors e = Error e
let error e = errors (NEL.one e)

(* --- *)

let translate_display_statement env stmt =
  let* rev_fields =
    List.fold_left begin fun acc { display_items; _ } ->
      List.fold_left begin fun acc term ->
        Error.cons_errors (Env.resolve_term env term) acc
      end acc display_items
    end (Ok []) ~&stmt.display_items_clauses
  in
  Ok [Core_display { fields = Array.of_list @@ List.rev rev_fields;
                     advancing = not ~&stmt.no_advancing } &@<- stmt]

let translate_stop_statement env stmt =
  match ~&stmt with
  | StopRun None ->
      Ok [Core_stop { optional_status = None } &@<- stmt]
  | StopArg Some StopWithQualIdent ident ->
      let* f = Env.resolve_term env ident in
      Ok [Core_stop { optional_status = Some f } &@<- stmt]
  | StopRun Some _
  | StopArg _
  | StopError
  | StopThread _ ->
      error @@ Unsupported { stuff = Statement (Stop ~&stmt); loc = ~@stmt }


let translate_procedure env (p: procedure) : (_ code_block, _) result =
  let append_statements acc r = Error.append_lists r acc in
  let* core_statements =
    Cobol_unit.Visitor.fold_procedure (object
      inherit [_] Cobol_unit.Visitor.folder
      method! fold_statement' s acc =
        Cobol_common.Visitor.do_children_and_then acc begin fun acc' ->
          if acc == acc'                    (* Note: rely on physical equality *)
          then error @@ Unsupported { stuff = Statement ~&s; loc = ~@s }
          else acc'
        end

      method! fold_display' s acc =
        Cobol_common.Visitor.skip @@
        append_statements acc @@ translate_display_statement env s

      method! fold_stop' s acc =
        Cobol_common.Visitor.skip @@
        append_statements acc @@ translate_stop_statement env s

    end) p (Ok [])
  in
  Ok (List.rev core_statements)
