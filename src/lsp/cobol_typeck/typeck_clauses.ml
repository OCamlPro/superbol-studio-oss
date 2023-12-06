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
open Typeck_diagnostics

type data_clauses =
  {
    occurs: occurs_under_construction;
    picture: Cobol_ptree.picture_clause with_loc option;
    values: Cobol_ptree.data_value_clause with_loc list;
    redefines: Cobol_ptree.name with_loc option;
    clause_diags: diagnostics;
  }

and occurs_under_construction =
  | OccursOnce
  | FixedOccurs of { length: Cobol_ptree.integer with_loc;
                     (* value: Cobol_ptree.literal option; *)
                     loc: srcloc }
  | OccursDepending of { min_occurs: Cobol_ptree.integer with_loc;
                         max_occurs: Cobol_ptree.integer with_loc;
                         depending: Cobol_ptree.qualname with_loc;
                         (* value: Cobol_ptree.literal with_loc option; *)
                         loc: srcloc }
  | OccursDynamic of { capacity: Cobol_ptree.name with_loc option;
                       min_capacity: Cobol_ptree.integer with_loc option;
                       max_capacity: Cobol_ptree.integer with_loc option;
                       initialized: bool with_loc;
                       loc: srcloc }


let init_clauses =
  {
    occurs = OccursOnce;
    picture = None;
    values = [];
    redefines = None;
    clause_diags = [];
  }


let clause_error acc error =
  { acc with clause_diags = Data_error error :: acc.clause_diags }

let clause_diagnostic acc diag =
  { acc with clause_diags = diag :: acc.clause_diags }

let register_used_feature acc ~loc:usage_loc ~feature =
  clause_diagnostic acc @@ Dialect_feature_used { feature; usage_loc }


let on_occurs_clause acc (c: Cobol_ptree.data_occurs_clause with_loc) =
  match acc.occurs, ~&c with
  | FixedOccurs { loc; _ }, _
  | OccursDepending { loc; _ }, _
  | OccursDynamic { loc; _ }, _ ->
      clause_error acc (Duplicate_clause { clause_name = "OCCURS";
                                           first_loc = loc;
                                           second_loc = ~@c })
  | OccursOnce, OccursFixed { times; _ } ->
      { acc with occurs = FixedOccurs { length = times;
                                        loc = ~@c } }
  | OccursOnce, OccursDepending { from; to_; depending; _ } ->
      { acc with occurs = OccursDepending { min_occurs = from;
                                            max_occurs = to_;
                                            depending;
                                            loc = ~@c } }
  | OccursOnce, OccursDynamic { capacity_in; from; to_; initialized; _ } ->
      { acc with occurs = OccursDynamic { capacity = capacity_in;
                                          min_capacity = from;
                                          max_capacity = to_;
                                          initialized;
                                          loc = ~@c } }


let on_unique_clause ~clause_name ~f prev_val acc clause =
  match prev_val with
  | Some { loc; _ } ->
      clause_error acc (Duplicate_clause { clause_name;
                                           first_loc = loc;
                                           second_loc = ~@clause })
  | None ->
      f acc clause


let on_picture_clause acc =
  on_unique_clause ~clause_name:"PICTURE" acc.picture acc
    ~f:(fun acc clause -> { acc with picture = Some clause })


let on_value_clause acc clause =
  { acc with values = clause :: acc.values }


let on_redefines_clause acc =
  on_unique_clause ~clause_name:"REDEFINES" acc.redefines acc
    ~f:begin fun acc clause ->
      let acc =
        if acc != init_clauses then  (* note: hackish use of physical equality *)
          register_used_feature acc
            ~feature:Cobol_config.Options.free_redefines_position
            ~loc:~@clause
        else acc
      in
      { acc with redefines = Some ~&clause }
    end


let of_data_item (data_clauses: Cobol_ptree.data_clause with_loc list) =
  List.fold_left begin fun acc { payload = clause; loc } ->
    match (clause: Cobol_ptree.data_clause) with
    | DataOccurs    o -> on_occurs_clause acc (o &@ loc)
    | DataRedefines r -> on_redefines_clause acc (r &@ loc)
    | DataPicture   p -> on_picture_clause acc p
    | DataValue     d -> on_value_clause acc (d &@ loc)
    | _ -> acc
  end init_clauses data_clauses
