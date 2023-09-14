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
open Cobol_data.Pictured_ast.Data_sections

exception SemanticError of string * srcloc

(* This follows the 85 standard and IBM dialect specification. *)
(* TODO: accept a `data_item with_loc` instead *)
let validate_data_clauses
    (module Diags: Cobol_common.Diagnostics.STATEFUL)
    ?(is_elementary = false)
    ({ payload = { data_clauses; _ }; loc }: data_item with_loc)
  =
  (* This does not generalize well to other mutual exclusion constraints.

     TODO: Instead, we should accumulate locations of clauses and do single
     mutual exclusion checks at the end. *)
  let has_usage                   = ref false in
  let has_usage_index             = ref false in
  let has_usage_pointer           = ref false in
  let has_usage_function_pointer  = ref false in
  let has_usage_procedure_pointer = ref false in
  let has_usage_object_reference  = ref false in
  let has_picture_clause          = ref false in
  List.iter begin fun { payload = clause; loc } -> match clause with
    | DataBlankWhenZero when not is_elementary ->
        Diags.error ~loc "BLANK-WHEN-ZERO clause is forbidden in non elementary \
                          data item"
    | DataJustified when not is_elementary ->
        Diags.error ~loc "JUSTIFIED clause is forbidden in non elementary data \
                          item"
    | DataPicture _ when not is_elementary ->
        Diags.error ~loc "PICTURE clause is forbidden in non elementary data item"
    | DataPicture _ ->
        has_picture_clause := true;
        if !has_usage_index then
          Diags.error ~loc "PICTURE clause is forbidden when USAGE INDEX is \
                            specified";
        if !has_usage_pointer then
          Diags.error ~loc "PICTURE clause is forbidden when USAGE POINTER is \
                            specified";
        if !has_usage_function_pointer then
          Diags.error ~loc "PICTURE clause is forbidden when USAGE \
                            FUNCTION-POINTER is specified";
        if !has_usage_procedure_pointer then
          Diags.error ~loc "PICTURE clause is forbidden when USAGE \
                            PROCEDURE-POINTER is specified";
        if !has_usage_object_reference then
          Diags.error ~loc "PICTURE clause is forbidden when USAGE OBJECT \
                            REFERENCE is specified";
    | DataSynchronized _ when not is_elementary ->
        Diags.error ~loc "SYNCHRONIZED clause is forbidden in non elementary \
                          data item"
    | DataUsage _ when !has_usage ->
        Diags.error ~loc "Only one USAGE clause is allowed."
    | DataUsage usage_clause ->
        has_usage := true;
        begin match usage_clause with
          | Index when !has_picture_clause ->
              Diags.error ~loc "USAGE INDEX clause is forbidden when PICTURE is \
                                specified"
          | Pointer _ when !has_picture_clause ->
              Diags.error ~loc "USAGE POINTER clause is forbidden when PICTURE \
                                is specified"
          | FunctionPointer _ when !has_picture_clause ->
              Diags.error ~loc "USAGE FUNCTION-POINTER clause is forbidden when \
                                PICTURE is specified"
          | ProgramPointer _ when !has_picture_clause ->
              Diags.error ~loc "USAGE PROCEDURE-POINTER clause is forbidden when \
                                PICTURE is specified"
          | ObjectReference _ when !has_picture_clause ->
              Diags.error ~loc "USAGE OBJECT REFERENCE clause is forbidden when \
                                PICTURE is specified"
          | Index ->
              has_usage_index := true
          | Pointer _ ->
              has_usage_pointer := true
          | FunctionPointer _ ->
              has_usage_function_pointer := true
          | ProgramPointer _ ->
              has_usage_procedure_pointer := true
          | ObjectReference _ ->
              has_usage_object_reference := true
          | _ -> ()
        end
    | _ -> ()
  end data_clauses;
  if is_elementary && not !has_picture_clause && not !has_usage then
    Diags.error ~loc "There must be either a PICTURE or a USAGE clause in an \
                      elementary item description"
