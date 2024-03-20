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

(* open Cobol_common.Srcloc.TYPES *)
(* open Cobol_data.Pictured_ast.Data_sections *)
module DIAGS = Cobol_common.Diagnostics
open Cobol_ptree

(* exception SemanticError of string * srcloc *)

(* This follows the 85 standard and IBM dialect specification. *)
let validate_data_clauses
    ?(is_elementary = false)
    Cobol_ptree.{ payload = Cobol_ptree.{ data_clauses; _ }; loc }
  =
  (* This does not generalize well to other mutual exclusion constraints.

     TODO: Instead, we should accumulate locations of clauses and do single
     mutual exclusion checks at the end. *)
  let has_usage                   = ref false in
  let has_usage_index             = ref false in
  let has_usage_pointer           = ref false in
  let has_usage_function_pointer  = ref false in
  let has_usage_procedure_pointer = ref false in
  let has_usage_program_pointer   = ref false in
  let has_usage_object_reference  = ref false in
  let has_picture_clause          = ref false in
  let error_if bool_ref diags ~loc fmt =
    if bool_ref
    then DIAGS.Acc.error diags ~loc fmt
    else Format.(ikfprintf (fun _ -> diags) str_formatter) fmt
  in
  let diags =
    List.fold_left begin fun diags { payload = clause; loc } -> match clause with
      | DataBlankWhenZero when not is_elementary ->
          DIAGS.Acc.error diags ~loc
            "BLANK-WHEN-ZERO clause is forbidden in non elementary data item"
      | DataJustified when not is_elementary ->
          DIAGS.Acc.error diags ~loc
            "JUSTIFIED clause is forbidden in non elementary data item"
      | DataPicture _ when not is_elementary ->
          DIAGS.Acc.error diags ~loc
            "PICTURE clause is forbidden in non elementary data item"
      | DataPicture _ ->
          has_picture_clause := true;
          diags |>
          fun diags -> error_if !has_usage_index diags ~loc
            "PICTURE clause is forbidden when USAGE INDEX is specified" |>
          fun diags -> error_if !has_usage_pointer diags ~loc
            "PICTURE clause is forbidden when USAGE POINTER is specified" |>
          fun diags -> error_if !has_usage_function_pointer diags ~loc
            "PICTURE clause is forbidden when USAGE FUNCTION-POINTER is \
             specified" |>
          fun diags -> error_if !has_usage_procedure_pointer diags ~loc
            "PICTURE clause is forbidden when USAGE PROCEDURE-POINTER is \
             specified" |>
          fun diags -> error_if !has_usage_program_pointer diags ~loc
            "PICTURE clause is forbidden when USAGE PROGRAM-POINTER is \
             specified" |>
          fun diags -> error_if !has_usage_object_reference diags ~loc
            "PICTURE clause is forbidden when USAGE OBJECT REFERENCE is \
             specified"
      | DataSynchronized _ when not is_elementary ->
          DIAGS.Acc.error diags ~loc
            "SYNCHRONIZED clause is forbidden in non elementary data item"
      | DataUsage _ when !has_usage ->
          DIAGS.Acc.error diags ~loc "Only one USAGE clause is allowed."
      | DataUsage usage_clause ->
          has_usage := true;
          DIAGS.Set.union diags @@
          (match usage_clause with
           | Index when !has_picture_clause ->
               DIAGS.Set.error ~loc
                 "USAGE INDEX clause is forbidden when PICTURE is specified"
           | Pointer _ when !has_picture_clause ->
               DIAGS.Set.error ~loc
                 "USAGE POINTER clause is forbidden when PICTURE is specified"
           | FunctionPointer _ when !has_picture_clause ->
               DIAGS.Set.error ~loc "USAGE FUNCTION-POINTER clause is forbidden \
                                     when PICTURE is specified"
           | ProcedurePointer when !has_picture_clause ->
               DIAGS.Set.error ~loc "USAGE PROCEDURE-POINTER clause is forbidden \
                                     when PICTURE is specified"
           | ProgramPointer _ when !has_picture_clause ->
               DIAGS.Set.error ~loc "USAGE PROGRAM-POINTER clause is forbidden \
                                     when PICTURE is specified"
           | ObjectReference _ when !has_picture_clause ->
               DIAGS.Set.error ~loc "USAGE OBJECT REFERENCE clause is forbidden \
                                     when PICTURE is specified"
           | Index ->
               has_usage_index := true;
               DIAGS.Set.none
           | Pointer _ ->
               has_usage_pointer := true;
               DIAGS.Set.none
           | FunctionPointer _ ->
               has_usage_function_pointer := true;
               DIAGS.Set.none
           | ProcedurePointer ->
               has_usage_procedure_pointer := true;
               DIAGS.Set.none
           | ProgramPointer _ ->
               has_usage_procedure_pointer := true;
               DIAGS.Set.none
           | ObjectReference _ ->
               has_usage_object_reference := true;
               DIAGS.Set.none
           | _ ->
               DIAGS.Set.none)
      | _ ->
          diags
    end DIAGS.Set.none data_clauses
  in
  if is_elementary && not !has_picture_clause && not !has_usage then
    DIAGS.Acc.error diags ~loc
      "There must be either a PICTURE or a USAGE clause in an elementary item \
       description"
  else diags
