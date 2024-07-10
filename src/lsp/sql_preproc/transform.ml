(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open Sql_ast
open Types

let num = ref 0

let transform_stm (_, stm) =
  match stm with
  | EXEC_SQL { tokens; _ } -> (
    match tokens with
    | SelectInto { select; select_options; _ } ->
      let s =  Format.asprintf "SELECT %a%a" Printer.pp_select_lst select Printer.pp_select_options_lst select_options in 
      let size = String.length s in 
      num:=!num+1;
      "       01  SQ"^string_of_int !num^".\n\
      \           02  FILLER PIC X("^string_of_int size^") VALUE \"" ^ s ^ "\".\n\
      \           02  FILLER PIC X(1) VALUE X\"00\".\n"
    | _ -> "" )
  | _ -> ""

let rec transform cobol_unit sql_statements =
  match sql_statements with
  | h :: t -> 
    let (sql, _) = transform cobol_unit t in 
    (transform_stm h ^ sql, cobol_unit)
  | [] -> ("", cobol_unit)
