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
module StringMap = Map.Make (String)

type variable_information =
  { length : int;
    vartype : int;
    scale : int;
    flags : int;
    ind_addr : int
  }

type t = variable_information StringMap.t

let add_var ~map ~name ?(length = 0) ?(vartype = 0) ?(scale = 0) ?(flags = 0)
    ?(ind_addr = 0) () =
  StringMap.add name { length; vartype; scale; flags; ind_addr } map

let num = ref 0

let transform_stm map (_, stm) =
  let create_new_var content =
    (*TODO: a function that cut (with &) the resquest if too long*)
    let size = String.length content in
    num := !num + 1;
    let name = "SQ" ^ string_of_int !num in
    ( "       01  " ^ name ^ ".\n           02  FILLER PIC X("
      ^ string_of_int size ^ ") VALUE \"" ^ content
      ^ "\".\n           02  FILLER PIC X(1) VALUE X\"00\".\n",
      add_var ~map ~name:("SQ" ^ string_of_int !num) ?length:(Some size) () )
  in
  match stm with
  | EXEC_SQL { tokens; _ } -> (
    match tokens with
    | SelectInto { select; select_options; _ } ->
      create_new_var
        (Format.asprintf "SELECT %a%a" Printer.pp_select_lst select
           Printer.pp_select_options_lst select_options )
    | Begin -> create_new_var ("BEGIN")
    | _ -> ("", map) )
  | DECLARATION { declaration; _ } -> (
    match declaration with
    | SQL_type_is { importance; name; sql_type; sql_type_size } -> begin
      match sql_type with
      | "BINARY"
      | "CHAR" ->
        let map =
          add_var ~map ~name ?length:(Some (int_of_string sql_type_size)) ()
        in
        ( "       " ^ importance ^ "  " ^ name ^ " PIC X(" ^ sql_type_size
          ^ ").\n",
          map )
      | "VARBINARY"
      | "VARCHAR" ->
        let map =
          add_var ~map ~name ?length:(Some (int_of_string sql_type_size)) ()
        in
        ( "       " ^ importance ^ "  " ^ name ^ ".\n           49 " ^ name
          ^ "-LEN PIC 9(8) COMP-5.\n           49 " ^ name ^ "-ARR PIC X("
          ^ sql_type_size ^ ").\n",
          map )
      | _ -> failwith "Unknow type."
    end )
  | _ -> ("", map)

let transform sql_statements =
  let rec transform_rec map sql_statements =
    match sql_statements with
    | h :: t ->
      let smt, map = transform_stm map h in
      let sql, map = transform_rec map t in
      (smt ^ sql, map)
    | [] -> ("", map)
  in
  let init_map = StringMap.empty in
  transform_rec init_map sql_statements

let find_opt map str = StringMap.find_opt str map
