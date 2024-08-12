(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open Cobol_indent.Types
open Types

let loc_of_edit ~filename e =
  { filename;
    line = e.tok_edit.edit.lnum;
    char = e.tok_indent + e.tok_edit.edit.offset_orig
  }

let error ?loc fmt =
  Printf.kprintf
    (fun s ->
      Printf.eprintf "Error";
      begin
        match loc with
        | None -> ()
        | Some loc -> Printf.eprintf " at %s:%d" loc.filename loc.line
      end;
      Printf.eprintf ": %s\n%!" s;
      Printf.eprintf "Aborting.\n%!";
      exit 2 )
    fmt

let warning ?loc fmt =
  Printf.kprintf
    (fun s ->
      Printf.eprintf "Warning";
      begin
        match loc with
        | None -> ()
        | Some loc -> Printf.eprintf " at %s:%d" loc.filename loc.line
      end;
      Printf.eprintf ": %s\n%!" s )
    fmt

let string_of_token = function
  | IDENT tok -> tok
  | CHARS tok -> tok
  | INTEGER tok -> tok
  | NUMBER tok -> tok
  | DOT -> "."
  | LPAREN -> "("
  | RPAREN -> ")"
  | EQUALEQUAL -> "=="
  | COMMA -> ","
  | SEMI -> ";"
  | COLON -> ":"
  | EQUAL -> "="
  | MINUS -> "-"
  | PLUS -> "+"
  | GT -> ">"
  | LT -> "<"
  | GTE -> ">="
  | LTE -> "<="
  | DIV -> "/"
  | MUL -> "*"
  | DOLLAR -> "$"
  | AMPER -> "&"
  | SHARP -> "#"
  | tok -> (
    try Hashtbl.find Cobol_indent.Lexer.keyword2string tok with
    | Not_found -> failwith (Cobol_indent.Misc.string_of_token tok) )

let add_dot ~with_dot b = if with_dot then Printf.bprintf b "            .\n"

let resolve_copy ~config file =
  let rec iter_exts exts =
    match exts with
    | [] -> raise Not_found
    | ext :: exts -> (
      let file = String.lowercase_ascii (file ^ ext) in
      match iter_paths file (Lazy.force config.copy_path) with
      | exception Not_found -> iter_exts exts
      | filename -> filename )
  and iter_paths file path =
    match path with
    | [] -> raise Not_found
    | (dir, map) :: path -> (
      match StringMap.find file map with
      | exception Not_found -> iter_paths file path
      | file -> Filename.concat dir file )
  in
  iter_exts config.copy_exts

let rec extract_cob_complex_lit = function
  | Sql_ast.SqlCompLit (LiteralVar (CobolVar variable))
  | SqlCompAs (LiteralVar (CobolVar variable), _) ->
    [ variable ]
  | SqlCompFun (_, sql_op_list) -> extract_cob_var_select sql_op_list
  | _ -> []

and extract_cob_var_sql_op = function
  | Sql_ast.SqlOpLit compl_lit -> extract_cob_complex_lit compl_lit
  | SqlOpBinop (_, compl_lit, sql_op) ->
    extract_cob_complex_lit compl_lit @ extract_cob_var_sql_op sql_op

and extract_cob_var_select = function
  | h :: t -> extract_cob_var_sql_op h @ extract_cob_var_select t
  | [] -> []

and extract_lit = function
  | Sql_ast.LiteralVar (CobolVar variable) -> [ variable ]
  | _ -> []

and extract_from_join_option = function
  | Sql_ast.JoinOn sc -> extract_from_search_condition sc
  | _ -> []

and extract_from_search_condition = function
  | WhereConditionOr (search_condition1, search_condition2)
  | WhereConditionAnd (search_condition1, search_condition2) ->
    extract_from_search_condition search_condition1
    @ extract_from_search_condition search_condition2
  | WhereConditionNot search_condition ->
    extract_from_search_condition search_condition
  | WhereConditionCompare sql_compare -> (
    match sql_compare with
    | CompareQuery (complex_literal, _, sql_instruction) ->
      extract_cob_complex_lit complex_literal @ extract_cob_var sql_instruction
    | CompareLit (complex_literal1, _, complex_literal2) ->
      extract_cob_complex_lit complex_literal1
      @ extract_cob_complex_lit complex_literal2 )
  | WhereConditionIn (InVarLst (lit, comp_lit_list)) ->
    let rec extract_comp_lit_list lst =
      match lst with
      | h :: t -> extract_cob_complex_lit h @ extract_comp_lit_list t
      | [] -> []
    in
    extract_lit lit @ extract_comp_lit_list comp_lit_list
  | WhereConditionBetween (Between (l1, l2, l3)) ->
    extract_lit l1 @ extract_lit l2 @ extract_lit l3
  | WhereConditionIsNull variable -> (
    match variable with
    | CobolVar v -> [ v ]
    | SqlVar _ -> [] )

and extract_cob_var_select_option = function
  | Sql_ast.From from_stm ->
    let rec extract_from_stm = function
      | h :: t ->
        let rec extract_from_tbl_ref = function
          | Sql_ast.FromLitAs (table_ref, literal) ->
            extract_from_tbl_ref table_ref @ extract_lit literal
          | FromLit literal -> extract_lit literal
          | FromSelect sql_query -> extract_cob_var_query sql_query
          | Join (table_ref1, _, table_ref2, Some join_option) ->
            extract_from_tbl_ref table_ref1
            @ extract_from_tbl_ref table_ref2
            @ extract_from_join_option join_option
          | Join (table_ref1, _, table_ref2, _) ->
            extract_from_tbl_ref table_ref1 @ extract_from_tbl_ref table_ref2
        in
        extract_from_tbl_ref h @ extract_from_stm t
      | [] -> []
    in
    extract_from_stm from_stm
  | Sql_ast.Where search_condition
  | Having search_condition ->
    extract_from_search_condition search_condition
  | OrderBy sql_orderBy_list ->
    let rec extract_sql_orderBy = function
      | Sql_ast.Asc lit :: h
      | Desc lit :: h ->
        extract_lit lit @ extract_sql_orderBy h
      | [] -> []
    in
    extract_sql_orderBy sql_orderBy_list
  | GroupBy literal_list ->
    let rec extract_sql_lit_list = function
      | h :: t -> extract_lit h @ extract_sql_lit_list t
      | [] -> []
    in
    extract_sql_lit_list literal_list

and extract_cob_var_query sql_query =
  match sql_query with
  | Sql_ast.SelectUnion (sql_query1, sql_query2)
  | Sql_ast.SelectExcept (sql_query1, sql_query2)
  | Sql_ast.SelectIntersect (sql_query1, sql_query2) ->
    extract_cob_var_query sql_query1 @ extract_cob_var_query sql_query2
  | Sql_ast.SelectQuery (sql_select, sql_select_option_list) ->
    extract_cob_var_select sql_select
    @ extract_cob_var_select_option_list sql_select_option_list

and extract_cob_var_select_option_list = function
  | h :: t ->
    extract_cob_var_select_option h @ extract_cob_var_select_option_list t
  | [] -> []

and extract_cob_var sql =
  match sql with
  | Sql_ast.SqlVarToken (CobolVar variable) :: t ->
    variable :: extract_cob_var t
  | Sql_ast.SqlLit (LiteralVar (CobolVar variable)) :: t ->
    variable :: extract_cob_var t
  | Sql_ast.SqlQuery sql_query :: t ->
    extract_cob_var_query sql_query @ extract_cob_var t
  | [] -> []
  | _ :: t -> extract_cob_var t

let extract_filename path =
  let parts = Str.split (Str.regexp "/") path in
  let filename_with_ext = List.hd (List.rev parts) in
  let filename_parts = Str.split (Str.regexp "\\.") filename_with_ext in
  List.hd filename_parts

let replace_colon_words str =
  let buffer = Buffer.create (String.length str) in
  let count = ref 0 in
  let len = String.length str in
  let i = ref 0 in

  while !i < len do
    if str.[!i] = ':' then (
      incr count;
      Buffer.add_string buffer ("$" ^ string_of_int !count);
      incr i;
      while
        !i < len
        && str.[!i] <> ' '
        && str.[!i] <> ','
        && str.[!i] <> ')'
        && str.[!i] <> '('
      do
        incr i
      done
    ) else (
      Buffer.add_char buffer str.[!i];
      incr i
    )
  done;

  Buffer.contents buffer
