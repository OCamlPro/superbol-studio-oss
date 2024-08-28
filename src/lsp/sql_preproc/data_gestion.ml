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
open Generated_type
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

let transform_stm map (_, stm) filename =
  let prefix = "       " in
  let create_new_var content ?(remplace=true) () =
    let new_content = if remplace then "\"" ^ Misc.replace_colon_words content ^ "\"" else content in
    let size = String.length new_content - 2 in
    (*Because " are part of this string"*)
    num := !num + 1;
    let var_name = "SQ" ^ string_of_int !num in
    let field =
      let prefix = prefix ^ "   " in
      [ Simple_var_declaration
          { prefix;
            var_importance = "02";
            var_name = None;
            var_type = "X(" ^ string_of_int size ^ ")";
            var_content = Some new_content
          };
        Simple_var_declaration
          { prefix;
            var_importance = "02";
            var_name = None;
            var_type = "X(1)";
            var_content = Some "X\"00\""
          }
      ]
    in
    ( [ Declaration
          (Field_var_declaration
             { prefix; var_importance = "01"; var_name; field } )
      ],
      add_var ~map ~name:("SQ" ^ string_of_int !num) ~length:size () )
  in
  let add_cur cur_name map ws filename =
    let pre_cur_name = "GIXSQL-CI-F-" ^ Misc.extract_filename filename ^ "-" in
    let ws =
      Declaration
        (Simple_var_declaration
           { prefix;
             var_importance = "01";
             var_name = Some (pre_cur_name ^ cur_name);
             var_type = "X";
             var_content = None
           } )
      :: ws
    in
    let map =
      add_var ~map ~name:(pre_cur_name ^ cur_name) ~length:0 ()
    in
    (ws, map)
  in

  let rec trans_sql tokens =
    match tokens with
    | At (_, sql) -> trans_sql sql
    | SelectInto { select; select_options; _ } ->
      let ws, map =
        create_new_var
          (Format.asprintf "SELECT %a%a" Sql_ast.Printer.pp_select_lst select
             Sql_ast.Printer.pp_select_options_lst select_options ) ()
      in
      (ws, map)
    | Begin ->
      let ws, map = create_new_var "BEGIN" () in
      (ws, map)
    | StartTransaction ->
      let ws, map = create_new_var "START TRANSACTION" () in
      (ws, map)
    | Sql sql -> (
      match sql with
      | Sql_ast.SqlInstr w :: _ when w = "VAR" ->
        ([], map)
        (*TODO: find what this should be replaced with. I think Gix juste ignorer these instruction, but mabe not*)
      | _ ->
        let ws, map =
          create_new_var (Format.asprintf "%a" Sql_ast.Printer.pp_sql sql) ()
        in
        (ws, map) )
    | Insert _
    | Savepoint _
    | ReleaseSavepoint _
    | Delete _ ->
      let ws, map =
        create_new_var (Format.asprintf "%a" Sql_ast.Printer.pp_esql tokens) ()
      in
      (ws, map)
    | ExecuteImmediate sql -> (
      match sql with
      | [ Sql_ast.SqlVarToken (CobolVar (CobVarNotNull _)) ] -> ([], map)
      | _ ->
        let ws, map =
          create_new_var (Format.asprintf "%a" Sql_ast.Printer.pp_sql sql) ()
        in
        (ws, map) )
    | Rollback (rb_work_or_tran, rb_args) -> begin
      match (rb_work_or_tran, rb_args) with
      | _, Some (To savepoint) ->
        let ws, map =
          create_new_var ("ROLLBACK TO SAVEPOINT " ^ savepoint.payload) ()
        in
        (ws, map)
      | _ -> ([], map)
    end
    | DeclareCursor cur -> begin
      match cur with
      | DeclareCursorSql (cur_name, query)
      | DeclareCursorWhithHold (cur_name, query) -> (*TODO: WhithHold specificity if there are any*)
        let ws, map =
          create_new_var
            (Format.asprintf "%a" Sql_ast.Printer.pp_sql_query query) ()
        in
        let ws, map = add_cur cur_name.payload map ws filename in

        (ws, map)
      | DeclareCursorVar (cur_name, var_name) ->
        let ws, map =
          create_new_var (Format.asprintf "\"@%a\"" Sql_ast.Printer.pp_var var_name) ~remplace:false ()
        in
        let ws, map = add_cur cur_name.payload map ws filename in

        (ws, map)
    end
    | Exeption _ ->
      create_new_var (Format.asprintf "%a" Sql_ast.Printer.pp_esql tokens) ()
    | _ -> ([], map)
  in

  let trans_declaration declaration =
    match declaration with
    | SQL_type_is { importance; name; sql_type; sql_type_size } -> begin
      match sql_type with
      | "BINARY"
      | "CHAR" ->
        let map =
          add_var ~map ~name ~length:(int_of_string sql_type_size) ()
        in

        ( [ Declaration
              (Simple_var_declaration
                 { prefix;
                   var_importance = importance;
                   var_name = Some name;
                   var_type = "X(" ^ sql_type_size ^ ")";
                   var_content = None
                 } )
          ],
          map )
      | "VARBINARY"
      | "VARCHAR" ->
        let map =
          add_var ~map ~name ~length:(int_of_string sql_type_size) ()
        in
        let field =
          let prefix = prefix ^ "   " in
          [ Simple_var_declaration
              { prefix;
                var_importance = "49";
                var_name = Some (name ^ "-LEN");
                var_type = "9(8) COMP-5";
                var_content = None
              };
            Simple_var_declaration
              { prefix;
                var_importance = "49";
                var_name = Some (name ^ "-ARR");
                var_type = "X(" ^ sql_type_size ^ ")";
                var_content = None
              }
          ]
        in

        let decl =
          Field_var_declaration
            { prefix; var_importance = importance; var_name = name; field }
        in

        ( [ Declaration decl ],
          (* "       " ^ importance ^ "  " ^ name ^ ".\n           49 " ^ name
             ^ "-LEN PIC 9(8) COMP-5.\n           49 " ^ name ^ "-ARR PIC X("
             ^ sql_type_size ^ ").\n", *)
          map )
      | _ -> failwith "Unknow type."
    end
  in

  match stm with
  | EXEC_SQL { tokens; _ } -> trans_sql tokens
  | DECLARATION { declaration; _ } -> trans_declaration declaration
  | _ -> ([], map)

let transform sql_statements filename =
  let rec transform_rec map sql_statements =
    match sql_statements with
    | h :: t ->
      let ws, map = transform_stm map h filename in
      let ws', map = transform_rec map t in
      (ws @ ws', map)
    | [] -> ([], map)
  in
  let init_map = StringMap.empty in
  transform_rec init_map sql_statements

let find_opt map str = StringMap.find_opt str map
