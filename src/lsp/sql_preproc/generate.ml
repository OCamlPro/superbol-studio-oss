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
open Types
open Sql_ast

let linkage_section =
  {|      *> SQL addition in linkage section:
       01 SOME-ARG PIC X(9).
|}

let begin_procedure_division ~ctxt:_ ~loc:_ =
  (* We might want to add something at the begining of PROCEDURE DIVISION ? *)
  ()

let end_procedure_division ~ctxt:_ ~loc:_ =
  (* We might want to add something before the end of PROCEDURE DIVISION ? *)
  ()

(* let strlit lit = Format.asprintf "%a" Printer.pp_lit lit *)

(* let strlitopt = function
   | Some lit -> Some (strlit lit)
   | None -> None *)

let cob_var_id_opt (cob_var : cobolVarId option) =
  match cob_var with
  | Some cob -> Some cob.payload
  | None -> None

let cob_var_opt = function
  | Some var -> (
    match var with
    | CobVarNotNull cobolVarId -> Some cobolVarId.payload
    | CobVarNullIndicator (var, _) -> Some var.payload )
  | None -> None

let var_opt = function
  | Some var -> (
    match var with
    | SqlVar sqlVarToken -> Some sqlVarToken.payload
    | CobolVar cobol_var -> cob_var_opt (Some cobol_var) )
  | None -> None

let generate ~filename ~contents ~cobol_unit sql_statements =
  (*TODO get function*)
  let get_length str = Sql_typeck.get_size cobol_unit str in

  let get_some_length var =
    match var_opt var with
    | Some x -> Some (get_length x)
    | None -> None
  in

  let get_some_cob_var_length (cob_var : cobolVarId option) =
    match cob_var with
    | Some x -> Some (get_length x.payload)
    | None -> None
  in

  let get_type _str = 16 in
  let get_scale _str = 0 in
  let get_flags _str = 0 in
  let get_ind_addr _str = 0 in

  let working_storage_section, _cobol_unit =
    Transform.transform cobol_unit sql_statements
  in

  (* split lines and numerotate them *)
  let lines = EzString.split contents '\n' in
  let lines = List.mapi (fun i line -> (filename, i + 1, line)) lines in
  (*string to add at the end of every sql processed*)
  let error_treatment = ref "" in
  let print_error_treatement ctxt =
    if !error_treatment <> "" then (
      Printf.bprintf ctxt.b "           EVALUATE TRUE\n";
      Printf.bprintf ctxt.b "%s\n" !error_treatment;
      Printf.bprintf ctxt.b "           END-EVALUATE\n"
    )
  in

  (* The result will be stored in this buffer: *)
  let b = Buffer.create 1000 in

  let ctxt = { b; main_filename = filename } in
  let final_loc = { filename; line = -1; char = 0 } in

  let generatesql_connect ?(data_source = "x\"00\"") ?(data_source_tl = 0)
      ?(d_connection_id = "x\"00\"") ?(connection_id_tl = 0)
      ?(d_dbname = "x\"00\"") ?(dbname_tl = 0) ?(d_username = "x\"00\"")
      ?(username_tl = 0) ?(d_password = "x\"00\"") ?(password_tl = 0) () =
    "           CALL STATIC \"GIXSQLConnect\" USING\n\
    \                 BY REFERENCE SQLCA\n\
    \                 BY REFERENCE " ^ data_source
    ^ "\n                 BY VALUE "
    ^ string_of_int data_source_tl
    ^ "\n                 BY REFERENCE " ^ d_connection_id
    ^ "\n                 BY VALUE "
    ^ string_of_int connection_id_tl
    ^ "\n                 BY REFERENCE " ^ d_dbname
    ^ "\n                 BY VALUE " ^ string_of_int dbname_tl
    ^ "\n                 BY REFERENCE " ^ d_username
    ^ "\n                 BY VALUE " ^ string_of_int username_tl
    ^ "\n                 BY REFERENCE " ^ d_password
    ^ "\n                 BY VALUE " ^ string_of_int password_tl
    ^ "\n             END-CALL"
  in

  let generatesql_connect_reset ?(d_connection_id = "x\"00\"")
      ?(connection_id_tl = 0) () =
    "               CALL STATIC \"GIXSQLConnectReset\" USING\n\
    \               BY REFERENCE SQLCA\n\
    \               BY REFERENCE " ^ d_connection_id
    ^ "\n               BY VALUE "
    ^ string_of_int connection_id_tl
    ^ "\n           END-CALL"
  in

  let generate_whenever_continuation = function
    | Continue -> "CONTINUE"
    | Perform sqlVarToken -> "PERFORM " ^ sqlVarToken.payload
    | Goto sqlVarToken -> "GOTO " ^ sqlVarToken.payload
  in

  let generate_whenever c k =
    match c with
    | Not_found_whenever ->
      "           WHEN SQLCODE = 100\n           "
      ^ generate_whenever_continuation k
      ^ "\n"
    | SqlError_whenever ->
      "           WHEN SQLCODE < 0\n           "
      ^ generate_whenever_continuation k
      ^ "\n"
    | SqlWarning_whenever ->
      "           WHEN SQLCODE < 0\n           "
      ^ generate_whenever_continuation k
      ^ "\n"
  in

  let get_name_cobol_var (cobol_var : cobol_var) =
    match cobol_var with
    | CobVarNotNull c -> c.payload
    | CobVarNullIndicator (c, n) -> c.payload ^ n.payload
  in

  let rec generate_select_into_rec vars =
    match vars with
    | h :: t ->
      let h = get_name_cobol_var h in
      "           CALL STATIC \"GIXSQLSetResultParams\" USING\n\
      \               BY VALUE "
      ^ string_of_int (get_type h)
      ^ "\n               BY VALUE "
      ^ string_of_int (get_length h)
      ^ "\n               BY VALUE "
      ^ string_of_int (get_scale h)
      ^ "\n               BY VALUE "
      ^ string_of_int (get_flags h)
      ^ "\n               BY REFERENCE " ^ h ^ "\n               BY REFERENCE "
      ^ string_of_int (get_ind_addr h)
      ^ "\n           END-CALL\n" ^ generate_select_into_rec t
    | [] -> ""
  in
  let generate_select_into_one vars =
    "           CALL STATIC \"GIXSQLExecSelectIntoOne\" USING\n\
    \               BY REFERENCE SQLCA\n\
    \               BY REFERENCE x\"00\"\n\
    \               BY VALUE 0\n\
    \               BY REFERENCE SQ0001\n\
    \               BY VALUE 0\n\
    \               BY VALUE 5\n"
    ^ string_of_int (List.length vars)
    ^ "\n               END-CALL\n"
  in

  let generate_select_into vars =
    "               CALL STATIC \"GIXSQLStartSQL\"\n               END-CALL"
    ^ generate_select_into_rec vars
    ^ generate_select_into_one vars
    ^ "               CALL STATIC \"GIXSQLEndSQL\"\n               END-CALL\n"
  in

  let generatesql ~loc ~line ~ctxt esql_instuction =
    match esql_instuction with
    | Include sqlvar ->
      let before_macro = String.sub line 0 loc.char in
      Printf.bprintf ctxt.b "%sCOPY %s\n" before_macro sqlvar.payload;
      print_error_treatement ctxt
    | Connect cs ->
      begin
        match cs with
        | Connect_reset lit ->
          Printf.bprintf ctxt.b "%s"
            (generatesql_connect_reset ?d_connection_id:(var_opt lit) ())
        | Connect_to_idby
            { dbname; db_conn_id; db_data_source; username; password } ->
          Printf.bprintf ctxt.b "%s"
            (generatesql_connect ~data_source:db_data_source.payload
               ~data_source_tl:(get_length db_data_source.payload)
               ?d_connection_id:(var_opt db_conn_id)
               ?connection_id_tl:(get_some_length db_conn_id)
               ~d_dbname:dbname.payload
               ~dbname_tl:(get_length dbname.payload)
               ~d_username:username.payload
               ~username_tl:(get_length username.payload)
               ~d_password:password.payload
               ~password_tl:(get_length password.payload)
               () )
        | Connect_to { db_conn_id; db_data_source; username; password } ->
          Printf.bprintf ctxt.b "%s"
            (generatesql_connect ~data_source:db_data_source.payload
               ~data_source_tl:(get_length db_data_source.payload)
               ?d_connection_id:(var_opt db_conn_id)
               ?connection_id_tl:(get_some_length db_conn_id)
               ~d_username:username.payload
               ~username_tl:(get_length username.payload)
               ?d_password:(cob_var_id_opt password)
               ?password_tl:(get_some_cob_var_length password)
               () )
        | Connect_using { db_data_source } ->
          Printf.bprintf ctxt.b "%s"
            (generatesql_connect ~data_source:db_data_source.payload
               ~data_source_tl:(get_length db_data_source.payload)
               () )
        | Connect_user { db_conn_id; db_data_source; username; password } ->
          Printf.bprintf ctxt.b "%s"
            (generatesql_connect
               ?data_source:(cob_var_id_opt db_data_source)
               ?data_source_tl:(get_some_cob_var_length db_data_source)
               ?d_connection_id:(var_opt db_conn_id)
               ?connection_id_tl:(get_some_length db_conn_id)
               ~d_username:username.payload
               ~username_tl:(get_length username.payload)
               ~d_password:password.payload
               ~password_tl:(get_length password.payload)
               () )
      end;
      print_error_treatement ctxt
    | Whenever (c, k) ->
      error_treatment := generate_whenever c k ^ !error_treatment
    | SelectInto { vars; _ } ->
      Printf.bprintf ctxt.b "%s" (generate_select_into vars)
    | _ -> ignore (loc, line, ctxt, esql_instuction)
  in

  let rec output lines statements =
    match statements with
    | [] ->
      List.iter (fun (_, _, line) -> Printf.bprintf ctxt.b "%s\n" line) lines
    | (begin_loc, stmt) :: statements -> begin
      match begin_loc with
      | None ->
        List.iter (fun (_, _, line) -> Printf.bprintf ctxt.b "%s\n" line) lines;
        begin
          match stmt with
          | END_PROCEDURE_DIVISION ->
            end_procedure_division ~ctxt ~loc:final_loc
          | _ -> ()
        end
      | Some begin_loc -> output_statement lines begin_loc stmt statements
    end
  and output_statement cur_lines begin_loc stmt statements =
    match cur_lines with
    | [] -> assert false
    | (filename, i, line) :: lines -> (
      if filename <> begin_loc.filename || i < begin_loc.line then begin
        Printf.bprintf ctxt.b "%s\n" line;
        output_statement lines begin_loc stmt statements
      end else
        match stmt with
        | LINKAGE_SECTION { defined } ->
          if defined then begin
            Printf.bprintf ctxt.b "%s\n" line;
            Buffer.add_string ctxt.b linkage_section;
            output lines statements
          end else begin
            Printf.bprintf ctxt.b "      *> Add missing LINKAGE SECTION\n";
            Printf.bprintf ctxt.b "       LINKAGE SECTION.\n";
            Buffer.add_string ctxt.b linkage_section;
            output cur_lines statements
          end
        | WORKING_STORAGE { defined } ->
          if defined then begin
            Printf.bprintf ctxt.b "%s\n" line;
            Buffer.add_string ctxt.b working_storage_section;
            output lines statements
          end else begin
            Printf.bprintf ctxt.b
              "      *> Add missing WORKING-STORAGE SECTION\n";
            Printf.bprintf ctxt.b "       WORKING-STORAGE SECTION.\n";
            Buffer.add_string ctxt.b working_storage_section;
            output cur_lines statements
          end
        | EXEC_SQL { end_loc; with_dot; tokens } ->
          Printf.bprintf ctxt.b "      *> REMOVED: %s\n" line;
          if i = end_loc.line then begin
            generatesql ~loc:begin_loc ~line ~ctxt tokens;
            (*  ignore (tokens); *)
            Misc.add_dot ~with_dot b;
            output lines statements
          end else
            output_statement lines begin_loc stmt statements
        | PROCEDURE_DIVISION_DOT { end_loc } ->
          Printf.bprintf ctxt.b "      *> REMOVED: %s\n" line;
          if i = end_loc.line then begin
            (* for now, just put it back *)
            Printf.bprintf ctxt.b "          PROCEDURE DIVISION.\n";
            output lines statements
          end else
            output_statement lines begin_loc stmt statements
        | IS_SQLVAR { end_loc } ->
          if i = begin_loc.line then begin
            let before_macro = String.sub line 0 begin_loc.char in
            Printf.bprintf ctxt.b "%s%s" before_macro
              "SOME STRING THAT REPLACE IS SQLVAR";
            if begin_loc.line <> end_loc.line then
              Printf.bprintf ctxt.b "\n          "
          end;
          if i = end_loc.line then (
            let len = String.length line in
            (* This code won't work with tabulations, because
               the end_loc.char is wrong in such a case *)
            let after_macro =
              String.sub line (end_loc.char + 1) (len - end_loc.char - 1)
            in
            Printf.bprintf ctxt.b "%s\n" after_macro;
            output lines statements
          ) else
            output_statement lines begin_loc stmt statements
        | BEGIN_PROCEDURE_DIVISION { enabled } ->
          if !enabled then
            begin_procedure_division ~ctxt ~loc:begin_loc
          else
            Printf.bprintf ctxt.b "      *> BEGIN PROCEDURE DIVISION disabled\n";
          output cur_lines statements
        | END_PROCEDURE_DIVISION ->
          end_procedure_division ~ctxt ~loc:begin_loc;
          output cur_lines statements
        | COPY { end_loc; filename; contents } ->
          Printf.bprintf ctxt.b "      *> INLINED: %s\n" line;
          if i = end_loc.line then begin
            let copylines = EzString.split contents '\n' in
            let copylines =
              List.mapi (fun i line -> (filename, i + 1, line)) copylines
            in
            let lines = copylines @ lines in
            output lines statements
          end else
            output_statement lines begin_loc stmt statements )
  in
  output lines sql_statements;
  Buffer.contents b
