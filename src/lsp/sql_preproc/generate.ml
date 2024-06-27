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

let working_storage_section =
  {|      *> SQL addition in working storage section:
       01 MY-SQL-STUFF PIC X(9).
|}

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

let generate ~filename ~contents sql_statements =

  (* split lines and numerotate them *)
  let lines = EzString.split contents '\n' in
  let lines = List.mapi (fun i line -> (filename, i+1, line)) lines in

  (* The result will be stored in this buffer: *)

  let b = Buffer.create 1000 in

  let ctxt = { b ;
               main_filename = filename ;
             } in
  let final_loc = { filename; line = -1; char = 0 } in

  
let _generatesql ~loc ~line ~ctxt esql_instuction = 
    match esql_instuction with
    | Include sqlvar -> 
      let before_macro = String.sub line 0 loc.char in
                Printf.bprintf ctxt.b "%sCOPY %s\n" before_macro
                sqlvar.payload;
    | _ -> ignore(loc, line, ctxt, esql_instuction)
  in 

  let rec output lines statements =
    match statements with
    | [] ->
        List.iter (fun (_,_,line) ->
            Printf.bprintf ctxt.b "%s\n" line
          ) lines
    | (begin_loc, stmt) :: statements ->
        match begin_loc with
        | None ->
            List.iter (fun (_,_,line) ->
                Printf.bprintf ctxt.b "%s\n" line
              ) lines ;
            begin
              match stmt with
              | END_PROCEDURE_DIVISION ->
                  end_procedure_division ~ctxt ~loc:final_loc
              | _ -> ()
            end
        | Some begin_loc ->
            output_statement lines begin_loc stmt statements

  and output_statement cur_lines begin_loc stmt statements =
    match cur_lines with
    | [] -> assert false
    | (filename,i,line) :: lines ->
        if filename <> begin_loc.filename || i < begin_loc.line then begin
          Printf.bprintf ctxt.b "%s\n" line;
          output_statement lines begin_loc stmt statements
        end
        else
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
                Buffer.add_string ctxt.b working_storage_section ;
                output lines statements
              end else begin
                Printf.bprintf ctxt.b "      *> Add missing WORKING-STORAGE SECTION\n";
                Printf.bprintf ctxt.b "       WORKING-STORAGE SECTION.\n";
                Buffer.add_string ctxt.b working_storage_section;
                output cur_lines statements
              end
          | EXEC_SQL { end_loc ; with_dot ; tokens } ->
              Printf.bprintf ctxt.b "      *> REMOVED: %s\n" line;
              if i = end_loc.line then begin
             (*    generatesql ~loc:begin_loc ~line:line ~ctxt tokens;  *)
                ignore (tokens);
                Misc.add_dot ~with_dot b;
                output lines statements
              end else
                output_statement lines begin_loc
                  stmt statements;
          | PROCEDURE_DIVISION_DOT { end_loc } ->
              Printf.bprintf ctxt.b "      *> REMOVED: %s\n" line;
              if i = end_loc.line then begin
                (* for now, just put it back *)
                Printf.bprintf ctxt.b "          PROCEDURE DIVISION.\n";
                output lines statements
              end else
                output_statement lines begin_loc
                  stmt statements;
          | IS_SQLVAR { end_loc } ->
              if i = begin_loc.line then begin
                let before_macro = String.sub line 0 begin_loc.char in
                Printf.bprintf ctxt.b "%s%s" before_macro
                  "SOME STRING THAT REPLACE IS SQLVAR";
                if begin_loc.line <> end_loc.line then
                  Printf.bprintf ctxt.b "\n          ";
              end;
              if i = end_loc.line then
                let len = String.length line in
                (* This code won't work with tabulations, because
                   the end_loc.char is wrong in such a case *)
                let after_macro =
                  String.sub line (end_loc.char+1) (len-end_loc.char-1) in
                Printf.bprintf ctxt.b "%s\n" after_macro ;
                output lines statements
              else
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
          | COPY { end_loc ; filename ; contents } ->
            Printf.bprintf ctxt.b "      *> INLINED: %s\n" line;
            if i = end_loc.line then begin
              let copylines = EzString.split contents '\n' in
              let copylines = List.mapi (fun i line ->
                  (filename, i+1, line)) copylines in
              let lines = copylines @ lines in
              output lines statements
              end else
                output_statement lines begin_loc
                  stmt statements;
  in
  output lines sql_statements;
  Buffer.contents b
