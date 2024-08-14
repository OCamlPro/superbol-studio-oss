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

 and extract_cob_var_name str = 
    let len = String.length str in
    let rec aux i acc =
      if i >= len then List.rev acc
      else if str.[i] = ':' then
        let start = i + 1 in
        let rec find_end j =
          if j >= len || str.[j] = ' ' || str.[j] = ',' || str.[j] = ')' || str.[j] = '(' then j
          else find_end (j + 1)
        in
        let end_pos = find_end start in
        let var_name = String.sub str start (end_pos - start) in
        if List.mem var_name acc then aux end_pos acc
        else aux end_pos (var_name :: acc)
      else aux (i + 1) acc
    in
    aux 0 []

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
  let tbl = Hashtbl.create 10 in

  while !i < len do
    if str.[!i] = ':' then (
      let start = !i + 1 in
      incr i;
      while
        !i < len
        && str.[!i] <> ' '
        && str.[!i] <> ','
        && str.[!i] <> ')'
        && str.[!i] <> '('
      do
        incr i
      done;
      let var_name = String.sub str start (!i - start) in
      if Hashtbl.mem tbl var_name then
        Buffer.add_string buffer
          ("$" ^ string_of_int (Hashtbl.find tbl var_name))
      else (
        incr count;
        Hashtbl.add tbl var_name !count;
        Buffer.add_string buffer ("$" ^ string_of_int !count)
      )
    ) else (
      Buffer.add_char buffer str.[!i];
      incr i
    )
  done;

  Buffer.contents buffer
