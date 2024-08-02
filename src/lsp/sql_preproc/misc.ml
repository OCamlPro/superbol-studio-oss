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
  { filename ;
    line = e.tok_edit.edit.lnum ;
    char = e.tok_indent +  e.tok_edit.edit.offset_orig ;
  }

let error ?loc fmt =
  Printf.kprintf (fun s ->
      Printf.eprintf "Error";
      begin match loc with
        | None -> ()
        | Some loc ->
            Printf.eprintf " at %s:%d" loc.filename loc.line
      end;
      Printf.eprintf ": %s\n%!" s;
      Printf.eprintf "Aborting.\n%!";
      exit 2
    ) fmt

let warning ?loc fmt =
  Printf.kprintf (fun s ->
      Printf.eprintf "Warning";
      begin match loc with
        | None -> ()
        | Some loc ->
            Printf.eprintf " at %s:%d" loc.filename loc.line
      end;
      Printf.eprintf ": %s\n%!" s;
    ) fmt


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

  | tok ->
      try
        Hashtbl.find Cobol_indent.Lexer.keyword2string tok
      with Not_found ->
        failwith ( Cobol_indent.Misc.string_of_token tok )

let add_dot ~with_dot b =
  if with_dot then
    Printf.bprintf b "            .\n"

let resolve_copy ~config file =

  let rec iter_exts exts =
    match exts with
    | [] ->
        raise Not_found
    | ext :: exts ->
        let file = String.lowercase_ascii (file ^ ext) in
        match iter_paths file (Lazy.force config.copy_path) with
        | exception Not_found -> iter_exts exts
        | filename -> filename

  and iter_paths file path =
    match path with
    | [] -> raise Not_found
    | (dir, map) :: path ->
        match StringMap.find file map with
        | exception Not_found -> iter_paths file path
        | file -> Filename.concat dir file
  in
  iter_exts config.copy_exts


  let extract_filename path =
    let parts = Str.split (Str.regexp "/") path in
    let filename_with_ext = List.hd (List.rev parts) in
    let filename_parts = Str.split (Str.regexp "\\.") filename_with_ext in
    List.hd filename_parts