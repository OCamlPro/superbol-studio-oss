(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Types

let string_of_token token =
  match token with
  | EOF -> "EOF"

  | DOT -> "DOT"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | EQUALEQUAL -> "EQUALEQUAL"
  | COMMA -> "COMMA"
  | SEMI -> "SEMI"
  | COLON -> "COLON"
  | EQUAL -> "EQUAL"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | GT -> "GT"
  | LT -> "LT"
  | GTE -> "GTE"
  | LTE -> "LTE"
  | DIV -> "DIV"
  | MUL -> "MUL"
  | DOLLAR -> "DOLLAR"
  | AMPER -> "AMPER"
  | SHARP -> "SHARP"

  | IDENT s -> Printf.sprintf "IDENT %S" s
  | INTEGER s -> Printf.sprintf "INTEGER %s" s
  | NUMBER s -> Printf.sprintf "NUMBER %s" s
  | CHARS s -> Printf.sprintf "STRING {|%s|}" s
  | COMMENT s -> Printf.sprintf "COMMENT {|%s|}" s
  | DIRECTIVE s -> Printf.sprintf "DIRECTIVE %S" s
  | INFORMATION s -> Printf.sprintf "INFORMATION %S" s
  | LEXING_ERROR s -> Printf.sprintf "LEXING_ERROR %S" s
  | PERFORM_PAR -> "PERFORM"

  | token ->
    match Hashtbl.find Lexer.keyword2string token with
    | exception Not_found -> assert false
    | s -> s

let oc = ref None
let () =
  match Sys.getenv "SUPERBOL_INDENT_DEBUG" with
  | exception Not_found -> ()
  | file ->
    oc := Some ( open_out file )

let log : ( 'a, unit, string, unit) format4 -> 'a =
  let f fmt =
    Printf.kprintf (fun s ->
        match !oc with
        | None -> ()
        | Some oc ->
          output_string oc s;
          flush oc
      ) fmt
  in
  f
