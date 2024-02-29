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

{

open Types

let initialized = ref false
let keyword2string = Hashtbl.create 113
let string2keyword = Hashtbl.create 113

let init () =
  if not !initialized then
    List.iter (fun (s,k) ->
        Hashtbl.add keyword2string k s ;
        Hashtbl.add string2keyword s k
      ) Types.keywords

let ident ?(directive=false) s =
  match Hashtbl.find string2keyword s with
  | token -> token
  | exception Not_found ->
    if directive then
      DIRECTIVE s
    else
      IDENT s

let directive s = (* >> DIRECTIVE ... *)
  let len = String.length s in
  let i0 = 2 in (* after >> *)
  let rec iter s i len =
    if i < len && s.[i] = ' ' then
      iter s (i+1) len
    else
      i
  in
  let i1 = iter s i0 len in

  let rec iter s i len =
    if i < len && s.[i] <> ' ' then
      iter s (i+1) len
    else
      i
  in
  let i2 = iter s i1 len in
  String.sub s i1 (i2-i1)

}

let space = [ ' ' '\t' ]
let spaces = space+
let identchar = [ 'a'-'z' 'A'-'Z' '0'-'9' '-' '_' ]  (* + extended characters *)
let firstidentchar = [ 'a'-'z' 'A'-'Z' '0'-'9' ]
let lastidentchar = firstidentchar
let ident = (firstidentchar (identchar* lastidentchar)?)

(* let ident = [ 'A'-'Z' ] [ 'A'-'Z' '0'-'9' '-' '_' '#' ]* [ '\'' '"' ]* *)

let directive = ">>" space* [ 'A'-'Z' ] [ 'A'-'Z' '0'-'9' '-' ]* _*
let string = '"' [ ^ '"' ]* '"'
let string2 = '\'' [ ^ '\'' ]* '\''
let integer = [ '0'-'9' ]+
let signed_integer = [ '-' '+' '%' (* octal *) ] [ '0'-'9' ]+
let comment = ( "*>" | '|' ) _*
let float = [ '+' '-' ]? ['0'-'9']* ('.' | ',') [ '0'-'9' ]+
let unit = "B" | "BX" | "X" | "Z" | "N" | "H"

let information =
  ( "AUTHOR" | "INSTALLATION"
  | "DATE-WRITTEN" | "DATE-MODIFIED"
  | "DATE-COMPILED" | "REMARKS" | "SECURITY"
  ) space* '.'
     [ ^ '.' ]+

let unit_number = unit ( string | string2 )
            (* We could have a per-division lexer ? *)

rule line = parse
  | spaces { line lexbuf }
  | eof { EOF }

  | string { CHARS ( Lexing.lexeme lexbuf ) }
  | string2 { CHARS ( Lexing.lexeme lexbuf ) }
  | integer { INTEGER ( Lexing.lexeme lexbuf ) }
  | signed_integer { NUMBER ( Lexing.lexeme lexbuf ) }
  | unit_number { NUMBER ( Lexing.lexeme lexbuf ) }
  | float { NUMBER ( Lexing.lexeme lexbuf ) }
  | comment { COMMENT ( Lexing.lexeme lexbuf ) }
  | information { INFORMATION ( Lexing.lexeme lexbuf ) }
  | directive {
      let s = Lexing.lexeme lexbuf in
      DIRECTIVE (directive s)
    }
  | ident { ident ( Lexing.lexeme lexbuf ) }

  | '.' { DOT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "==" { EQUALEQUAL }
  | ',' { COMMA }
  | ';' { SEMI }
  | ':' { COLON }
  | '=' { EQUAL }
  | '-' { MINUS }
  | '+' { PLUS }
  | '>' { GT }
  | '<' { LT }
  | ">=" { GTE }
  | "<=" { LTE }
  | "/" { DIV }
  | "*" { MUL }
  | '$' { DOLLAR }
  | '&' { AMPER }
  | '#' { SHARP }

  | '\'' [ ^ '\'' ]*   { LEXING_ERROR ( Lexing.lexeme lexbuf ) }
  | '"' [ ^ '"' ]*   { LEXING_ERROR ( Lexing.lexeme lexbuf ) }
  | _   { LEXING_ERROR ( Lexing.lexeme lexbuf ) }
