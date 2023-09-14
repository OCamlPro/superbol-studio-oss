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
  open Lexing
  open Conf_parser
  exception LexError of string * Lexing.position * Lexing.position
}

let newline = '\r'* '\n'
let spaces = [' ' '\t']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let punct = (newline | spaces)
let true = "true"
let false = "false"
let yes = "yes"
let no = "no"
let boolean = true | false | yes | no
let integer = ('+' | '-')? digit+
let ident_chars = letter | '-' | digit
let ident = letter ident_chars*
let value_char = letter | digit
let value = value_char (value_char | '-' | '_')*

let reserved_words = "reserved-words"
let reserved = "reserved"
let not_reserved = "not-reserved"
let intrinsic_function = "intrinsic-function"
let not_intrinsic_function = "not-intrinsic-function"
let system_name = "system-name"
let not_system_name = "not-system-name"
let register = "register"
let not_register = "not-register"
let include = "include"

rule main = parse
| newline                { Lexing.new_line lexbuf; main lexbuf  }
| spaces                 { main lexbuf  }
| ":"                    { COLON }
| "#"                    { single_line_comment lexbuf }
| '"'                    { read_string (Buffer.create 17) lexbuf}
| "*"                    { STAR }
| "="                    { EQ }
| "+"                    { PLUS }
| reserved_words         { RESERVED_WORDS }
| reserved               { RESERVED }
| not_reserved           { NOT_RESERVED }
| intrinsic_function     { INTRINSIC_FUNCTION }
| not_intrinsic_function { NOT_INTRINSIC_FUNCTION }
| system_name            { SYSTEM_NAME}
| not_system_name        { NOT_SYSTEM_NAME }
| register               { REGISTER }
| not_register           { NOT_REGISTER }
| "ok"                   { OK }
| "warning"              { WARNING }
| "archaic"              { ARCHAIC }
| "obsolete"             { OBSOLETE }
| "skip"                 { SKIP }
| "ignore"               { IGNORE }
| "error"                { ERROR }
| "unconformable"        { UNCONFORMABLE }
| include                { INCLUDE }
| true | yes             { BOOLEAN true }
| false | no             { BOOLEAN false }
| integer as i           { INT (int_of_string i) }
| ident as i             { IDENT i }
| value as x             { ANY x }
| _ as c                 { raise @@ LexError (Format.sprintf "Invalid char: %c" c, lexbuf.lex_start_p, lexbuf.lex_curr_p) }
| eof                    { EOF }

and single_line_comment = parse
| newline { new_line lexbuf; main lexbuf }
| eof     { EOF }
| _       { single_line_comment lexbuf }

and read_string buf = parse
| '"'           { STRING (Buffer.contents buf) }
| '\\' '\\'     { Buffer.add_string buf "\\"; read_string buf lexbuf }
| '\\' '\ '     { Buffer.add_string buf "\ "; read_string buf lexbuf }
| [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
| _ as c        { raise @@ LexError (Format.sprintf "Invalid char: %c" c, lexbuf.lex_start_p, lexbuf.lex_curr_p) }
| eof           { raise @@ LexError ("Unexpected end of string", lexbuf.lex_start_p, lexbuf.lex_curr_p) }
