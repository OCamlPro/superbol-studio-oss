(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

{
  open Mi2Parser

  let buf = Buffer.create 1024

  exception Lexing_error of string

  let error lexbuf msg =
    let pos = Lexing.lexeme_start_p lexbuf in
    let line = pos.Lexing.pos_lnum in
    let col  = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    raise (Lexing_error (
        Printf.sprintf "Lexer error at %d:%d: %s" line col msg))

  let parse_octal digits =
    try Char.chr (int_of_string ("0o" ^ digits) land 0xFF)
    with _ -> Char.chr 0

  let parse_hex digits =
    try Char.chr (int_of_string ("0x" ^ digits) land 0xFF)
    with _ -> Char.chr 0
}

let digit   = ['0'-'9']
let alpha   = ['a'-'z' 'A'-'Z' '_']
let ident_c = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']

let odigit  = ['0'-'8']
let hdigit  = ['0'-'9' 'a'-'f' 'A'-'F']

let hexa    = hdigit+
let octal   = odigit odigit? odigit?

rule token = parse
  | [' ' '\t' '\r']      { token lexbuf }   (* skip whitespace *)
  | '\n'                 { EOL }
  | '^'                  { CARET }
  | '*'                  { STAR }
  | '+'                  { PLUS }
  | '='                  { EQUALS }
  | '~'                  { TILDE }
  | '@'                  { AT }
  | '&'                  { AMP }
  | ','                  { COMMA }
  | '{'                  { LBRACE }
  | '}'                  { RBRACE }
  | '['                  { LBRACKET }
  | ']'                  { RBRACKET }
  | digit+ as i          { INT (int_of_string i) }
  | alpha ident_c* as s  { IDENT s }
  | '"'                  { Buffer.clear buf; STRING (string lexbuf) }
  | eof                  { EOF }
  | _ as c               { error lexbuf (Printf.sprintf "Unexpected character '%c'" c) }

and string = parse
  | '"'                  { Buffer.contents buf }
  | '\\' '"'             { Buffer.add_char buf '"'; string lexbuf }
  | '\\' '''             { Buffer.add_char buf '\''; string lexbuf }
  | '\\' '\\'            { Buffer.add_char buf '\\'; string lexbuf }
  | '\\' 'n'             { Buffer.add_char buf '\n'; string lexbuf }
  | '\\' 't'             { Buffer.add_char buf '\t'; string lexbuf }
  | '\\' 'r'             { Buffer.add_char buf '\r'; string lexbuf }
  | '\\' 'b'             { Buffer.add_char buf '\b'; string lexbuf }
  | '\\' 'f'             { Buffer.add_char buf '\012'; string lexbuf }
  | '\\' 'a'             { Buffer.add_char buf '\007'; string lexbuf }
  | '\\' 'v'             { Buffer.add_char buf '\011'; string lexbuf }
  | '\\' 'x' (hexa as s) { Buffer.add_char buf (parse_hex s); string lexbuf }
  | '\\' 'x'             { string lexbuf }
  | '\\' (octal as s)    { Buffer.add_char buf (parse_octal s); string lexbuf }
  | '\\' (_ as c)        { Buffer.add_char buf c; string lexbuf }
  | '\n'                 { error lexbuf "Unterminated string literal" }
  | eof                  { error lexbuf "Unterminated string literal" }
  | _ as c               { Buffer.add_char buf c; string lexbuf }
