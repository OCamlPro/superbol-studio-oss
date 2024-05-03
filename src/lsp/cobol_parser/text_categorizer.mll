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
type output =
  | Digits of string
  | Numeric of string * (char * string * string option) option
  | Word of string
  | Punctuation of string
  | End
  | Unexpected of char

type pic_output =
  | PIC_string of string
  | PIC_is
  | PIC_end
  | PIC_unexpected of char

type alphanum_suffix = STR | EBCDIC
type alphanum_content =
  | AStr of string * alphanum_suffix
  | AEBCDIC of int
  | AEnd of { wellformed: bool }
  | AUnexpected of char * alphanum_suffix
}

let blank = [' ' '\009' '\r' ]
let blanks = (blank+ | '\t')
let digit = [ '0'-'9' ]
let sign = [ '+' '-' ]
let opers = sign | ['*' '/' '>' '<' '=' '&'] | "**" | "::" | ">=" | "<=" | "<>"
let punct = opers | ['.' ':' '(' ')']

let integer = (sign? digit+)
let exponent = (sign? digit+)

let identchar = [ 'a'-'z' 'A'-'Z' '0'-'9' '-' '_' ]  (* + extended characters *)
let firstidentchar = [ 'a'-'z' 'A'-'Z' '0'-'9' ]
let lastidentchar = firstidentchar
let ident = (firstidentchar (identchar* lastidentchar)?)

let picchar   = _ # [' ' '\t' '\009' '\n' '\r' '\'' '"' ';']
let picstring = (picchar # [',']) (picchar*)

(* Text-word tokenizer (after text manipulation phase) *)
rule token = parse

  | blanks
      { token lexbuf }

  | (digit+ as s)
      { Digits s }

  | (sign digit+ as n)
      { Numeric (n, None) }

  | (sign? digit* as n) (['.' ','] as sep) (digit+ as d)
      { Numeric (n, Some (sep, d, None)) }

  | (sign? digit* as n) (['.' ','] as sep) (digit+ as d) ['E' 'e']
      (exponent as e)
      { Numeric (n, Some (sep, d, Some e)) }

  | ident as s                                           (* 31 characters max *)
      { Word s }

  | (opers | punct) as s
      { Punctuation s }

  | eof
      { End }

  | (_ as c)
      { Unexpected c }

and pic_token = parse

  | blanks
      { pic_token lexbuf }

  | ['i' 'I'] ['s' 'S']
      { PIC_is }

  | (picstring as s)
      { PIC_string s }

  | eof
      { PIC_end }

  | (_ as c)
      { PIC_unexpected c }

(* TODO: distinguish lexing entry based on quotation *)
and alphanum_string = parse

  | ['"' '''] (digit+ as num)
      { AEBCDIC (int_of_string num) }

  | ((['"' '''] (_ # digit)? | [^ '"' '''])* as str)
      { AStr (str, STR) }

  | eof
      { AEnd { wellformed = true } }

and symbolic_ebcdic = parse

  | ','? ' '* (digit+ as num)
      { AEBCDIC (int_of_string num) }

  | ' '* ['"' ''']
      { alphanum_string lexbuf }

  | (_ as c)
      { AUnexpected (c, EBCDIC) }

  | eof
      { AEnd { wellformed = false } }

(* --- *)
