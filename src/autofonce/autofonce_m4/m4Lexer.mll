(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.0, as described in the LICENSE.md file in the root  *)
(*  directory of this source tree.                                        *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

{
  (* open Lexing *)
  open M4Types

let debug_lexer = match Sys.getenv "DEBUG_LEXER" with
  | exception Not_found -> false
  | _ -> true

(* while reading an argument, whether we should return a token for
   a first argument or a next argument *)
let first_arg = ref true

let quoted_buffer = Buffer.create 1000
let opened_quotes = ref 0
let opened_parens = ref 0
let translate_quadrigraphs = ref false

let location lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let file = pos.Lexing.pos_fname in
  let line = pos.Lexing.pos_lnum in
  let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  { file ; line ; char }

let saved_loc = ref ( None : location option )

let with_saved_loc token =
  match !saved_loc with
  | None -> assert false
  | Some loc -> loc, token
let save_loc lexbuf =
  saved_loc := Some ( location lexbuf )


let in_macro = ref 0
let init ?loc lexbuf =
  in_macro := 0 ;
  opened_quotes := 0 ;
  translate_quadrigraphs := false ;
  match loc with
  | None -> ()
  | Some loc ->
      let open Lexing in
      let pos = { lexbuf.lex_curr_p with
                  pos_fname = loc.file ;
                  pos_lnum = loc.line ;
                  pos_cnum = loc.char ;
                }
      in
      lexbuf.lex_curr_p <- pos ;
      lexbuf.lex_start_p <- pos ;
      ()

let init_token lexbuf =
  opened_quotes := 0 ;
  opened_parens := 0 ;
  translate_quadrigraphs := false ;
  Buffer.clear quoted_buffer ;
  save_loc lexbuf

let count_newlines lexbuf =
  let s = Lexing.lexeme lexbuf in
  for i = 0 to String.length s - 1 do
    if s.[i] = '\n' then Lexing.new_line lexbuf
  done

let debug lexbuf =
  if debug_lexer then begin
    Printf.eprintf "  lexeme=%S quotes=%d parens=%d\n%!"
      (Lexing.lexeme lexbuf) !opened_quotes !opened_parens
  end

let quadrigraph lexbuf s =
  if not !translate_quadrigraphs then
    Buffer.add_string quoted_buffer ( Lexing.lexeme lexbuf)
  else
    Buffer.add_string quoted_buffer s


}

let upper =[ 'A'-'Z' ]
let lower =[ 'a'-'z' ]
let alpha =['a'-'z' 'A'-'Z' ]
let alphanum = [ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]
let space = [ '\r' '\t' ' ' ]

rule shell = parse
  | eof { location lexbuf, EOF }
  | '#' (( ([ ^ '\n' ]* "\n#")* [ ^ '\n' ]* ) as comment) {
      let loc = location lexbuf in
      String.iter (function '\n' -> Lexing.new_line lexbuf | _ -> ()) comment;
      loc, COMMENT comment }
  | space+ { shell lexbuf }
  | '\n' { Lexing.new_line lexbuf; shell lexbuf }
  | ( alpha alphanum* as ident ) {
      let loc = location lexbuf in
      let token =
        match ident.[0] with
        | 'a'..'z' when ident <> "m4_include" ->
            SHELL (ident ^ end_of_line lexbuf)
        | _ ->
            IDENT ( Lexing.lexeme lexbuf )
      in
      ( loc, token )
    }
  | '(' [' ' '\r' '\t' '\n' ]* {
      incr in_macro;
      first_arg := true ;
      init_token lexbuf;
      count_newlines lexbuf ;
      arg lexbuf }
  | _
      { Printf.kprintf failwith "Unexpected char %S" (Lexing.lexeme lexbuf) }

and end_of_line = parse
  | [ ^ '\n' '#' ]* as line { line }
  | '\n' { Lexing.new_line lexbuf; "" }
  | '#' [ ^ '\n' ]* { "" }
  | eof { "" }

and comment = parse
  | '\n' { Lexing.new_line lexbuf }
  | eof {}
  | _ { comment lexbuf }

and arg = parse
  | '#' {
      debug lexbuf ;
      if !opened_quotes > 0 then
        Buffer.add_char quoted_buffer '#'
      else
        comment lexbuf ;
      arg lexbuf
    }
  | eof { failwith "Unexpected end of file in macro" }
  | ' '* ('\r'?) '\n' {
      debug lexbuf ;
      Lexing.new_line lexbuf;
      Buffer.add_char quoted_buffer '\n';
      arg lexbuf }
  | '[' {
      debug lexbuf ;
      if !opened_quotes > 0 then Buffer.add_char quoted_buffer '[';
      incr opened_quotes;
      arg lexbuf
    }
  | ']' {
      debug lexbuf ;
      if !opened_quotes = 0 then failwith "unbalanced ]";
      decr opened_quotes;
      if !opened_quotes > 0 then Buffer.add_char quoted_buffer ']';
      arg lexbuf
    }
  | '(' {
      debug lexbuf ;
      if !opened_quotes == 0 then incr opened_parens ;
      Buffer.add_char quoted_buffer '(';
      arg lexbuf
    }
  | ')' {
      debug lexbuf ;
      if !opened_quotes > 0 then begin
        Buffer.add_char quoted_buffer ')';
        arg lexbuf
      end else
      if !opened_parens > 0 then begin
        decr opened_parens ;
        Buffer.add_char quoted_buffer ')';
        arg lexbuf
      end else
        begin
        decr in_macro;
        let s = Buffer.contents quoted_buffer in
        let token = if !first_arg then
            ONE_ARG s
          else
            LAST_ARG s
        in
        with_saved_loc token
      end
    }
  | ',' [ ' ' '\r' '\n' '\t' ]* {
      debug lexbuf ;
      count_newlines lexbuf ;
      if !opened_quotes > 0 || !opened_parens > 0 then begin
        Buffer.add_string quoted_buffer ( Lexing.lexeme lexbuf );
        arg lexbuf
      end else begin
        let s = Buffer.contents quoted_buffer in
        let token = if !first_arg then
            FIRST_ARG s
          else
            NEXT_ARG s
        in
        first_arg := false;
        with_saved_loc token
      end
    }
  | _ {
      debug lexbuf ;
      Buffer.add_string quoted_buffer ( Lexing.lexeme lexbuf );
      arg lexbuf
    }

and unescape = parse
  | eof { () }
  | "@<:@" { quadrigraph lexbuf "["; unescape lexbuf }
  | "@:>@" { quadrigraph lexbuf "]"; unescape lexbuf }
  | "@S|@" { quadrigraph lexbuf "$"; unescape lexbuf }
  | "@%:@" { quadrigraph lexbuf "#"; unescape lexbuf }
  | "@&t@" { quadrigraph lexbuf ""; unescape lexbuf }
  | "@{:@" { quadrigraph lexbuf "("; unescape lexbuf }
  | "@:}@" { quadrigraph lexbuf ")"; unescape lexbuf }
  | '\n' { Lexing.new_line lexbuf;
           Buffer.add_char quoted_buffer '\n';
           unescape lexbuf }
  | '[' {
      if !opened_quotes > 0 then Buffer.add_char quoted_buffer '[';
      incr opened_quotes;
      unescape lexbuf
    }
  | ']' {
      if !opened_quotes = 0 then failwith "unbalanced ]";
      decr opened_quotes;
      if !opened_quotes > 0 then Buffer.add_char quoted_buffer ']';
      unescape lexbuf
    }
  | _ { Buffer.add_string quoted_buffer ( Lexing.lexeme lexbuf );
        unescape lexbuf;
      }

      {
        let token lexbuf =
          let (loc, token) =
            if !in_macro > 0 then begin
              init_token lexbuf ;
              arg lexbuf
            end else
              shell lexbuf
          in
          if debug_lexer then
            Printf.eprintf "token [%s] at %s\n%!"
              (M4Printer.string_of_token token)
              (M4Printer.string_of_location loc);
          loc, token

let unescape ?(last=false) lexbuf =
  Buffer.clear quoted_buffer ;
  opened_quotes := 0 ;
  translate_quadrigraphs := last ;
  unescape lexbuf ;
  Buffer.contents quoted_buffer
}
