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

open Ez_file.V1
open M4Types

let macro_error macro fmt =
  Printf.kprintf (fun s ->
      Autofonce_misc.Misc.error "Error %s at %s, in macro %s" s
        (M4Printer.string_of_location macro.loc)
        (M4Printer.string_of_macro macro)
    ) fmt

let error loc fmt =
  Printf.kprintf (fun s -> raise ( Error (s, loc))) fmt

let unescape ?last s =
  M4Lexer.unescape ?last ( Lexing.from_string s )

let to_string arg = unescape ~last:true arg.arg

let new_arg arg_loc arg = { arg ; arg_loc }

let parse_string ?loc content =
  let lexbuf = Lexing.from_string content in
  M4Lexer.init ?loc lexbuf;

  let rec iter macros =
    expect_ident macros ( M4Lexer.token lexbuf )

  and expect_ident macros ( new_loc, token ) =
    match token with
    | EOF -> ( new_loc, token ), List.rev macros
    | IDENT ident ->
        expect_first_arg macros (new_loc,ident) ( M4Lexer.token lexbuf )
    | SHELL shell ->
        expect_ident ( { kind = Shell shell ; loc = new_loc } :: macros )
          ( M4Lexer.token lexbuf )
    | COMMENT comment ->
        expect_ident ( { kind = Comment comment ; loc = new_loc } :: macros )
          ( M4Lexer.token lexbuf )
    | _ -> error new_loc "Unexpected token %S in expect_ident"
             (M4Printer.string_of_token token)

  and expect_first_arg macros (loc,ident) ( new_loc, token ) =
    match token with
    | EOF ->
        ( new_loc, token ),
        List.rev ( { kind = Macro (ident,[]) ; loc } :: macros )
    | IDENT new_ident ->
        expect_first_arg ( { kind = Macro (ident,[]) ; loc } :: macros )
          (new_loc,new_ident) ( M4Lexer.token lexbuf )
    | SHELL shell ->
        expect_ident ( { kind = Shell shell ; loc = new_loc } ::
                       { kind = Macro (ident,[]) ; loc } :: macros )
          ( M4Lexer.token lexbuf )
    | COMMENT comment ->
        expect_ident ( { kind = Comment comment ; loc = new_loc } ::
                       { kind = Macro (ident,[]) ; loc } :: macros )
          ( M4Lexer.token lexbuf )
    | FIRST_ARG arg ->
        let arg = new_arg new_loc arg in
        expect_next_arg macros (loc,ident) [arg] ( M4Lexer.token lexbuf )
    | ONE_ARG arg ->
        let arg = new_arg new_loc arg in
        expect_ident ( { kind = Macro (ident, [arg]) ; loc } :: macros )
          ( M4Lexer.token lexbuf )
    | _ -> error new_loc "Unexpected token %S in expect_first_arg"
             (M4Printer.string_of_token token)

  and expect_next_arg macros (loc,ident) args ( new_loc, token ) =
    match token with
    | LAST_ARG arg ->
        let arg = new_arg new_loc arg in
        expect_ident (
          { kind = Macro (ident,List.rev (arg::args)) ; loc } ::macros )
          ( M4Lexer.token lexbuf )
    | NEXT_ARG arg ->
        let arg = new_arg new_loc arg in
        expect_next_arg macros (loc,ident) (arg::args) ( M4Lexer.token lexbuf )
    | _ -> error new_loc "Unexpected token %S in expect_next_arg"
             (M4Printer.string_of_token token)
  in
  match iter [] with
  | exception ( Failure s ) ->
      let new_loc = M4Lexer.location lexbuf in
      raise ( M4Types.Error (s, new_loc) )
  | ( _, EOF), macros -> macros
  | ( loc, token ), _ ->
      error loc "Unexpected token %S"
        (M4Printer.string_of_token token)

let parse_file filename =
  let content = EzFile.read_file filename in
  let loc = {
    file = filename ;
    line = 1;
    char = 0;
  } in
  parse_string ~loc content
