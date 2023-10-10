(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Types
open Internal_types
open Lexing

let init () = ()

let loc_of_pos (begin_pos, end_pos) =
  {
    file = begin_pos.pos_fname ;
    line_begin = begin_pos.pos_lnum ;
    char_begin = begin_pos.pos_cnum - begin_pos.pos_bol ;
    line_end = end_pos.pos_lnum ;
    char_end = end_pos.pos_cnum - end_pos.pos_bol ;
  }

let loc_of_lexbuf lexbuf =
  loc_of_pos ( lexbuf.lex_start_p, lexbuf.lex_curr_p )

let error_lexbuf lexbuf n err =
  Internal_misc.error ~loc:( loc_of_lexbuf lexbuf ) n err

let loc pos txt = { loc = loc_of_pos pos  ; txt }

let expand_loc l1 l2 =
  if l2.line_begin < l1.line_begin then begin
    l1.line_begin <- l2.line_begin ;
    l1.char_begin <- l2.char_begin;
  end else
  if l2.line_begin = l1.line_begin && l2.char_begin < l1.char_begin then
    l1.char_begin <- l2.char_begin ;
  if l2.line_end > l1.line_end then begin
    l1.line_end <- l2.line_end ;
    l1.char_end <- l2.char_end;
  end else
  if l2.line_end = l1.line_end && l2.char_end > l1.char_end then
    l1.char_end <- l2.char_end ;
  ()

let merge_locs l1 l2 =
  let l1 = { l1 with line_begin = l1.line_begin } in
  expand_loc l1 l2;
  l1

(* The lexer calls this function everytime it finds a newline, to update
   the line counter of the location *)
let update_loc lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

let line pos operation =
  let line_operation_loc = loc_of_pos pos in
  let line =
    {
      line_comments_before = [] ;
      line_comment_after = None ;
      line_operation = operation ;
      line_global_loc = line_operation_loc;
      line_operation_loc ;
    }
  in
  line
