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

type 'k state

val init_state: 'k Src_format.source_format -> 'k state
val diagnostics: _ state -> Cobol_common.Diagnostics.Set.t
val rev_comments: _ state -> Text.comments
val rev_ignored: _ state -> Cobol_common.Srcloc.lexloc list
val rev_newline_cnums: _ state -> int list
val source_format: 'k state -> 'k Src_format.source_format
val change_source_format
  : 'k state -> 'c Src_format.source_format -> ('c state, unit) result
val allow_debug: 'a state -> bool
val flush: 'a state -> 'a state * Text.text
val flush_continued: ?force:bool -> 'a state -> 'a state
val eof: 'a state -> Lexing.lexbuf -> 'a state
val new_line: 'a state -> Lexing.lexbuf -> 'a state * Text.text
val skip: 'a state -> Lexing.lexbuf -> 'a state

val comment
  : ?marker:string
  -> ?floating:bool
  -> 'a state
  -> Lexing.lexbuf
  -> 'a state * Text.text

type alphanumeric_continuation =
  | Nominal
  | Closed of Text.quotation
  | UnclosedEBCDICs of Text.quotation
val continue_quoted_alphanum
  : 'a state
  -> alphanumeric_continuation

val eqeq
  : (Src_format.fixed state as 's)
  -> ktkd:('s -> Lexing.lexbuf -> 'a)
  -> knom:('s -> Lexing.lexbuf -> 'a)
  -> Lexing.lexbuf -> 'a
val eqeq'
  : k:('a state -> Lexing.lexbuf -> 'b)
  -> 'a state -> Lexing.lexbuf -> 'b

val sna
  : (Src_format.fixed state as 's)
  -> Lexing.lexbuf -> 's
val cdir_word
  : ?marker:string
  -> (Src_format.fixed state as 's)
  -> ktkd:('s -> Lexing.lexbuf -> 'a)
  -> knom:('s -> Lexing.lexbuf -> 'a)
  -> Lexing.lexbuf -> 'a
val cdir_word'
  : k:('a state -> Lexing.lexbuf -> 'b)
  -> 'a state -> Lexing.lexbuf -> 'b
val text_word
  : ?cont:bool
  -> ktkd:('s -> Lexing.lexbuf -> 'a)
  -> knom:('s -> Lexing.lexbuf -> 'a)
  -> (Src_format.fixed state as 's) -> Lexing.lexbuf -> 'a
val text_word'
  : k:('a state -> Lexing.lexbuf -> 'b)
  -> 'a state -> Lexing.lexbuf -> 'b
val alphanum_lit
  : ?doubled_opener:bool
  -> ktkd:('s -> Lexing.lexbuf -> 'a)
  -> knom:('s -> Lexing.lexbuf -> 'a)
  -> (Src_format.fixed state as 's) -> Lexing.lexbuf -> 'a
val alphanum_lit'
  : k:('a state -> Lexing.lexbuf -> 'b)
  -> 'a state -> Lexing.lexbuf -> 'b

(* --- *)

val lex_diag
  : severity:Cobol_common.Diagnostics.severity
  -> 'a state
  -> ?loc:Cobol_common.Srcloc.srcloc
  -> (_, 'a state) Pretty.func

type _ c = Char: char c | Str: string c | Integer: int c
val unexpected
  : 'a c
  -> ?knd:Pretty.simple
  -> ?c:'a
  -> ?severity:Cobol_common.Diagnostics.severity
  -> k:('k state -> Lexing.lexbuf -> 'b)
  -> 'k state -> Lexing.lexbuf -> 'b
