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

(* Paging *)

type free = |
type fixed = |
type _ paging =
  | FreePaging: free paging
  | FixedWidth: fixed_paging_params -> fixed paging
and fixed_paging_params =
  {
    cut_at_col: int;
    alphanum_padding: char option;
  }

(* Actual format and indicator positioning *)

type 'k source_format = 'k indicator_position * 'k paging
and _ indicator_position =
  |    NoIndic:  free indicator_position
  | FixedIndic: fixed indicator_position
  | XOpenIndic: fixed indicator_position
  |   CRTIndic: fixed indicator_position
  |   TrmIndic: fixed indicator_position
  |  CBLXIndic: fixed indicator_position
and any_source_format =
  | SF: 'k source_format -> any_source_format                          [@@unboxed]

type comment_entry_termination =                  (* skip until... *)
  | Newline                                       (* ... newline *)
  | Period                                        (* ... next period (unused) *)
  | AreaB of { first_area_b_column: int }         (* ... next word in area A *)

val select_source_format: Cobol_config.source_format -> any_source_format
val source_format_spec: 'k source_format -> Cobol_config.source_format
val same_source_formats: 'k source_format -> 'r source_format -> bool
val comment_entry_termination: 'k source_format -> comment_entry_termination
val decypher_source_format
  : dialect:Cobol_config.dialect
  -> string
  -> (Cobol_config.source_format, [> `SFUnknown of string ]) result

type 'k state

val init_state: 'k source_format -> 'k state
val diagnostics: _ state -> Cobol_common.Diagnostics.Set.t
val comments: _ state -> Text.comments
val newline_cnums: _ state -> int list
val source_format: 'k state -> 'k source_format
val change_source_format: 'k state -> 'c source_format Cobol_common.Srcloc.with_loc
  -> ('c state, 'k state) result
val allow_debug: 'a state -> bool
val flush: 'a state -> 'a state * Text.text
val flush_continued: ?force:bool -> 'a state -> 'a state
val eof: 'a state -> Lexing.lexbuf -> 'a state
val new_line: 'a state -> Lexing.lexbuf -> 'a state * Text.text

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

val eqeq :
  fixed state ->
  ktkd:(fixed state -> Lexing.lexbuf -> 'a) ->
  knom:(fixed state -> Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'a
val eqeq' :
  k:('a state -> Lexing.lexbuf -> 'b) ->
  'a state -> Lexing.lexbuf -> 'b

val cdir_word :
  fixed state ->
  ktkd:(fixed state -> Lexing.lexbuf -> 'a) ->
  knom:(fixed state -> Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'a
val cdir_word' :
  k:('a state -> Lexing.lexbuf -> 'b) ->
  'a state -> Lexing.lexbuf -> 'b
val text_word :
  ?cont:bool ->
  ktkd:(fixed state -> Lexing.lexbuf -> 'a) ->
  knom:(fixed state -> Lexing.lexbuf -> 'a) ->
  fixed state -> Lexing.lexbuf -> 'a
val text_word' :
  k:('a state -> Lexing.lexbuf -> 'b) ->
  'a state -> Lexing.lexbuf -> 'b
val alphanum_lit :
  ?doubled_opener:bool ->
  ktkd:(fixed state -> Lexing.lexbuf -> 'a) ->
  knom:(fixed state -> Lexing.lexbuf -> 'a) ->
  fixed state -> Lexing.lexbuf -> 'a
val alphanum_lit' :
  k:('a state -> Lexing.lexbuf -> 'b) ->
  'a state -> Lexing.lexbuf -> 'b

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
