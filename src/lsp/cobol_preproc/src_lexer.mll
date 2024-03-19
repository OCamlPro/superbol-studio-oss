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
  (* Three lexers:
     * `line`: for lines of source text before preprocessing
     * `cdtoken`: for simple tokens used by compiler directives
     * `pptoken`: for simple tokens used by text manipulation statements
  *)

  let pptoken_of_keyword = Hashtbl.create 15
  let keyword_of_pptoken = Hashtbl.create 15
  let __init_keywords =
    List.iter begin fun (kwd, token) ->
      Hashtbl.add keyword_of_pptoken token kwd;
      Hashtbl.add pptoken_of_keyword kwd token;
    end Preproc_keywords.keywords

  type pptoken_component =
    | PPTok of Preproc_tokens.token
    | PPEnd

  let cdtoken_of_keyword = Hashtbl.create 15
  let keyword_of_cdtoken = Hashtbl.create 15
  let __init_keywords =
    List.iter begin fun (kwd, token) ->
      Hashtbl.add keyword_of_cdtoken token kwd;
      Hashtbl.add cdtoken_of_keyword kwd token;
    end Compdir_keywords.keywords

  type cdtoken_component =
    | CDTok of Compdir_grammar.token
    | CDEnd

  let update_loc lexbuf file line absolute chars =
    let open Lexing in
    let pos = lexbuf.lex_curr_p in
    let new_file = match file with
      | None -> pos.pos_fname
      | Some s -> s
    in
    lexbuf.lex_curr_p <-
      { pos with
        pos_fname = new_file;
        pos_lnum = if absolute then line else pos.pos_lnum + line;
        pos_bol = pos.pos_cnum - chars }

}

let newline = '\r'* '\n'
let nnl = _ # ['\r' '\n']                             (* anything but newline *)
let sna = nnl nnl nnl nnl nnl nnl              (* 6 chars; TODO: exclude tabs *)
let spaces = ([' ' '\t']*)
let    blank        = [' ' '\009' '\r']
let nonblank        = nnl # blank
let    blanks       =(blank+ | '\t')
let    blank_area_A = blank blank blank blanks | '\t'
let nonblank_area_A =(nonblank nnl nnl nnl |
                      blank nonblank nnl nnl |
                      blank blank nonblank nnl |
                      blank blank blank nonblank)
let nonblank = nonblank # ['\t']    (* now, also exclude tab from blank chars *)
let separators = ([ ',' ';' ]+)
let epsilon = ""
let letter = [ 'a'-'z' 'A'-'Z' ]                      (* TODO: '\128'-'\255'? *)
let digit = [ '0'-'9' ]
let sign = [ '+' '-' ]
let opers = sign | ['*' '/' '>' '<' '=' '&'] | "**" | "::" | ">=" | "<=" | "<>"

let line_directive_prefix = '#' blanks* "line" spaces
let line_directive_filename = ("\"" ([^ '\r' '\n' '\"' ] * as name) "\"")

let symbolic_ebcdics =
  (digit (digit|','|' ')*)
let alphanum_lit_content_spl =
  ((nnl # ['\'']) | ('\'' symbolic_ebcdics '\'') | ("''"))
let alphanum_lit_content_dbl =
  ((nnl # [ '"']) | ('"' symbolic_ebcdics '"') | ('"' '"'))
let alphanum_lit_suffix_spl =
  (('\'' | "'-")? | '\'' symbolic_ebcdics)
let alphanum_lit_suffix_dbl =
  (('"' | "\"-")? | '"' symbolic_ebcdics)
let alphanum_lit =
  (* allow zero-length, even if length >= 1 byte according to spec *)
  (('\'' alphanum_lit_content_spl* alphanum_lit_suffix_spl) |
   ('"'  alphanum_lit_content_dbl* alphanum_lit_suffix_dbl))
let alphanum_lit_new =                               (* may lack G & GX still *)
  ((['B' 'b' 'X' 'x' 'Z' 'z' 'N' 'n']
   |['B' 'b' 'N' 'n'] ['X' 'x'])? alphanum_lit)
let alphanum_lit_cont_double_apostrophes =
  ("''"    alphanum_lit_content_spl* alphanum_lit_suffix_spl)
let alphanum_lit_cont_double_quotes =
  ('"' '"' alphanum_lit_content_dbl* alphanum_lit_suffix_dbl)

let alphanum_lit_cont_unclosed_ebcdics_double_apostrophes =
  ('\'' symbolic_ebcdics
     ('\'' alphanum_lit_content_spl*)? alphanum_lit_suffix_spl)
let alphanum_lit_cont_unclosed_ebcdics_double_quotes =
  ('"' symbolic_ebcdics
     ('"' alphanum_lit_content_dbl*)? alphanum_lit_suffix_dbl)

let currency_sign_char =                                    (* as per ISO/IEC *)
  nonblank # ['0'-'9'
              'A'-'E' 'N' 'P' 'R' 'S' 'V' 'X' 'Z'
              'a'-'e' 'n' 'p' 'r' 's' 'v' 'x' 'z'
                ' ' '+' '-' ',' '.' '*' '/' ';' '(' ')' '\'' '"' '=']
let text_char = nonblank # [';' '\'' '"' '=']
let text_word_prefix =
  ((text_char # ['*' '>'])  text_char       ?)  |
  (             ['*' '>']  (text_char # ['>']))
let text_word =
  (text_word_prefix (text_char # ['*' '>'] | '*' (text_char # ['>']))*) | opers

let cdir_char =
  (letter | digit | ':')                            (* colon for pseudo-words *)
let cdir_word_suffix =
  (cdir_char ((cdir_char | '_' | '-') cdir_char*)*)? (* CHECKME: allow empty? *)
let cdir_word =
  (">>" ' '? cdir_word_suffix)

(* Fixed format *)

rule fixed_line state
  = shortest
  | sna                                                            (* nominal *)
      {
        fixed_indicator (Src_lexing.sna state lexbuf) lexbuf
      }
  | '\t'
      {
        fixed_nominal_line (Src_lexing.flush_continued state) lexbuf
      }
  | (nnl* newline)                                  (* blank line (too short) *)
      {
        Src_lexing.new_line (Src_lexing.sna state lexbuf) lexbuf
      }
  | (nnl* eof)           (* blank line (too short), without newline character *)
      {
        Src_lexing.(flush @@ eof (Src_lexing.sna state lexbuf) lexbuf)
      }
and fixed_indicator state
  = parse
  | ' ' | '\t' (* second tab *)                                    (* nominal *)
      {
        fixed_nominal_line (Src_lexing.flush_continued state) lexbuf
      }
  | '-'                                                  (* continuation line *)
      {
        fixed_continue_line state lexbuf
      }
  | ('$' as marker)                                     (* compiler directive *)
      {
        fixed_mf_cdir_line (String.make 1 marker) state lexbuf
      }
  | ('>' as marker)
      {
        maybe_fixed_cdir_line marker state lexbuf
      }
  | (['*' '/'] as marker)                                     (* comment line *)
      {
        comment_line marker state lexbuf
      }
  | ['d' 'D']
      {
        fixed_debug_line state lexbuf
      }
  | (_#['\n' '\r'] as c)                                 (* unknown indicator *)
      {
        Src_lexing.unexpected (Indicator_char c) state lexbuf
          ~k:(fun state -> fixed_nominal_line (Src_lexing.flush_continued state))
      }
  | epsilon
      {
        gobble_line (Src_lexing.sna state lexbuf) lexbuf
      }
and xopen_line state                                     (* X/Open free-form  *)
  = parse                            (* (note no continuation line indicator) *)
  | "D "
      {
        fixed_debug_line state lexbuf
      }
  | epsilon
      {
        xopen_or_crt_or_acutrm_followup state lexbuf
      }
and crt_line state                                  (* ICOBOL Free-form (CRT) *)
  = parse
  | ['D' 'd']
      {
        fixed_debug_line state lexbuf
      }
  | '-'                                                  (* continuation line *)
      {
        fixed_continue_line state lexbuf
      }
  | epsilon
      {
        xopen_or_crt_or_acutrm_followup state lexbuf
      }
and acutrm_line state   (* ACUCOBOL-GT Terminal (compat with VAX COBOL term.) *)
  = parse
  | "\\D"
      {
        fixed_debug_line state lexbuf
      }
  | '-'                                                  (* continuation line *)
      {
        fixed_continue_line state lexbuf
      }
  | epsilon
      {
        xopen_or_crt_or_acutrm_followup state lexbuf
      }
and xopen_or_crt_or_acutrm_followup state
  = parse
  | ('$' as marker)
      {
        fixed_mf_cdir_line (String.make 1 marker) state lexbuf
      }
  | cdir_word
      {
        Src_lexing.cdir_word ~ktkd:gobble_line ~knom:fixed_nominal state lexbuf
      }
  | (['*' '/'] as marker)                                     (* comment line *)
      {
        comment_line marker state lexbuf
      }
  | epsilon
      {
        fixed_nominal_line (Src_lexing.flush_continued state) lexbuf
      }
and cobolx_line state                                 (* COBOLX format (GCOS) *)
  = parse
  | [' ' '\t']                                                     (* nominal *)
      {
        fixed_nominal_line (Src_lexing.flush_continued state) lexbuf
      }
  | '-'                                                  (* continuation line *)
      {
        fixed_continue_line state lexbuf
      }
  | ('$' as marker)
      {
        fixed_mf_cdir_line (String.make 1 marker) state lexbuf
      }
  | (['*' '/'] as marker)                                     (* comment line *)
      {
        comment_line marker state lexbuf
      }
  | ['D' 'd']
      {
        fixed_debug_line state lexbuf
      }
  | (_#['\n' '\r'] as c)                                 (* unknown indicator *)
      {
        Src_lexing.unexpected (Indicator_char c) state lexbuf
          ~k:(fun state -> fixed_nominal_line (Src_lexing.flush_continued state))
      }
  | cdir_word
      {
        Src_lexing.cdir_word ~ktkd:gobble_line ~knom:fixed_nominal state lexbuf
      }
  | epsilon
      {
        newline_or_eof state lexbuf
      }
and fixed_debug_line state
  = parse
  | epsilon
      {
        let state = Src_lexing.flush_continued state in
        if Src_lexing.allow_debug state
        then fixed_nominal_line state lexbuf
        else gobble_line state lexbuf
      }
and fixed_nominal_line state
  = parse
  | blanks | separators
      {
        fixed_nominal_line state lexbuf
      }
  | cdir_word
      {
        Src_lexing.cdir_word ~ktkd:gobble_line ~knom:fixed_nominal state lexbuf
      }
  | epsilon
      {
        fixed_nominal state lexbuf
      }
and fixed_nominal state
  = parse
  | blanks | separators
      {
        fixed_nominal state lexbuf
      }
  | "*>" nnl* (newline | eof)                             (* floating comment *)
      {
        Src_lexing.comment ~floating:true state lexbuf
      }
  | "=="
      {
        Src_lexing.eqeq ~ktkd:gobble_line ~knom:fixed_nominal
          state lexbuf
      }
  | alphanum_lit_new
      {
        Src_lexing.alphanum_lit ~ktkd:gobble_line ~knom:fixed_nominal
          state lexbuf
      }
  | text_word
      {
        Src_lexing.text_word ~ktkd:gobble_line ~knom:fixed_nominal
          state lexbuf
      }
  | epsilon
      {
        newline_or_eof state lexbuf
      }
and fixed_cdir_line marker state          (* `>>`-prefixed compiler directive *)
  = parse
  | ' '? cdir_word_suffix
      {
        Src_lexing.cdir_word ~ktkd:gobble_line ~knom:fixed_nominal
          ~marker (Src_lexing.flush_continued state) lexbuf
      }
  | epsilon
      {
        newline_or_eof (Src_lexing.flush_continued state) lexbuf
      }
and fixed_mf_cdir_line marker state   (* Micro-focus compiler directive (`$`) *)
  = parse
  | blanks? cdir_word_suffix
      {
        Src_lexing.cdir_word ~ktkd:gobble_line ~knom:fixed_nominal
          ~marker (Src_lexing.flush_continued state) lexbuf
      }
  | epsilon
      {
        newline_or_eof (Src_lexing.flush_continued state) lexbuf
      }
and maybe_fixed_cdir_line c state (* we just read [c='>'] in indicator column *)
  = parse
  | '>'
      {
        fixed_cdir_line ">>" state lexbuf
      }
  | epsilon                                              (* report error on c *)
      {
        Src_lexing.unexpected (Indicator_char c) state lexbuf
          ~k:(fun state -> fixed_nominal_line (Src_lexing.flush_continued state))
      }
and fixed_continue_line state
  = parse
  | blank_area_A
      {
        (* Special rule for double-quoted alphanum continuation and continuation
           of literals with unclosed sequences of symbolic EBCDIC characters.

           NB: The ISO/IEC standard is less specific than IBM docs on this. *)
        let cont = match Src_lexing.continue_quoted_alphanum state with
          | Nominal -> fixed_continue_open
          | Closed Quote -> fixed_continue_quoted
          | Closed Apostrophe -> fixed_continue_apostrophed
          | UnclosedEBCDICs Quote -> fixed_continue_quoted_ebcdics
          | UnclosedEBCDICs Apostrophe -> fixed_continue_apostrophed_ebcdics
        in
        cont state lexbuf
      }
  | nonblank_area_A
      {
        Src_lexing.unexpected Non_blank_area_A_on_continuation_line
          state lexbuf
          ~severity:`Warn
          ~k:begin match Src_lexing.continue_quoted_alphanum state with
            | Nominal -> fixed_continue_open
            | Closed Quote -> fixed_continue_quoted
            | Closed Apostrophe -> fixed_continue_apostrophed
            | UnclosedEBCDICs Quote -> fixed_continue_quoted_ebcdics
            | UnclosedEBCDICs Apostrophe -> fixed_continue_apostrophed_ebcdics
          end
      }
  | epsilon
      {
        newline_or_eof state lexbuf
      }
and fixed_continue_open state
  = parse
  | alphanum_lit
      {
        Src_lexing.alphanum_lit ~ktkd:gobble_line ~knom:fixed_nominal
          state lexbuf
      }
  | text_word
      {
        Src_lexing.text_word ~cont:true ~ktkd:gobble_line ~knom:fixed_nominal
          state lexbuf
      }
  | epsilon
      {
        fixed_nominal state lexbuf
      }
and fixed_continue_apostrophed state                    (* Maybe IBM-specific *)
  = parse
  | alphanum_lit_cont_double_apostrophes
      {
        Src_lexing.alphanum_lit ~doubled_opener:true
          ~ktkd:gobble_line ~knom:fixed_nominal state lexbuf
      }
  | epsilon
      {
        fixed_nominal state lexbuf
      }
and fixed_continue_quoted state                         (* Maybe IBM-specific *)
  = parse
  | alphanum_lit_cont_double_quotes
      {
        Src_lexing.alphanum_lit ~doubled_opener:true
          ~ktkd:gobble_line ~knom:fixed_nominal state lexbuf
      }
  | epsilon
      {
        fixed_nominal state lexbuf
      }
and fixed_continue_apostrophed_ebcdics state
  = parse
  | alphanum_lit_cont_unclosed_ebcdics_double_apostrophes
      {
        Src_lexing.alphanum_lit ~ktkd:gobble_line ~knom:fixed_nominal
          state lexbuf
      }
  | epsilon
      {
        fixed_nominal state lexbuf
      }
and fixed_continue_quoted_ebcdics state
  = parse
  | alphanum_lit_cont_unclosed_ebcdics_double_quotes
      {
        Src_lexing.alphanum_lit ~ktkd:gobble_line ~knom:fixed_nominal
          state lexbuf
      }
  | epsilon
      {
        fixed_nominal state lexbuf
      }

(* Free format *)

and free_line state
  = parse
  | blanks | '\t'
      {
        free_line state lexbuf
      }
  | separators                 (* Allow separators , & ; at begining of line? *)
      {
        free_line (Src_lexing.flush_continued ~force:true state) lexbuf
      }
  | (cdir_word | '$' blanks? cdir_word_suffix)
      {
        Src_lexing.cdir_word' ~k:free_nominal
          (Src_lexing.flush_continued ~force:true state) lexbuf
      }
  | (line_directive_prefix
       (['0'-'9']+ as num) spaces line_directive_filename as dir) nnl*
    {
        match int_of_string num with
        | exception _ ->
            Pretty.failwith "line number out of range: %s" dir
        | line_num ->
            update_loc lexbuf (Some name) (line_num - 1) true 0;
            free_line state lexbuf
      }
  | epsilon
      {
        free_nominal state lexbuf
      }
and free_nominal state
  = parse
  | blanks | separators
      {
        free_nominal state lexbuf
      }
  | "*>" nnl* (newline | eof)                             (* floating comment *)
      {
        Src_lexing.comment ~floating:true state lexbuf
      }
  | "=="
      {
        Src_lexing.eqeq' ~k:free_nominal state lexbuf
      }
  | alphanum_lit_new
      {
        Src_lexing.alphanum_lit' ~k:free_nominal state lexbuf
      }
  | text_word
      {
        Src_lexing.text_word' ~k:free_nominal state lexbuf
      }
  | epsilon
      {
        free_newline_or_eof state lexbuf
      }

(* Common stuff *)

and gobble_line state
  = parse
  | (nnl+)
      {
        gobble_line (Src_lexing.skip state lexbuf) lexbuf
      }
  | newline
      {
        Src_lexing.new_line state lexbuf
      }
  | eof
      {
        Src_lexing.(flush @@ eof state lexbuf)
      }
and comment_line marker state
  = parse
  | (nnl* (newline | eof))
      {
        Src_lexing.comment ~marker:(String.make 1 marker) state lexbuf
      }
and free_gobble_line state
  = parse
  | (nnl* newline)
      {
        Src_lexing.new_line state lexbuf
      }
  | (nnl* eof)
      {
        Src_lexing.(flush @@ eof state lexbuf)
      }

and newline_or_eof state
  = parse
  | newline
      {
        Src_lexing.new_line state lexbuf
      }
  | eof
      {
        Src_lexing.(flush @@ eof state lexbuf)
      }
  | nnl+
      {
        Src_lexing.unexpected Characters state lexbuf
          ~k:gobble_line
      }
and free_newline_or_eof state
  = parse
  | newline
      {
        Src_lexing.new_line state lexbuf
      }
  | eof
      {
        Src_lexing.(flush @@ eof state lexbuf)
      }
  | _
      {
        Src_lexing.unexpected Character state lexbuf ~k:free_gobble_line
      }

(* Text-word tokenizer (compiler directives) *)
and cdtoken = parse

  | blanks
      { cdtoken lexbuf }

  | (nonblank+ as s)
      { CDTok (try Hashtbl.find cdtoken_of_keyword (String.uppercase_ascii s)
               with Not_found -> TEXT_WORD s) }

  | eof
      { CDEnd }

(* Text-word tokenizer (text manipulation statements/replacing clauses) *)
and pptoken = parse

  | blanks
      { pptoken lexbuf }

  | '(' { PPTok LPAR }
  | ')' { PPTok RPAR }
  | '.' { PPTok PERIOD }

  | (([^ '(' ')']+) as s)
      { PPTok (try Hashtbl.find pptoken_of_keyword (String.uppercase_ascii s)
               with Not_found -> TEXT_WORD s) }

  | eof
      { PPEnd }

(* --- *)

{
  let line
    : type k. k Src_lexing.state -> Lexing.lexbuf -> k Src_lexing.state * _
    = fun s -> match Src_lexing.source_format s with
      |          _, FreePaging   -> free_line s
      | XOpenIndic, _            -> xopen_line s
      |   CRTIndic, _            -> crt_line s
      |   TrmIndic, _            -> acutrm_line s
      |  CBLXIndic, _            -> cobolx_line s
      |          _, FixedWidth _ -> fixed_line s
}
