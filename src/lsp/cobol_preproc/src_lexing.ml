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

(** Various utilities for pre-processing cobol code into text and pseudo-text *)

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Text.TYPES
open Src_format

(* --- *)

(* Lexing positons manipulated in this module are expressed in bytes.

   They are adjusted to match actual character positions before they are used to
   build source locations (types `lexloc` and `srcloc`) that are attached to
   emitted text words or other artifacts like ignored portions of source
   text. *)

(* --- *)

let expand_tabs ?(tab_stop=8) ?(starting_col=0) src =
  match String.index_opt src '\t' with
  | None -> src
  | Some _ ->
      let buf = Buffer.create (String.length src) in
      let col = ref starting_col in
      let spaces = String.make tab_stop ' ' in
      String.iter (function
        | '\t' ->
            let n = (tab_stop - !col mod tab_stop) in
            Buffer.add_substring buf spaces 0 n;
            col := !col + 1
        | ('\n' | '\r') as c ->
            Buffer.add_char buf c;
            col := 0;
        | c ->
            Buffer.add_char buf c;
            incr col)
      src;
      Buffer.contents buf

let remove_blanks = Str.global_replace (Str.regexp " ") ""           (* '\t'? *)

(* --- *)

(* TODO: split this into hot/cold records w.r.t likely mutation frequency. *)
type 'k state =
  {
    lex_prods: text;
    continued: continued;
    pseudotext: (srcloc * text) option;
    comments: comments;
    ignored: lexloc list;               (** lexical locations of ignored text *)
    cdir_seen: bool;
    current_cpos_shift: int;
    newline: bool;
    newline_cnums: int list;    (** index of all newline characters encountered
                                    so far (in reverse order) *)
    diags: Src_diagnostics.diagnostics;
    config: 'k config;
  }
and continued =
  | CNone
  | CText of {
      str: string with_loc;                                       (** content *)
      txt: prefix_kind;
    }
and prefix_kind =
  | CAlphanum of {
      qte: quotation;             (** quotation style *)
      knd: literal_kind;          (** possible typing prefix (N, X, B, etc.) *)
      cld: bool;                  (** holds if properly closed with quotation *)
      tbc: bool;                  (** to be continued flag (must) *)
      unclosed_ebcdics: bool Lazy.t;
    }
and 'k config =
  {
    debug: bool;
    source_format: 'k source_format;
  }

let position_encoding_in_bytes = false

let init_state source_format : _ state =
  {
    lex_prods = [];
    continued = CNone;
    pseudotext = None;
    comments = [];
    ignored = [];
    cdir_seen = false;
    current_cpos_shift = 0;
    newline = true;
    newline_cnums = [];
    diags = Src_diagnostics.none;
    config =
      {
        debug = false;
        source_format;
      }
  }

let diagnostics { diags; _ } = diags
let rev_comments { comments; _ } = comments
let rev_ignored { ignored; _ } = ignored
let rev_newline_cnums { newline_cnums; _ } = newline_cnums
let source_format { config = { source_format; _ }; _ } = source_format
let allow_debug { config = { debug; _ }; _ } = debug

(** Flush buffered lexing productions, possibly holding onto one that may be
    subject to continuation on the following line.

    Always flushes completely whenever a compiler-directive word has been seen,
    or the last token is a lonesome period. *)
let flush ({ lex_prods; _ } as state) : _ state * text =
  match lex_prods with
  | { payload = TextWord w; _ } as h :: prods
    when not state.cdir_seen && w <> "." ->
      { state with lex_prods = [h]                   }, List.rev prods
  | prods ->
      { state with lex_prods = []; cdir_seen = false }, List.rev prods

let reset_cont state =
  { state with continued = CNone }

let lex_warn state w =
  { state with diags = Src_diagnostics.add_warning w state.diags }
let lex_error state e =
  { state with diags = Src_diagnostics.add_error e state.diags }

let change_source_format ({ config; _ } as state) sf =
  (* Just check there is no text that requires continuation. *)
  let flushed = function
    | { continued = CNone; pseudotext = None; _ } -> true
    | _ -> false
  in
  if flushed state
  then Ok { state with config = { config with source_format = sf } }
  else Error ()

let pos_column { current_cpos_shift; _ } Lexing.{ pos_bol; pos_cnum; _ } =
  pos_cnum - pos_bol + 1 - current_cpos_shift            (* count cols from 1 *)

let adjust_postion { current_cpos_shift; _ } pos =
  if current_cpos_shift = 0 || position_encoding_in_bytes
  then pos
  else Lexing.{ pos with pos_cnum = pos.pos_cnum - current_cpos_shift }

let raw_loc ~start_pos ~end_pos ?end_state start_state =
  let in_area_a =
    start_state.newline &&
    Src_format.enforceable_area_a start_state.config.source_format &&
    match Src_format.first_area_b_column start_state.config.source_format with
    | None -> false
    | Some c -> pos_column start_state start_pos < c
  in
  let end_state = Option.value end_state ~default:start_state in
  let start_pos = adjust_postion start_state start_pos
  and   end_pos = adjust_postion   end_state   end_pos in
  Cobol_common.Srcloc.raw ~in_area_a (start_pos, end_pos)

type lexeme_info = string * Lexing.position * Lexing.position
let lexeme_info lexbuf : lexeme_info =
  let lexeme = Lexing.lexeme lexbuf
  and start_pos = Lexing.lexeme_start_p lexbuf
  and end_pos = Lexing.lexeme_end_p lexbuf in
  lexeme, start_pos, end_pos

let ignore_lexloc ~start_pos ~end_pos ?start_state end_state =
  let start_state = Option.value start_state ~default:end_state in
  let start_pos = adjust_postion start_state start_pos
  and   end_pos = adjust_postion   end_state   end_pos in
  { end_state with ignored = (start_pos, end_pos) :: end_state.ignored }

let count_utf8_codepoints s =
  let n = ref 0 in
  String.iter (fun c -> if Char.code c land 0xC0 != 0x80 then incr n) s;
  !n

let lexeme_with_utf8_chars state lexbuf =
  let (s, _start_pos, _end_pos) as lexinf = lexeme_info lexbuf in
  let additional_shift = String.length s - count_utf8_codepoints s in
  let end_cpos_shift = state.current_cpos_shift + additional_shift in
  if end_cpos_shift == state.current_cpos_shift
  then lexinf, state
  else lexinf, { state with current_cpos_shift = end_cpos_shift }

let skip state lexbuf =
  (* Note: we use `lexeme_with_utf8_chars` here in case skipped lexeme contain
     extended characters; rules in Src_lexer.mll may need to be adjusted if we
     wanted to avoid the additonal cost. *)
  (* let _, start_pos, end_pos = lexeme_info lexbuf in *)
  let (_, start_pos, end_pos), state = lexeme_with_utf8_chars state lexbuf in
  ignore_lexloc ~start_pos ~end_pos state

let emit prod ({ pseudotext; cdir_seen; _ } as state) =
  match pseudotext with
  | None ->
      { state with lex_prods = prod :: state.lex_prods;
                   cdir_seen = cdir_seen || Text.cdirp prod;
                   newline = false }
  | Some (start_loc, prods) ->
      { state with pseudotext = Some (start_loc, prod :: prods);
                   cdir_seen = cdir_seen || Text.cdirp prod;
                   newline = false }

let append t state =
  match state, t with
  | { pseudotext = None;
      lex_prods = { payload = TextWord w; loc = wloc } :: tl; _ },
    { payload = TextWord s; loc = tloc } ->
      let prod = TextWord (w ^ s) &@ (Cobol_common.Srcloc.concat wloc tloc) in
      { state with lex_prods = prod :: tl;
                   newline = false }
  | { pseudotext = Some (start_loc, { payload = TextWord w;
                                      loc = wloc} :: prods); _ },
    { payload = TextWord s; loc = tloc } ->
      let prod = TextWord (w ^ s) &@ (Cobol_common.Srcloc.concat wloc tloc) in
      { state with pseudotext = Some (start_loc, prod :: prods);
                   newline = false }
  | _, { loc; payload = word } ->
      (* NB: this seems to be allowed after all: *)
      (* lex_warn state @@ Warn_unexpected { loc; *)
      (*                                     item = Word_in_continuation word } *)
      ignore (loc, word);
      emit t state

let sna ({ config = { source_format; _ }; _ } as state) lexbuf =
  let indicator_pos, FixedWidth _ = source_format in
  match indicator_pos with
  | FixedIndic ->
      let _, start_pos, end_pos = lexeme_info lexbuf in
      let lex_len = end_pos.pos_cnum - start_pos.pos_cnum in
      let sna_len = min 6 lex_len in
      let end_pos = { start_pos with
                      pos_cnum = start_pos.pos_cnum + sna_len } in
      ignore_lexloc ~start_pos ~end_pos state
  | _ ->
      state

let new_line state lexbuf =
  Lexing.new_line lexbuf;
  let state =
    { state with
      current_cpos_shift = 0;
      newline = true;
      newline_cnums = Lexing.lexeme_end lexbuf :: state.newline_cnums }
  in
  match state.lex_prods, state.cdir_seen with
  | { payload = TextWord _ | Separator _ | Alphanum _ | Pseudo _ | Eof; _ } :: _, _
  | _, true ->
      flush state
  | _ ->
      state, []

(* --- *)

let unexpected
    (item: Src_diagnostics.unexpected_stuff)
    ?(severity : [`Error | `Warn] = `Error)
    ~k state lexbuf =
  let loc =
    let _, start_pos, end_pos = lexeme_info lexbuf in
    raw_loc ~start_pos ~end_pos state
  in
  let state = match severity with
    | `Error -> lex_error state @@ Unexpected { loc; item }
    | `Warn -> lex_warn state @@ Warn_unexpected { loc; item }
  in
  k state lexbuf

(* --- *)

let textword s =
  TextWord ~&s &@<- s

let cdirword ?marker ~start_pos ~end_pos s state =
  let s = remove_blanks s in
  let s, start_pos = match marker with
    | Some m ->
        m ^ s, Lexing.{ start_pos with
                        pos_cnum = start_pos.pos_cnum - String.length m }
    | None ->
        s, start_pos
  in
  let loc = raw_loc ~start_pos ~end_pos state in
  emit (CDirWord s &@ loc) state


let rev_pseudotext: text -> _ state -> pseudotext * _ state = fun text state ->
  List.fold_left begin fun (acc, state) pt -> match ~&pt with
    | TextWord w ->
        (Text.pseudo_string (w &@<- pt)) :: acc, state
    | Alphanum a ->
        (PseudoAlphanum a &@<- pt) :: acc, state
    | Separator _ ->
        acc, state                                       (* ignore separators *)
    | word ->
        acc, lex_error state @@ Unexpected { loc = ~@pt;
                                             item = Word_in_pseudotext word }
  end ([], state) text

let pseudotext_delimiter ~loc = function
  | { pseudotext = None; _ } as state ->
      { state with pseudotext = Some (loc, []);
                   newline = false }
  | { pseudotext = Some (start_loc, prods); _ } as state ->
      assert (state.continued = CNone);
      (* Here, we assume pseudotext only spans in a single text file. *)
      let start_pos = Cobol_common.Srcloc.start_pos start_loc
      and _, end_pos = Cobol_common.Srcloc.as_lexloc loc in
      let loc = raw_loc ~start_pos ~end_pos state in
      let state = { state with pseudotext = None; newline = false } in
      let pseudotext, state = rev_pseudotext prods state in
      emit (Pseudo pseudotext &@ loc) state


(* Hackish approach to deal with picture strings ending with a period or comma:
   thankfully, they must be immediately followed by another period or comma and
   terminate the sentence.  As a result, text words may either terminate with
   two periods, two commas, or no period or comma at all.

   We just intercept the emission of each text word s ending with either a dot
   or a comma, and emit the dot separately or discard the comma.

   TODO: This currently applies in pseudo-text too; is this the expected
   behavior? *)
let gen emit0 = function
  | { payload = TextWord str; loc } as s ->
      let len = String.length str in
      (* XXX: may pos' end up with an invalid column number? *)
      if len > 1 && str.[len - 1] = '.' then
        let sloc = Cobol_common.Srcloc.trunc_suffix 1 loc
        and ploc = Cobol_common.Srcloc.suffix 1 loc in
        fun state ->
          emit0 (textword (String.sub str 0 (len - 1) &@ sloc)) state |>
          emit (TextWord "." &@ ploc)
      else if len > 1 && (str.[len - 1] = ',' || str.[len - 1] = ';') then
        let sloc = Cobol_common.Srcloc.trunc_suffix 1 loc
        and ploc = Cobol_common.Srcloc.suffix 1 loc in
        fun state ->
          emit0 (textword (String.sub str 0 (len - 1) &@ sloc)) state |>
          emit (Separator str.[len - 1] &@ ploc)
      else
        emit0 s
  | s ->
      emit0 s

let emit s = gen emit s
let append = gen append

let flush_continued ?(force = false) state = match state.continued with
  | CNone ->
      state
  | CText { txt = CAlphanum { tbc = true; _ }; _ } when not force ->
      state
  | CText { str = { payload = str; loc };
            txt = CAlphanum { qte; knd; cld = true; tbc = false; _ } } ->
      emit (Alphanum { knd; qte; str } &@ loc) (reset_cont state)
  | CText { str = { payload = prefix; loc };
            txt = CAlphanum { qte; knd; _ } } when force ->
      lex_error (reset_cont state) (Missing_continuation { loc; prefix }) |>
      emit (Alphanum { knd; qte; str = prefix } &@ loc)
  | CText { str = { payload = str; loc };
            txt = CAlphanum { qte; knd; _ } } ->
      (* Missing continuation error is delayed until the final tokenization
         stage to account for quotes in comment paragraphs. *)
      emit (AlphanumPrefix { knd; qte; str } &@ loc) (reset_cont state)

let eof state lexbuf =
  let _, start_pos, end_pos = lexeme_info lexbuf in
  let loc = raw_loc ~start_pos ~end_pos state in
  let state = flush_continued ~force:true state in  (* checks state.continued *)
  match state.pseudotext with                       (* and state.pseudotext  *)
  | None ->
      emit (Eof &@ loc) state
  | Some (start_loc, _prods) ->
      let state = { state with pseudotext = None } in
      (* As in `pseudotext_delimiter`, we assume pseudotext only spans in a
         single text file. *)
      let start_pos = Cobol_common.Srcloc.start_pos start_loc
      and _, end_pos = Cobol_common.Srcloc.as_lexloc loc in
      let loc = raw_loc ~start_pos ~end_pos state in
      lex_error state @@ Unterminated_pseudotext { loc } |>
      emit (Eof &@ loc)

(* --- *)


let remove_floating_comment ~start_pos ~end_pos w state =
  let w_len = String.length w in
  let rec floating_comment idx =
    if idx >= w_len - 1 then None else match w.[idx] with
      | '*' when w.[idx + 1] = '>' -> Some idx
      | _ -> floating_comment (succ idx)
  in
  match floating_comment 0 with
  | None ->
      w, end_pos, state
  | Some comment_idx ->
      let comment_len = String.length w - comment_idx in
      let start_cnum = start_pos.Lexing.pos_cnum + comment_idx + 1 in
      let comment =
        {
          comment_loc = { start_pos with pos_cnum = start_cnum }, end_pos;
          comment_kind = `Floating;
          comment_contents = String.sub w comment_idx comment_len;
        }
      in
      String.sub w 0 comment_idx,
      Lexing.{ end_pos with pos_cnum = start_pos.pos_cnum + comment_idx },
      { state with comments = comment :: state.comments }


type line_fitting = Nominal | Tacked

let text_word ?(cont = false) ~start_pos ~end_pos ?(fitting = Nominal) w state =
  ignore fitting;
  let w, end_pos, end_state =
    remove_floating_comment ~start_pos ~end_pos w state
  in
  let wloc = raw_loc ~start_pos ~end_pos ~end_state state in
  let w = w &@ wloc in
  match end_state.continued with
  | CNone when cont ->
      append (textword w) end_state
  | CNone ->
      emit (textword w) end_state
  | CText _ ->
      let state = flush_continued ~force:true end_state in
      let state = lex_error state @@ Unexpected { loc = wloc; item = Word } in
      emit (textword w) state

let separator ~char ~start_pos ~end_pos ?(fitting = Nominal) _s state =
  ignore fitting;
  emit (Separator char &@ raw_loc ~start_pos ~end_pos state) state

let to_be_continued_alphanum: string -> bool =
  fun s -> match s.[0], Str.last_chars s 2 with
    | '"', "\"-"
    | '\'', "'-" -> true
    | _ -> false
    | exception Invalid_argument _ -> false

let count_char c s =
  let i = ref 0 in
  String.iter (fun c' -> if c == c' then incr i) s;
  !i

let closed_alphanum: string -> bool = fun s ->
  (* Note we allow alphanumeric tokens to be empty, although some old standards
     may forbid them; this may be checked later on though. *)
  String.length s >= 2 &&
  match s.[0], (Str.last_chars s 1, Str.last_chars s 2) with
  | '\'', ("'", _ | _, "'-") -> count_char '\'' s mod 2 == 0
  | '"', ("\"", _ | _, "\"-") -> count_char '"' s mod 2 == 0
  | _ -> false                                                      (* in case *)
  | exception Invalid_argument _ -> true                            (* in case *)

let strip_left_quote str =
  String.sub str 1 (String.length str - 1)

let strip_right_quote str =
  String.sub str 0 (String.length str - 1)

let strip_quotes str =
  let len = String.length str in
  if to_be_continued_alphanum str then String.sub str 1 (len - 3)
  else if closed_alphanum str then String.sub str 1 (len - 2)
  else String.sub str 1 (len - 1)

let unclosed_ebcdics =
  (* NOTE: applies on internal representation, i.e, without surrounding
     quotes/apostrophes. *)
  let symc = "[0-9][0-9, ]*" in                            (* symbolic EBCDIC *)
  let dblq = "\\([^\"]\\|\"" ^ symc ^ "\"\\|\"\"\\)*\"" ^ symc in
  let splq = "\\([^']\\|'" ^ symc ^ "\"\\|''\\)*'" ^ symc in
  let re = Str.regexp_case_fold ("^\\(" ^ dblq ^ "\\|" ^ splq ^ "\\)$") in
  fun str -> Str.string_match re str 0

let quoted_alphanum ?(fitting = Nominal) ~knd
    ({ payload = str; _ } as str') state =
  (* Note substitution of doubled quotation or apostrophe marks is delayed until
     after text manipulation stage. *)
  let cld = closed_alphanum str
  and tbc = to_be_continued_alphanum str
  and qte = if str.[0] = '\'' then Apostrophe else Quote in
  match state.continued with
  | CNone when fitting = Nominal && cld && not tbc ->
      emit (Alphanum { knd; qte; str = strip_quotes str } &@<- str') state
  | CNone ->
      let str = strip_quotes str in
      let unclosed_ebcdics = lazy (unclosed_ebcdics str) in
      { state with
        continued = CText { str = str &@<- str';
                            txt = CAlphanum { qte; knd; cld; tbc;
                                              unclosed_ebcdics }; };
        newline = false }
  | CText { str = s0;
            txt = CAlphanum { qte = q0; knd = k0; _ } } ->
      let state =
        if q0 = qte then state else
          lex_error state @@
          Mismatch_in_alphanum_continuation { continued_alphanum_loc = ~@str';
                                              expected_quotation =q0 }
      in
      let str = ~&s0 ^ strip_left_quote str
      and strloc = Cobol_common.Srcloc.concat ~@s0 ~@str' in
      let cld = match qte with
        | Quote -> closed_alphanum ("\"" ^ str)
        | Apostrophe -> closed_alphanum ("'" ^ str)
      in
      let str = if cld then strip_right_quote str else str in
      if fitting = Nominal && cld && not tbc then
        emit (Alphanum { knd = k0; qte; str } &@ strloc) (reset_cont state)
      else
        let unclosed_ebcdics = lazy (unclosed_ebcdics str) in
        { state with
          continued = CText { str = str &@ strloc;
                              txt = CAlphanum { qte; knd = k0; cld; tbc;
                                                unclosed_ebcdics } };
          newline = false }

type alphanumeric_continuation =
  | Nominal
  | Closed of Text.quotation
  | UnclosedEBCDICs of Text.quotation

let continue_quoted_alphanum state = match state.continued with
  | CText { txt = CAlphanum { qte; cld; tbc; _ }; _ }
    when cld && not tbc ->
      Closed qte
  | CText { txt = CAlphanum { qte; cld; tbc; unclosed_ebcdics; _ }; _ }
    when not cld && not tbc && Lazy.force unclosed_ebcdics ->
      UnclosedEBCDICs qte
  | _ ->
      Nominal

(* --- *)

let extract_knd str state lexbuf =
  let open struct
    exception UnexpectedChar of char
    exception UnexpectedStr
  end in
  try
    (* TODO: use start_pos & end_pos instead (see below) *)
    let s, knd = match str.[0] with
      | '"' | '\'' -> str, Basic
      | 'B' | 'b' -> Str.string_after str 1, Bool
      | 'H' | 'h'
      | 'X' | 'x' -> Str.string_after str 1, Hex
      | 'Z' | 'z' -> Str.string_after str 1, NullTerm
      | 'N' | 'n' -> Str.string_after str 1, National
      | c -> raise @@ UnexpectedChar c
    in
    let s, knd = match s.[0], knd with
      |('"' | '\''), knd -> s, knd
      |('X' | 'x'), Bool -> Str.string_after s 1, BoolX
      |('X' | 'x'), National -> Str.string_after s 1, NationalX
      | _ -> raise @@ UnexpectedStr
    in
    s, knd, state
  with
  | UnexpectedChar c ->
      unexpected (Opening_alphanumeric_literal_delimiter (String.make 1 c))
        state lexbuf
        ~k:(fun state _lexbuf -> Str.string_after str 1, Basic, state)
  | UnexpectedStr ->
      unexpected (Opening_alphanumeric_literal_delimiter str)
        state lexbuf
        ~k:(fun state _lexbuf -> Str.string_after str 2, Basic, state)

let comment ?(marker = "") ?(floating = false) state lexbuf =
  let s, start_pos, end_pos = lexeme_info lexbuf in
  let start_pos =                       (* include location of comment marker *)
    Lexing.{ start_pos with
             pos_cnum = start_pos.pos_cnum - String.length marker } in
  let comment_contents, end_pos =
    if EzString.ends_with ~suffix:"\n" s          (* remove potential newline *)
    then marker ^ String.(sub s 0 (String.length s - 1)),
         Lexing.{ end_pos with pos_cnum = end_pos.pos_cnum - 1 }
    else marker ^ s, end_pos
  in
  let comment =
    {
      comment_loc = start_pos, end_pos;
      comment_kind = if floating then `Floating else `Line;
      comment_contents;
    }
  in
  new_line { state with comments = comment :: state.comments } lexbuf

let prefix_buff = Buffer.create 42           (* Warning: shared mutable state *)
let string_prefix_with_utf8_codepoints s up_to_index =
  let rec aux n i =
    if i < up_to_index then
      let u = String.get_utf_8_uchar s i in
      let l = Uchar.utf_decode_length u in
      Buffer.add_substring prefix_buff s i l;
      aux (n + 1) (i + l)
  in
  Buffer.clear prefix_buff;
  aux 0 0;
  Buffer.contents prefix_buff

let trunc_to_col n ((s, sp, ep) as info: lexeme_info) ~start_state ~end_state =
  let sc = pos_column start_state sp and ec = pos_column end_state ep in
  assert (sc <= n);        (* starts on last column (CHECKME: always avoided?) *)
  if ec <= n               (* ends before, or on, column n *)
  then
    info, (if ec = n + 1 then Tacked else Nominal), end_state
  else                  (* truncate lexeme and shift end position accordingly *)
  if start_state.current_cpos_shift == end_state.current_cpos_shift
  then          (* no extra character postion shift: use regular substitution *)
    let s' = String.sub s 0 (n - sc + 1) in
    let ep' = { ep with pos_cnum = ep.pos_cnum - ec + n + 1 } in
    (s', sp, ep'), Tacked, ignore_lexloc ~start_pos:ep' ~end_pos:ep end_state
  else                                             (* have to walk characters *)
    let s_len = String.length s in
    let s' = string_prefix_with_utf8_codepoints s (s_len - ec + n + 1) in
    let trunc_len = s_len - String.length s' in
    let ep' = { ep with pos_cnum = ep.pos_cnum - trunc_len } in
    (s', sp, ep'), Tacked, ignore_lexloc ~start_pos:ep' ~end_pos:ep end_state

let fixed_text mk ({ config = { source_format; _ }; _ } as state) lexbuf =
  (* Note: assumes the current lexeme does not contain characters/codepoints
     that occupy strictly more than a byte. *)
  let _, FixedWidth { cut_at_col; _ } = source_format in
  let (_, start_pos, end_pos) as lexinf = lexeme_info lexbuf in
  if pos_column state start_pos > cut_at_col then
    ignore_lexloc ~start_pos ~end_pos state, Tacked
  else
    let (s, start_pos, end_pos), fitting, state =
      trunc_to_col cut_at_col lexinf ~start_state:state ~end_state:state
    in
    mk ~start_pos ~end_pos ?fitting:(Some fitting) s state, fitting

let fixed_text_word ?cont : fixed state -> _ =
  fixed_text (text_word ?cont)

let fixed_cdir_word ?marker : fixed state -> _ =
  fixed_text begin fun ~start_pos ~end_pos ?fitting s state ->
    ignore fitting;                          (* cannot be broken across lines *)
    cdirword ~start_pos ~end_pos ?marker s state
  end

let fixed_eqeq: fixed state -> _ =
  fixed_text begin fun ~start_pos ~end_pos ?fitting w state ->
    if w = "=="               (* check [w] has not been truncated to only "=" *)
    then pseudotext_delimiter ~loc:(raw_loc ~start_pos ~end_pos state) state
    else text_word ~start_pos ~end_pos ?fitting w state
  end

let fixed_separator ~char : fixed state -> _ =
  fixed_text (separator ~char)

let continuing_unclosed_ebcdics = function
  | { continued = CText { txt = CAlphanum { unclosed_ebcdics; _ }; _ }; _ } ->
      Lazy.force unclosed_ebcdics
  | _ -> false

let fixed_alphanum_lit ?(doubled_opener = false)
    ({ config = { source_format; _ }; _ } as start_state: fixed state) lexbuf =
  let _, FixedWidth { cut_at_col; alphanum_padding; _ } = source_format in
  let (_, start_pos, end_pos) as lexinf, end_state =
    lexeme_with_utf8_chars start_state lexbuf
  in
  let end_col = pos_column end_state end_pos in
  assert (end_col > 0);                (* should never have zero-length token *)
  if pos_column start_state start_pos > cut_at_col then
    end_state, Tacked
  else
    let (s, start_pos, end_pos), fitting, end_state =
      trunc_to_col cut_at_col lexinf ~start_state ~end_state
    in
    let s, knd, end_state = extract_knd s end_state lexbuf in
    let s, end_pos, fitting =
      (* Actually double the opening delimiter ('\'' or '"'), to have the
         doubled quote/apostrophe character prefix after stripping of opening
         and closing delimiters in `quoted_alphanum`.  *)
      let s = if doubled_opener then String.sub s 0 1 ^ s else s in
      let length_to_right_col = cut_at_col - end_col + 1 in
      if closed_alphanum s || length_to_right_col <= 0 ||
         continuing_unclosed_ebcdics start_state
      then s, end_pos, fitting
      else match alphanum_padding with
        | None ->
            s, end_pos, Tacked
        | Some c ->
            let pos_cnum = end_pos.pos_cnum + length_to_right_col in
            let end_pos = { end_pos with pos_cnum } in
            s ^ String.make length_to_right_col c, end_pos, Tacked
    in
    let loc = raw_loc ~start_pos ~end_pos start_state ~end_state in
    quoted_alphanum ~fitting ~knd (s &@ loc) end_state, fitting

(* --- *)

let fixed_srctok mk ~ktkd ~knom state lexbuf =
  let state, fitting = mk state lexbuf in
  (if fitting = Tacked then ktkd else knom) state lexbuf

let text_word ?cont = fixed_srctok (fixed_text_word ?cont)
let cdir_word ?marker s = fixed_srctok (fixed_cdir_word ?marker) s
let eqeq s = fixed_srctok fixed_eqeq s
let separator ~char s = fixed_srctok (fixed_separator ~char) s
let alphanum_lit ?doubled_opener =
  fixed_srctok (fixed_alphanum_lit ?doubled_opener)

(* Free-format versions: *)

let free_srctok mk state lexbuf =
  let s, start_pos, end_pos = lexeme_info lexbuf in
  mk ~start_pos ~end_pos s state

let free_text_word state =
  free_srctok begin fun ~start_pos ~end_pos s state ->
    let loc = raw_loc ~start_pos ~end_pos state in
    emit (textword (s &@ loc)) state
  end state

let free_cdir_word state =
  free_srctok (cdirword ?marker:None) state

let free_eqeq state =
  free_srctok begin fun ~start_pos ~end_pos _ state ->
    pseudotext_delimiter ~loc:(raw_loc ~start_pos ~end_pos state) state
  end state

let free_separator ~char state =
  free_srctok begin fun ~start_pos ~end_pos _ state ->
    emit (Separator char &@ raw_loc ~start_pos ~end_pos state) state
  end state

let free_alphanum_lit state lexbuf =
  let s, start_pos, end_pos = lexeme_info lexbuf in
  let s, knd, state = extract_knd s state lexbuf in
  quoted_alphanum ~knd (s &@ raw_loc ~start_pos ~end_pos state) state

(* --- *)

let free_text mk ~k state lexbuf = k (mk state lexbuf) lexbuf

let text_word' ~k = free_text free_text_word ~k
let cdir_word' ~k = free_text free_cdir_word ~k
let eqeq' ~k = free_text free_eqeq ~k
let separator' ~char ~k = free_text (free_separator ~char) ~k
let alphanum_lit' ~k = free_text free_alphanum_lit ~k

(* --- *)
