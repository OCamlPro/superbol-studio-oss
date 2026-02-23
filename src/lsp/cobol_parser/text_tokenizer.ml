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

open EzCompat
module TEXT = Cobol_preproc.Text

open Cobol_common.Srcloc.INFIX
open Cobol_common.Srcloc.TYPES
open Cobol_preproc.Text.TYPES
open Grammar_tokens                              (* import token constructors *)
open Parser_diagnostics

(* --- *)

type lexer_update =
  | Enabled_keywords of Text_lexer.TokenHandles.t
  | Disabled_keywords of Text_lexer.TokenHandles.t
  | Enabled_intrinsics
  | Disabled_intrinsics
  | CommaBecomesDecimalPoint

type token = Grammar_tokens.token with_loc
type stream = {
  toks : token list ;
  enabled_keywords : Text_lexer.TokenHandles.t ;
}

type tokens = {
  former_stream : stream ;
  new_stream : stream ;
}

let pp_tokens ppf { former_stream ; _ } =
  Grammar_tokens_printer.pp_tokens ppf former_stream.toks

let loc_in_area_a: srcloc -> bool = Cobol_common.Srcloc.in_area_a
let token_in_area_a: token -> bool = fun t -> loc_in_area_a ~@t

(* --- *)

(* Tokenization of manipulated text, to feed the compilation group parser: *)

let preproc_n_combine_tokens ~intrinsics_enabled ~source_format =
  (* Simplifies the grammar, and applies some token-based pre-processsing to
     deal with old-style informational paragraphs (COBOL85). *)
  let ( +@+ ) = Cobol_common.Srcloc.concat
  and start_pos = Cobol_common.Srcloc.start_pos in
  let comment_entry_termination =
    let Cobol_preproc.Src_format.SF sf = source_format in
    Cobol_preproc.Src_format.comment_entry_termination sf
  and info_word = function
    | WORD w ->
        INFO_WORD w
    | t ->
        (* Try de-tokenizing to accept, e.g, PROGRAM-ID. nested. (as NESTED is a
           keyword). *)
        try INFO_WORD (Hashtbl.find Text_lexer.word_of_token t)
        with Not_found -> t
  and function_name = function
    | t when not intrinsics_enabled ->
        t
    | WORD w | WORD_IN_AREA_A w as t ->
        (try Text_lexer.token_of_intrinsic w with Not_found -> t)
    | t ->
        (try (Text_lexer.token_of_intrinsic @@
              Hashtbl.find Text_lexer.word_of_token t)
         with Not_found -> t)
  and comment_entry revtoks =
    COMMENT_ENTRY (LIST.rev_map (Pretty.to_string "%a" Grammar_tokens_printer.pp_token_string) revtoks)
  in
  let rec skip ((p', l', dgs) as acc) ((p, l) as pl) = function
    | 0 -> acc, pl
    | i -> skip (LIST.hd p :: p', LIST.hd l :: l', dgs) (LIST.tl p, LIST.tl l) (i - 1)
  and aux acc (p, l) =
    let subst_n x y =
      let rec cons x ((p', l', dgs) as _acc) (p, l) = function
        | 0 -> assert false
        | 1 -> aux (x :: p', LIST.hd l :: l', dgs) (LIST.tl p, LIST.tl l)
        | i -> cons x acc (LIST.tl p, LIST.hd l +@+ LIST.hd (LIST.tl l) :: LIST.tl (LIST.tl l)) (i - 1)
      in
      cons x acc (p, l) y
    and replace_n x y =
      let rec cons x ((p', l', dgs) as acc) ((p, l) as pl) = function
        | 0 -> aux acc pl
        | i -> cons x (x :: p', hd l :: l', dgs) (tl p, tl l) (i - 1)
      in
      cons x acc (p, l) y
    and info_word_after n =
      let (p', l', dgs), (p, l) = skip acc (p, l) n in
      match p with
      | [] -> Result.Error `MissingInputs
      | t :: _ -> aux (info_word t :: p', LIST.hd l :: l', dgs) (LIST.tl p, LIST.tl l)
    and function_name_after n =
      let (p', l', dgs), (p, l) = skip acc (p, l) n in
      match p with
      | [] -> Result.Error `MissingInputs
      | t :: _ -> aux (function_name t :: p', LIST.hd l :: l', dgs) (LIST.tl p, LIST.tl l)
    and missing_continuation_of str =
      let (p', l', diags), pl = skip acc (p, l) 1 in
      let error = Missing { loc = LIST.hd l; stuff = Continuation_of str } in
      aux (p', l', Parser_diagnostics.add_error error diags) pl
    and comment_entry_after n =
      let acc, ((p, l) as suff) = skip acc (p, l) n in
      if p = [] then Result.Error `MissingInputs else
        let consume_comment = match comment_entry_termination with
          | Newline ->
              comment_line ~init_pos:(Cobol_common.Srcloc.start_pos @@ LIST.hd l)
          | Period ->
              comment_paragraph ?stop_column:None
          | AreaB { first_area_b_column } ->
              comment_paragraph ~stop_column:first_area_b_column
        and at_end ~loc ~revtoks (p', l', diags) =
          comment_entry revtoks :: p', loc :: l', diags
        in
        consume_comment ~loc:(LIST.hd l) ~revtoks:[] ~at_end
          Comment_entry acc suff
    in
    match p with

    | [ON] | [NOT] | [NOT; ON]       -> Error `MissingInputs
    | ON :: EXCEPTION :: _             -> subst_n     ON_EXCEPTION 2
    | NOT :: EXCEPTION :: _            -> subst_n NOT_ON_EXCEPTION 2
    | NOT :: ON :: EXCEPTION :: _       -> subst_n NOT_ON_EXCEPTION 3

    | ON :: OVERFLOW :: _              -> subst_n     ON_OVERFLOW 2
    | NOT :: OVERFLOW :: _             -> subst_n NOT_ON_OVERFLOW 2
    | NOT :: ON :: OVERFLOW :: _        -> subst_n NOT_ON_OVERFLOW 3

    | [ON; SIZE] | [SIZE]
    | [NOT; ON; SIZE] | [NOT; SIZE]  -> Error `MissingInputs
    | ON :: SIZE :: ERROR :: _          -> subst_n     ON_SIZE_ERROR 3
    | SIZE :: ERROR :: _               -> subst_n     ON_SIZE_ERROR 2
    | NOT :: ON :: SIZE :: ERROR :: _    -> subst_n NOT_ON_SIZE_ERROR 4
    | NOT :: SIZE :: ERROR :: _         -> subst_n NOT_ON_SIZE_ERROR 3

    | [INVALID] | [NOT; INVALID]     -> Error `MissingInputs
    | INVALID :: KEY :: _              -> subst_n     INVALID_KEY 2
    | INVALID :: _                    -> subst_n     INVALID_KEY 1
    | NOT :: INVALID :: KEY :: _        -> subst_n NOT_INVALID_KEY 3
    | NOT :: INVALID :: _              -> subst_n NOT_INVALID_KEY 2

    | [AT] | [NOT; AT]               -> Error `MissingInputs
    | AT :: END :: _                   -> subst_n     AT_END 2
    | NOT :: AT :: END :: _             -> subst_n NOT_AT_END 3
    | NOT :: END :: _                  -> subst_n NOT_AT_END 2

    | AT ::(END_OF_PAGE|EOP):: _       -> subst_n     AT_EOP 2
    | NOT :: AT ::(END_OF_PAGE|EOP):: _ -> subst_n NOT_AT_EOP 3
    | NOT ::(END_OF_PAGE|EOP):: _      -> subst_n NOT_AT_EOP 2

    | [WITH] | [NO]                  -> Error `MissingInputs
    | WITH :: DATA :: _                -> subst_n WITH_DATA 2
    | NO :: DATA :: _                  -> subst_n NO_DATA 2

    | [WITH; NO]                     -> Error `MissingInputs
    | WITH :: NO :: ADVANCING :: _     -> subst_n WITH_NO_ADVANCING 3
    | NO :: ADVANCING :: _             -> subst_n WITH_NO_ADVANCING 2

    | [IS]                           -> Error `MissingInputs
    | IS :: GLOBAL :: _                -> subst_n IS_GLOBAL 2
    | IS :: EXTERNAL :: _              -> subst_n IS_EXTERNAL 2
    | IS :: TYPEDEF :: _               -> subst_n IS_TYPEDEF 2

    | [DATA]                         -> Error `MissingInputs
    | DATA :: RECORD :: _              -> subst_n DATA_RECORD 2
    | DATA :: RECORDS :: _             -> subst_n DATA_RECORDS 2

    | [NEXT]                         -> Error `MissingInputs
    | NEXT :: PAGE :: _                -> subst_n NEXT_PAGE 2
    | NEXT :: SENTENCE :: _            -> subst_n NEXT_SENTENCE 2

    | [CONSTANT]                     -> Error `MissingInputs
    | CONSTANT :: RECORD :: _          -> subst_n CONSTANT_RECORD 2

    | PROGRAM_ID :: PERIOD :: _
    | FUNCTION_ID :: PERIOD :: _
    | CLASS_ID :: PERIOD :: _
    | INTERFACE_ID :: PERIOD :: _      -> info_word_after 2

    | PROGRAM_ID :: _
    | FUNCTION_ID :: _                -> info_word_after 1

    | [END]                          -> Error `MissingInputs
    | END :: PROGRAM :: _
    | END :: FUNCTION :: _
    | END :: CLASS :: _
    | END :: INTERFACE :: _            -> info_word_after 2

    | FUNCTION :: _                   -> function_name_after 1

    | [AUTHOR | INSTALLATION |
       DATE_WRITTEN | DATE_MODIFIED |
       DATE_COMPILED | REMARKS |
       SECURITY]                     -> Error `MissingInputs
    | (AUTHOR | INSTALLATION |
       DATE_WRITTEN | DATE_MODIFIED |
       DATE_COMPILED | REMARKS |
       SECURITY) :: PERIOD :: _        -> comment_entry_after 2

    | LPAR :: rest ->
      let rec check_par nb_par = function
        | LPAR :: rest -> check_par (nb_par + 1) rest
        | [IS] | [IS; NOT] -> Error `MissingInputs
        | (GREATER | LESS | EQUAL | EQ | NE | GT | LT | GE | LE) :: _
        | (IS | NOT) :: (GREATER | LESS | EQUAL | EQ | NE | GT | LT | GE | LE) :: _
        | IS :: NOT :: (GREATER | LESS | EQUAL | EQ | NE | GT | LT | GE | LE) :: _
            -> replace_n LPAR_BEFORE_RELOP nb_par
        | _ -> replace_n LPAR nb_par
      in
      check_par 1 rest

    | ALPHANUM_PREFIX { str; _ } :: _ -> missing_continuation_of str

    | tok :: _                        -> subst_n tok 1


    | []                             -> Ok acc

  and comment_paragraph ?stop_column ~loc ~revtoks ~at_end descr acc = function
    | [], _ ->                (* no word starting in Area A, or not period yet *)
        Result.Error `MissingInputs
    | EOF :: _ as p, l ->                        (* ignore all tokens until EOF *)
        let _, _, diags = at_end ~loc ~revtoks acc in
        Error (`ReachedEOF (loc, descr, diags, p, l))
    | PERIOD as period :: p, period_loc :: l
      when Option.is_none stop_column ->
        let revtoks = period :: revtoks and loc = loc +@+ period_loc in
        aux (at_end ~loc ~revtoks acc) (p, l)
    | p, (p_loc :: _ as l)
      when (let Lexing.{ pos_bol; pos_cnum; _ } = start_pos p_loc in
            Option.fold stop_column                (* stop_column starts at 1 *)
              ~some:(fun col -> pos_cnum - pos_bol + 1 < col) ~none:false) ->
        aux (at_end ~loc ~revtoks acc) (p, l)
    | t :: tlp, l ->
        comment_paragraph ?stop_column ~at_end descr acc (tlp, LIST.tl l)
          ~loc:(loc +@+ LIST.hd l) ~revtoks:(t :: revtoks)

  and comment_line ~init_pos ~loc ~revtoks ~at_end descr acc = function
    | [], _ ->                  (* found no word starting on anther line (yet) *)
        Result.Error `MissingInputs
    | EOF :: _ as p, l ->                                (* non-terminated line *)
        aux (at_end ~loc ~revtoks acc) (p, l)
    | p, (p_loc :: _ as l)
      when (let Lexing.{ pos_fname; pos_bol; _ } = start_pos p_loc in
            pos_bol > init_pos.Lexing.pos_bol ||
            pos_fname <> init_pos.pos_fname) ->
        aux (at_end ~loc ~revtoks acc) (p, l)
    | t :: tlp, l ->
        comment_line ~init_pos ~at_end descr acc (tlp, LIST.tl l)
          ~loc:(loc +@+ LIST.hd l) ~revtoks:(t :: revtoks)

  in
  fun tokens ->
    let p, srclocs = LIST.split @@ LIST.map (~&@) tokens in
    match aux ([], [], Parser_diagnostics.none) (p, srclocs) with
    | Ok (p, l, dgs) ->
        Ok (LIST.rev_map2 (&@) p l, dgs)
    | Error (`ReachedEOF (loc, descr, dgs, p, l)) ->
        Error (`ReachedEOF (loc, descr, dgs, LIST.rev_map2 (&@) p l))
    | Error `MissingInputs ->
        Error `MissingInputs

(* --- *)

type 'a memory =
  | Amnesic: Cobol_common.Behaviors.amnesic memory
  | Eidetic: token list -> Cobol_common.Behaviors.eidetic memory

type 'm state =
  {
    lexer_state: Text_lexer.lexer_state;
    leftover_tokens: token list; (* non-empty only when [preproc_n_combine_tokens]
                                    errors out for lack of input tokens. *)
    memory: 'm memory;
    context_stack: Context.stack;
    registered_intrinsics: Text_lexer.IntrinsicHandles.t;
    intrinsics_enabled: bool;
    diags: Parser_diagnostics.t;
    persist: persist;
  }

(** Part of the state that (almost) never changes. *)
and persist =
  {
    lexer: Text_lexer.lexer;
    context_tokens: Grammar_contexts.context_tokens;
    known_intrinsics: Text_lexer.IntrinsicHandles.t;
    default_intrinsics: Text_lexer.IntrinsicHandles.t;
    exec_scanners: Parser_options.exec_scanners;
    verbose: bool;
    show_if_verbose: [`Tks | `Ctx] list;
  }

let amnesic = Amnesic
let eidetic = Eidetic []
let init
    ?(verbose = false)
    ?(show_if_verbose = [`Tks; `Ctx])
    ~exec_scanners
    ~memory
    ~intrinsics
    ?(default_intrinsics = StringSet.empty)       (* preregistered_intrinsics *)
    words
  =
  let lexer = Text_lexer.create () in
  let Grammar_contexts.{ context_tokens;
                         context_sensitive_tokens;
                         context_sensitive_tokens_unimplemented = _ } =
    Grammar_contexts.init
      ~handle_of_token:(Text_lexer.handle_of_token lexer)
  in
  Text_lexer.disable_keywords context_sensitive_tokens;
  Text_lexer.reserve_words lexer words;
  let known_intrinsics =
    Text_lexer.intrinsic_handles lexer (StringSet.elements intrinsics)
  in
  let default_intrinsics =
    Text_lexer.IntrinsicHandles.inter known_intrinsics @@
    Text_lexer.intrinsic_handles lexer (StringSet.elements default_intrinsics)
  in
  {
    lexer_state = Text_lexer.initial_state;
    leftover_tokens = [] ;
    memory;
    context_stack = Context.empty_stack;
    registered_intrinsics = default_intrinsics;
    intrinsics_enabled = false;
    diags = Parser_diagnostics.none;
    persist =
      {
        lexer;
        context_tokens;
        known_intrinsics;
        default_intrinsics;
        exec_scanners;
        verbose;
        show_if_verbose =
          (if LIST.mem `Tks show_if_verbose then [`Tks] else []) @
          (if LIST.mem `Ctx show_if_verbose then [`Ctx] else []);
      }
  }

let diagnostics { diags; _ } = diags
let parsed_tokens { memory = Eidetic tokens ; _ } = lazy ( LIST.rev tokens )

let show tag { persist = { verbose; show_if_verbose; _ }; _ } =
  verbose && LIST.mem tag show_if_verbose

(* --- *)

let distinguish_words: (Grammar_tokens.token with_loc as 't) -> 't = function
  | { payload = WORD w; loc } when loc_in_area_a loc ->
      WORD_IN_AREA_A w &@ loc
  | t -> t


let scan_exec_block
    ({ persist = { exec_scanners = { exec_scanners = scanners;
                                     exec_scanner_fallback = fallback; _ }; _ };
       _ } as state)
    text =
  let module EXEC_MAP = Parser_options.EXEC_MAP in
  let exec_scanner, lang = match ~&text with
    | { payload = TextWord _(* EXEC(UTE) *); _ } ::
      { payload = TextWord lang; _ } :: _ ->
        (try EXEC_MAP.find lang scanners, Some lang
         with Not_found -> fallback, None)
    | _ ->
        fallback, None
  in
  match exec_scanner with
  | Stateless_exec_scanner s ->
      let block, diags = s ~&text in
      let diags = Parser_diagnostics.add_exec_block_diags diags state.diags in
      EXEC_BLOCK block &@<- text,
      if diags == state.diags then state else { state with diags }
  | Stateful_exec_scanner (s, s_acc) ->
      let block, diags, s_acc = s ~&text s_acc in
      let scanner = Parser_options.Stateful_exec_scanner (s, s_acc) in
      let exec_scanners = match lang with
        | None ->
            { state.persist.exec_scanners with
              exec_scanner_fallback = scanner }
        | Some lang ->
            { state.persist.exec_scanners with
              exec_scanners = EXEC_MAP.add lang scanner scanners }
      in
      let diags = Parser_diagnostics.add_exec_block_diags diags state.diags in
      EXEC_BLOCK block &@<- text,
      { state with diags; persist = { state.persist with exec_scanners } }


let acc_tokens_of_text_word (rev_prefix_tokens, state) { payload = c; loc } =
  (* After text manipulation, a "word" of manipulated text may comprise some
     separators like `,' and `;', so they may actually encode multiple tokens.
     We additionally need special handling of `PICTURE [IS]` to bypass usual
     tokenization of picture strings. *)
  let generic ({ lexer_state; persist = { lexer; _ }; _ } as state) w =
    let tokens, lexer_state =
      Text_lexer.read_tokens lexer lexer_state (w &@ loc)
    in
    LIST.append ~loc:__LOC__ ( LIST.rev_map distinguish_words tokens ) rev_prefix_tokens,
    if lexer_state != state.lexer_state
    then { state with lexer_state }
    else state
  in
  let alphanum ~hexadecimal ?(repr = Cobol_ptree.Native_bytes) str qte =
    let quotation: Cobol_ptree.alphanum_quote = match qte with
      | Apostrophe -> Simple_quote
      | Quote -> Double_quote
    in
    Cobol_ptree.{ str; hexadecimal; quotation; runtime_repr = repr }
  and boollit ~base str =
    Cobol_ptree.boolean_of_string ~base str
  in
  let nominal state =
    let tok t = (t &@ loc) :: rev_prefix_tokens, state in
    match c with
    | TextWord w
    | CDirWord w ->
        generic state w
    (* | Alphanum { knd = Basic; str; qte = quotation; _ } *)
    (*   (\* TODO: Leave those as is in the parse-tree, and decode later *\) *)
    (*   when Config.ebcdic_symbolic_characters#value *)
    (*   -> let token, diags = *)
    (*        Text_lexer.decode_symbolic_ebcdics' ~quotation (str &@ loc) in *)
    (*     [token], diags *)
    | Alphanum { knd = Basic; str; qte; _ } ->
        tok @@ ALPHANUM (alphanum ~hexadecimal:false str qte)
    | Alphanum { knd = Bool; str; _ } ->
        tok @@ BOOLIT (boollit ~base:`Bool str)
    | Alphanum { knd = BoolX; str; _ } ->
        tok @@ BOOLIT (boollit ~base:`Hex str)
    | Alphanum { knd = Hex; str; qte }
    | Alphanum { knd = HexH; str; qte } ->
        tok @@ ALPHANUM (alphanum ~hexadecimal:true str qte)
    | Alphanum { knd = NullTerm; str; qte } ->
        tok @@ ALPHANUM (alphanum ~hexadecimal:false str qte
                           ~repr:Null_terminated_bytes)
    | Alphanum { knd = National | NationalX; str; _ } ->
        tok @@ NATLIT str                              (* TODO: differentiate *)
    | AlphanumPrefix { knd = Hex; str; qte }
    | AlphanumPrefix { knd = HexH; str; qte } ->
        tok @@ ALPHANUM_PREFIX (alphanum ~hexadecimal:true str qte)
    | AlphanumPrefix { knd = _; str; qte } ->
        tok @@ ALPHANUM_PREFIX (alphanum ~hexadecimal:false str qte)
    | Separator _ ->
        rev_prefix_tokens, state
    | Eof ->
        tok EOF
    | ExecBlock text ->
        let block, state = scan_exec_block state (text &@ loc) in
        block :: rev_prefix_tokens, state
    | Pseudo _ ->
        let error = Unexpected { loc; stuff = Pseudotext } in
        rev_prefix_tokens,
        { state with diags = Parser_diagnostics.add_error error state.diags }
  in
  let pic_string state =
    match c with
    | TextWord w ->
        generic state w
    | _ ->
        let lexer_state
          = Text_lexer.cancel_picture_string_expectation state.lexer_state in
        nominal { state with lexer_state }
  in
  if Text_lexer.expecting_picture_string state.lexer_state
  then pic_string state
  else nominal state

let new_stream toks = { toks ;
                     enabled_keywords = Text_lexer.TokenHandles.empty }

let new_tokens toks = {
  former_stream = new_stream toks ;
  new_stream = new_stream toks ;
}

let tokens_of_text: 'a state -> text -> tokens * 'a state = fun state text ->
  let tokens, state = LIST.fold_left acc_tokens_of_text_word ([], state) text in
  new_tokens ( LIST.rev tokens ), state

let remaining_tokens stream =
  stream.former_stream.toks

let tokenize_text ~source_format ({ leftover_tokens; _ } as state) text =
  let state = { state with leftover_tokens = [] } in
  let tokens, state = tokens_of_text state text in
  let tokens = LIST.append ~loc:__LOC__ leftover_tokens
      (remaining_tokens tokens) in
  let intrinsics_enabled = state.intrinsics_enabled in
  match preproc_n_combine_tokens ~intrinsics_enabled ~source_format tokens with
  | Ok (tokens, diags) ->
      if show `Tks state then
        Pretty.error "Tks: %a@." Grammar_tokens_printer.pp_tokens tokens;
      let diags = Parser_diagnostics.union diags state.diags in
      Ok ( new_tokens tokens ), { state with diags }
  | Error `MissingInputs ->
      Error `MissingInputs, { state with leftover_tokens = tokens }
  | Error (`ReachedEOF (loc, unterminated_item, diags, tokens)) ->
      Error (`ReachedEOF ( new_tokens tokens )),
      let error = Unterminated { loc; stuff = unterminated_item } in
      let diags = Parser_diagnostics.union diags state.diags in
      { state with diags = add_error error diags }

let emit_token (type m) (s: m state) tok : m state =
  match s.memory with
  | Amnesic -> s
  | Eidetic toks -> { s with memory = Eidetic ( tok :: toks ) }

let put_token_back (type m) (s: m state) : m state =
  match s.memory with
  | Amnesic -> s
  | Eidetic  [] -> Fmt.invalid_arg "put_token_back: unexpected memory state"
  | Eidetic  ( _ :: toks ) -> { s with memory = Eidetic toks }

let rec skip_redundant_periods = function
    | { payload = PERIOD; _ } :: rest -> skip_redundant_periods rest
    | rest -> rest

let retokenize { lexer_state; persist = { lexer; _ }; _ } w =
  (* Note: for now we can forget the lexer state that is returned on rescan of
     tokens, as this part of the state alters PICTURE_STRINGs, that we don't
     need to reconsider, or else is updated before [retokenize] is called
     (handling of DECIMAL POINT).  *)
  fst @@ Text_lexer.read_tokens lexer lexer_state w

let comma_becomes_decimal_point s toks =
  (* This may only happen when the comma becomes a decimal separator in
     numerical literals, instead of periods.  Before this (irreversible)
     change, any intervening comma is represented with a special
     [INTERVENING_ ','] token in the list of tokens procuded by
     {!tokenize_text}. *)
  (* Find any INTERVENING_COMMA and retokenize with the two adjacent words
     if they are SINTLIT on the left, and DIGITS (or FLOATLIT) on the
     right (possible combinations are generated in {!Text_lexer.token}).
     To deal with periods, we need to retokenize any FIXEDLIT and
     FLOATLIT. *)
  let show_fixed (i, c, d) = Pretty.to_string "%s%c%s" i c d in
  let show_float (i, c, d, e) = Pretty.to_string "%s%c%sE%s" i c d e in
  let rec aux rev_prefix suffix =
    match rev_prefix, suffix with
    | { payload = SINTLIT l; loc = lloc } :: rev_prefix,
      { payload = INTERVENING_ ','; loc = cloc } ::
      { payload = DIGITS r; loc = rloc } :: suffix ->
      retokenize_with_comma rev_prefix suffix
        l lloc cloc r rloc
    | { payload = SINTLIT l; loc = lloc } :: rev_prefix,
      { payload = INTERVENING_ ','; loc = cloc } ::
      { payload = FLOATLIT f; loc = rloc } :: suffix ->
      retokenize_with_comma rev_prefix suffix
        l lloc cloc (show_float f) rloc
    | _, { payload = FIXEDLIT f; loc } :: suffix ->
      let toks = retokenize s (show_fixed f &@ loc) in
      aux (LIST.rev_append toks rev_prefix) suffix
    | _, { payload = FLOATLIT f; loc } :: suffix ->
      let toks = retokenize s (show_float f &@ loc) in
      aux (LIST.rev_append toks rev_prefix) suffix
    | _, [] ->
      LIST.rev rev_prefix
    | _, x :: tl ->
      aux (x :: rev_prefix) tl
  and retokenize_with_comma rev_prefix suffix l l_loc sep_loc r r_loc =
    let loc = Cobol_common.Srcloc.(concat (concat l_loc sep_loc) r_loc) in
    let tks = retokenize s (Pretty.to_string "%s,%s" l r &@ loc) in
    aux (LIST.rev_append tks rev_prefix) suffix
  in
  aux [] toks

let reword_intrinsics s tokens =
  (* Some intrinsics NOT preceded with FUNCTION may now be words; assumes
     [Disabled_intrinsics] does not occur on a `FUNCTION` keyword (but that's
     unlikely). *)
  let keyword_of_token token = Hashtbl.find Text_lexer.word_of_token token in
  let is_intrinsic_token = function
    | INTRINSIC_FUNC _ -> true
    | t when Text_keywords.is_known_intrinsic_token t -> true
    | _ -> false in
  let rec aux rev_prefix suffix =
    match suffix with
    | [] ->
      LIST.rev rev_prefix
    | ({ payload = k1_token; _ } as k1) ::
      ({ payload = k2_token; _ } as k2) :: tl
      when k1_token <> FUNCTION && is_intrinsic_token k2_token ->
      aux (LIST.tail_map ~loc:__LOC__ distinguish_words @@
           retokenize s (keyword_of_token k2_token &@<- k2) @
           k1 :: rev_prefix) tl
    | k1 :: tl ->
      aux (k1 :: rev_prefix) tl
  in
  aux [] tokens

module type INTERFACE = sig

  val retokenize_after : lexer_update -> 'a state -> stream -> stream
  val next_token :
    'a state ->
    stream ->
    ('a state * Grammar_tokens.token with_loc * stream) option

  val put_back_token : token -> stream -> stream

end

module FORMER_IMPLEMENTATION : INTERFACE = struct

  (** Retokenizes the tokens {e after} the given operation has been perfomed on
      {!module:Text_lexer}. *)
  (* TODO: Find whether everything related to Area A and comma-retokenization
     could be moved to Text_lexer *)
  let retokenize_after = fun update s stream ->
    let toks = stream.toks in
    let toks =
      match update with
      | Enabled_keywords tokens
      | Disabled_keywords tokens
        when Text_lexer.TokenHandles.is_empty tokens ->
        toks
      | Enabled_keywords _
      | Enabled_intrinsics ->          (* <- some words may have become intrinsics *)
        LIST.concat_map ~loc:__LOC__ ( fun token -> match ~&token with
            | WORD_IN_AREA_A w
            | WORD w ->
              (* translate WORD in a area_a to WORD_IN_AREA_A *)
              LIST.tail_map ~loc:__LOC__ distinguish_words
              @@
              (* why would we need to retokenize this WORD differently ? *)
              retokenize s (w &@<- token)
            | _ ->
              [token]
          ) toks
      | Disabled_keywords tokens ->
        (*
        Printf.eprintf "Disabled_keywords:\n%!";
        Text_lexer.TokenHandles.iter (fun s ->
            Printf.eprintf "   * %S\n%!"
              (Text_lexer.string_of_keyword_handle s)) tokens;
           *)
        (* When removing these keywords, translate back any such keyword to
           the corresponding WORD/WORD_IN_AREA_A *)
        LIST.tail_map ~loc:__LOC__ ( fun token ->
            if Text_lexer.TokenHandles.mem_text_token ~&token tokens
            then
              let w = Hashtbl.find Text_lexer.word_of_token ~&token in
              if token_in_area_a token then WORD_IN_AREA_A w &@<- token
              else WORD w &@<- token
            else token
          ) toks
      | Disabled_intrinsics ->
        reword_intrinsics s toks
      | CommaBecomesDecimalPoint -> comma_becomes_decimal_point s toks
    in
    (* We don't store updates as we have already applied them ! *)
    {  toks = toks ; enabled_keywords = Text_lexer.TokenHandles.empty }

  let rec next_token (s: _ state) stream =
    match stream.toks with
    | { payload = INTERVENING_(',' | ';'); _ } :: tokens ->
      next_token s { stream with toks = tokens }
    | { payload = INTERVENING_ '.'; loc } as token :: tokens ->
      Some (emit_token s (PERIOD &@ loc), token, { stream with toks = tokens })
    | { payload = PERIOD; _ } as token :: tokens ->
      let tokens = skip_redundant_periods tokens in
      Some (emit_token s token, token, { stream with toks = tokens  } )
    | token :: tokens ->
      Some (emit_token s token, token, { stream with toks = tokens })
    | [] ->
      None

  let put_back_token token stream =
    { stream with toks = token :: stream.toks }

end

module NEW_IMPLEMENTATION : INTERFACE = struct

  (** Retokenizes the tokens {e after} the given operation has been perfomed on
      {!module:Text_lexer}. *)
  (* TODO: Find whether everything related to Area A and comma-retokenization
     could be moved to Text_lexer *)
  let retokenize_after = fun update s stream ->
      match update with
      | Enabled_keywords tokens ->
        let enabled_keywords = stream.enabled_keywords in
        let enabled_keywords =
          Text_lexer.TokenHandles.union_
            enabled_keywords ~normalize:tokens in
        (*
        Printf.eprintf "Newly Enabled_keywords:\n%!";
          Text_lexer.TokenHandles.iter (fun s ->
              Printf.eprintf "   * %S\n%!"
                (Text_lexer.string_of_keyword_handle s)) enabled_keywords;
           *)
        { stream with enabled_keywords }
      | Disabled_keywords tokens -> 
        let enabled_keywords = stream.enabled_keywords in
        (*
        if not @@ Text_lexer.TokenHandles.subset tokens enabled_keywords then begin
          Printf.eprintf "NEW Disabled_keywords:\n%!";
          Text_lexer.TokenHandles.iter (fun s ->
              Printf.eprintf "   * %S\n%!"
                (Text_lexer.string_of_keyword_handle s)) tokens;
          Printf.eprintf "Formerly Enabled_keywords:\n%!";
          Text_lexer.TokenHandles.iter (fun s ->
              Printf.eprintf "   * %S\n%!"
                (Text_lexer.string_of_keyword_handle s)) enabled_keywords;
        end; *)
        let enabled_keywords =
          Text_lexer.TokenHandles.diff enabled_keywords tokens in
        { stream with enabled_keywords }
      | _ ->
        FORMER_IMPLEMENTATION.retokenize_after update s stream

  let rec next_token (s: _ state) stream =
    match stream.toks with
    | [] -> None
    | token :: toks ->
      let token =
        match ~&token with
        | WORD_IN_AREA_A w
        | WORD w ->
          (* translate WORD in a area_a to WORD_IN_AREA_A *)
              LIST.tail_map ~loc:__LOC__ distinguish_words
              @@
              (* why would we need to retokenize this WORD differently ? *)
              retokenize
                { s with
                  lexer_state =
                    Text_lexer.cancel_picture_string_expectation s.lexer_state }
                (w &@<- token)
        | _ ->
          [token]
      in
      let token = match token with
        | [] -> assert false
        | [ token ] -> token
        | _ -> assert false
      in
      let loc = token.Cobol_common.Srcloc.loc in
      match token.payload with
      | INTERVENING_(',' | ';') -> next_token s { stream with toks }
      | INTERVENING_ '.' ->
        Some (emit_token s (PERIOD &@ loc), token, { stream with toks })
      | PERIOD ->
        let toks = skip_redundant_periods toks in
        Some (emit_token s token, token, { stream with toks  } )
      | _ ->
        Some (emit_token s token, token, { stream with toks })

  let put_back_token token stream =
    { stream with toks = token :: stream.toks }

end

module COMPARISON = struct
  let retokenize_after enable state { former_stream ; new_stream } =
    let former_stream = FORMER_IMPLEMENTATION.retokenize_after
        enable state former_stream in
    let new_stream = NEW_IMPLEMENTATION.retokenize_after
        enable state new_stream in
    { former_stream ; new_stream }

  let put_back_token token  { former_stream ; new_stream } =
    let former_stream = FORMER_IMPLEMENTATION.put_back_token token former_stream in
    let new_stream = NEW_IMPLEMENTATION.put_back_token token new_stream in
    { former_stream ; new_stream }

  let next_token state { former_stream ; new_stream } =
    let output1 = FORMER_IMPLEMENTATION.next_token state former_stream in
    let output2 = NEW_IMPLEMENTATION.next_token state new_stream in
    match output1 with
    | None -> None
    | Some (state1, token1, former_stream) ->
      match output2 with
      | None -> assert false
      | Some (state2, token2, new_stream) ->
        if token1.payload <> token2.payload then begin
          Printf.eprintf "former_stream returned %s\n%!"
            ( Text_lexer.string_of_token token1.payload);
          Printf.eprintf "new_stream returned %s\n%!"
            ( Text_lexer.string_of_token token2.payload);
          assert false
        end;
        (*
        Printf.eprintf "next_token returned %s\n%!"
          ( Text_lexer.string_of_token token1.payload);
           *)
        ignore (state1, state2);
        Some (state2, token2, { former_stream ; new_stream })

end

module ONE_IMPLEMENTATION = struct
  let retokenize_after enable state { former_stream ; new_stream } =
    let new_stream = NEW_IMPLEMENTATION.retokenize_after
        enable state new_stream in
    { former_stream ; new_stream }

  let put_back_token token  { former_stream ; new_stream } =
    let new_stream = NEW_IMPLEMENTATION.put_back_token token new_stream in
    { former_stream ; new_stream }

  let next_token state { former_stream ; new_stream } =
    let output2 = NEW_IMPLEMENTATION.next_token state new_stream in
    match output2 with
    | None -> None
    | Some (state, token2, new_stream) ->
        Some (state, token2, { former_stream ; new_stream })

end

include COMPARISON


(** Enable incoming tokens w.r.t the lexer, and retokenize awaiting tokens
    (i.e. that may have been tokenized according to out-of-date rules) *)
let enable_tokens
  : 'a state -> tokens -> Text_lexer.TokenHandles.t -> 'a state * tokens =
  fun state tokens incoming_tokens ->
  Text_lexer.enable_keywords incoming_tokens;
  state, retokenize_after (Enabled_keywords incoming_tokens) state tokens

(** Disable incoming tokens w.r.t the lexer, and retokenize awaiting tokens
    (i.e. that may have been tokenized according to out-of-date rules) *)
let disable_tokens state tokens outgoing_tokens =
  Text_lexer.disable_keywords outgoing_tokens;
  state, retokenize_after (Disabled_keywords outgoing_tokens) state tokens


let register_intrinsics { persist = { lexer; _ }; registered_intrinsics; _ } =
  Text_lexer.register_intrinsics lexer registered_intrinsics


let unregister_intrinsics { persist = { lexer; _ }; registered_intrinsics; _ } =
  Text_lexer.unregister_intrinsics lexer registered_intrinsics


let save_intrinsics state =
  unregister_intrinsics state

let restore_intrinsics ({ intrinsics_enabled; _ } as state) =
  if intrinsics_enabled
  then register_intrinsics state

let enable_intrinsics state token stream =
  if state.intrinsics_enabled then state, token, stream else        (* error? *)
    let state = put_token_back { state with intrinsics_enabled = true } in
    register_intrinsics state;
    let stream = put_back_token token stream in
    let stream = retokenize_after Enabled_intrinsics state stream in
    match next_token state stream with
    | None -> assert false
    | Some (state, token, stream) -> state, token, stream

let disable_intrinsics state token stream =
  if not state.intrinsics_enabled then state, token, stream else      (* error? *)
    let state = put_token_back { state with intrinsics_enabled = false } in
    unregister_intrinsics state;
    let stream = put_back_token token stream in
    let stream = retokenize_after Disabled_intrinsics state stream in
    match next_token state stream with
    | None -> assert false
    | Some (state, token, stream) -> state, token, stream

let reset_intrinsics state token tokens =
  let state, token, tokens = disable_intrinsics state token tokens in
  { state with registered_intrinsics = state.persist.default_intrinsics },
  token,
  tokens


(** Replaces the set of registered intrinsics.  Registers the default set if
    [intrinsics = None]. *)
let replace_intrinsics state intrinsics =
  assert (not state.intrinsics_enabled);
  unregister_intrinsics state;
  let registered_intrinsics = match intrinsics with
    | None ->
        state.persist.default_intrinsics
    | Some s ->
        Text_lexer.intrinsic_handles state.persist.lexer (LIST.map (~&) s)
  in
  { state with registered_intrinsics }


let decimal_point_is_comma (type m) (state: m state) token stream =
  let state = put_token_back state in
  let state =
    { state with
      lexer_state = Text_lexer.decimal_point_is_comma state.lexer_state }
  in
  let stream = put_back_token token stream in
  let stream = retokenize_after CommaBecomesDecimalPoint state stream in
  match next_token state stream with
  | None -> assert false
  | Some (state, token, stream) -> state, token, stream


let put_token_back state token stream =
  put_token_back state, put_back_token token stream

(* --- *)

let with_context_stack state context_stack =
  if context_stack == state.context_stack then state
  else { state with context_stack }

let push_contexts state tokens : Context.t list -> 's * 'a = function
  | [] ->
      state, tokens
  | contexts ->
    let context_stack, tokens_set =
        let context_tokens = state.persist.context_tokens in
        LIST.fold_left begin fun (stack, set) ctx ->
          if show `Ctx state then
            Pretty.error "Incoming: %a@."
              (Context.pp_context context_tokens) ctx;

          (* Push the new context on top of the stack *)
          let stack = Context.push context_tokens ctx stack in

          stack, Text_lexer.TokenHandles.union set @@ Context.top_tokens stack
        end (state.context_stack, Text_lexer.TokenHandles.empty) contexts
      in

      (* Update tokenizer state *)
      let state, tokens = enable_tokens state tokens tokens_set in
      if show `Tks state then
        Pretty.error "Tks': %a@." pp_tokens tokens;
      with_context_stack state context_stack, tokens

let top_context state =
  Context.top state.context_stack

let pop_context ({ context_stack; _ } as state) tokens =
  let context_stack, tokens_set = Context.pop context_stack in

  if show `Ctx state then
    Pretty.error "Outgoing: %a@." Text_lexer.pp_tokens_via_handles tokens_set;

  let state, tokens = disable_tokens state tokens tokens_set in
  { state with context_stack }, tokens

let enable_context_sensitive_tokens state =
  Text_lexer.enable_keywords (Context.all_tokens state.context_stack)

let disable_context_sensitive_tokens state =
  Text_lexer.disable_keywords (Context.all_tokens state.context_stack)
