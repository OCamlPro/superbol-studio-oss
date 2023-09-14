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

module TEXT = Cobol_preproc.Text
module DIAGS = Cobol_common.Diagnostics

open Cobol_common.Srcloc.INFIX
open Cobol_common.Srcloc.TYPES
open Cobol_preproc.Text.TYPES
open Grammar_tokens                              (* import token constructors *)

(* --- *)

type token = Grammar_tokens.token with_loc
type tokens = token list

let combined_tokens =
  (* /!\ WARNING: None of the constituents of combined tokens may be
     context-sensitive.

     Rationale: this would considerably complicate retokenization (which is
     necessary with the current solution to handle context-sensitive
     keywords) *)
  Hashtbl.of_seq @@ List.to_seq [
      ON_EXCEPTION, "ON_EXCEPTION";
      NOT_ON_EXCEPTION, "NOT_ON_EXCEPTION";
      ON_OVERFLOW, "ON_OVERFLOW";
      NOT_ON_OVERFLOW, "NOT_ON_OVERFLOW";
      ON_SIZE_ERROR, "ON_SIZE_ERROR";
      NOT_ON_SIZE_ERROR, "NOT_ON_SIZE_ERROR";
      INVALID_KEY, "INVALID_KEY";
      NOT_INVALID_KEY, "NOT_INVALID_KEY";
      AT_END, "AT_END";
      NOT_AT_END, "NOT_AT_END";
      AT_EOP, "AT_EOP";
      NOT_AT_EOP, "NOT_AT_EOP";
      WITH_DATA, "WITH_DATA";
      NO_DATA, "NO_DATA";
      IS_GLOBAL, "IS_GLOBAL";
      IS_EXTERNAL, "IS_EXTERNAL";
      IS_TYPEDEF, "IS_TYPEDEF";
      DATA_RECORD, "DATA_RECORD";
      DATA_RECORDS, "DATA_RECORDS";
      NEXT_PAGE, "NEXT_PAGE";
    ]

let pp_token: token Pretty.printer = fun ppf ->
  let string s = Pretty.string ppf s
  and print format = Pretty.print ppf format in
  fun t -> match ~&t with
    | WORD w -> print "WORD[%s]" w
    | WORD_IN_AREA_A w -> print "WORD_IN_AREA_A[%s]" w
    | PICTURE_STRING w -> print "PICTURE_STRING[%s]" w
    | AUTHOR s -> print "AUTHOR[%s]" s
    | DIGITS i -> print "DIGITS[%s]" i
    | SINTLIT i -> print "SINT[%s]" i
    | EIGHTY_EIGHT -> string "88"
    | FIXEDLIT (i, sep, d) -> print "FIXED[%s%c%s]" i sep d
    | FLOATLIT (i, sep, d, e) -> print "FLOAT[%s%c%sE%s]" i sep d e
    | ALPHANUM (s, q) -> print "%a%s%a" TEXT.pp_quote q s TEXT.pp_quote q
    | ALPHANUM_PREFIX (s, q) -> print "%a%s" TEXT.pp_quote q s
    | NATLIT s -> print "N\"%s\"" s
    | BOOLIT b -> print "B\"%a\"" Cobol_ast.pp_boolean b
    | HEXLIT s -> print "X\"%s\"" s
    | NULLIT s -> print "Z\"%s\"" s
    | INTERVENING_ c -> print "<%c>" c
    | EOF -> string "EOF"
    | t -> string @@
        try Text_lexer.show_token t
        with Not_found ->
        try Hashtbl.find combined_tokens t
        with Not_found -> "<unknown token>"

let pp_tokens = Pretty.list ~fopen:"@[" ~fclose:"@]" pp_token

(* --- *)

let loc_in_area_a: srcloc -> bool = Cobol_common.Srcloc.in_area_a
let token_in_area_a: token -> bool = fun t -> loc_in_area_a ~@t

(* --- *)

(* Tokenization of manipulated text, to feed the compilation group parser: *)

let preproc_n_combine_tokens (module Config: Cobol_config.T) ~source_format =
  (* Simplifies the grammar. *)
  let ( +@+ ) = Cobol_common.Srcloc.concat
  and start_pos = Cobol_common.Srcloc.start_pos
  and comment_entry_termination
    = Cobol_preproc.Src_lexing.comment_entry_termination source_format in
  let open List in
  let rec skip ((p', l', dgs) as acc) ((p, l) as pl) = function
    | 0 -> acc, pl
    | i -> skip (hd p :: p', hd l :: l', dgs) (tl p, tl l) (i - 1)
  and aux acc (p, l) =
    let subst_n x y =
      let rec cons x ((p', l', dgs) as _acc) (p, l) = function
        | 0 -> assert false
        | 1 -> aux (x :: p', hd l :: l', dgs) (tl p, tl l)
        | i -> cons x acc (tl p, hd l +@+ hd (tl l) :: tl (tl l)) (i - 1)
      in
      cons x acc (p, l) y
    and word_after n =
      let word = function
        | WORD _ as t -> t
        | t -> try WORD (Hashtbl.find Text_lexer.keyword_of_token t) with
          | Not_found -> t
      in
      let (p', l', dgs), (p, l) = skip acc (p, l) n in
      match p with
      | [] -> Result.Error `MissingInputs
      | t :: _ -> aux (word t :: p', hd l :: l', dgs) (tl p, tl l)  (* XXX: +@+ ? *)
    and lex_err msg =
      Pretty.delayed_to begin fun dmsg ->
        let (p', l', diags), pl = skip acc (p, l) 1 in
        aux (p', l', DIAGS.Acc.error diags ~loc:(hd l) "%t" dmsg) pl
      end msg
    and comment_paragraph t =
      let subst_comment_entry = match comment_entry_termination with
        | Newline ->
            subst_comment_line ~init_pos:(Cobol_common.Srcloc.start_pos @@ hd l)
        | Period ->
            subst_comment_entry ?stop_column:None
        | AreaB { first_area_b_column } ->
            subst_comment_entry ~stop_column:first_area_b_column
      and at_end ~loc ((p', l', diags) as acc) =
        match Config.comment_paragraphs#verify ~loc:(Some loc) with
        | Ok ((), None)   | Error  None    -> acc
        | Ok ((), Some d) | Error (Some d) -> p', l', DIAGS.Set.cons d diags
      in
      subst_comment_entry ~loc:(hd l) ~at_end
        ("comment@ paragraph": Pretty.simple) t acc (tl (tl p), tl (tl l))
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

    | [IS]                           -> Error `MissingInputs
    | IS :: GLOBAL :: _                -> subst_n IS_GLOBAL 2
    | IS :: EXTERNAL :: _              -> subst_n IS_EXTERNAL 2
    | IS :: TYPEDEF :: _               -> subst_n IS_TYPEDEF 2

    | [DATA]                         -> Error `MissingInputs
    | DATA :: RECORD :: _              -> subst_n DATA_RECORD 2
    | DATA :: RECORDS :: _             -> subst_n DATA_RECORDS 2

    | [NEXT]                         -> Error `MissingInputs
    | NEXT :: PAGE :: _                -> subst_n NEXT_PAGE 2

    | PROGRAM_ID :: PERIOD :: _        -> word_after 2

    | (AUTHOR _ |
       INSTALLATION _ |
       DATE_WRITTEN _ |
       DATE_MODIFIED _ |
       DATE_COMPILED _ |
       REMARKS _ |
       SECURITY _) as t :: PERIOD :: _ -> comment_paragraph t

    | ALPHANUM_PREFIX (s, _) :: _     -> lex_err "Missing continuation of `%s'" s

    | tok :: _                        -> subst_n tok 1

    | []                             -> Ok acc
  and subst_comment_entry ?stop_column ~loc ~at_end descr x acc = function
    | [], _ ->
        Result.Error `MissingInputs (* no word starting in Area A, or not period yet *)
    | EOF :: _ as p, l ->
        let _, _, diags = at_end ~loc acc in   (* ignore all tokens until EOF *)
        Error (`ReachedEOF (loc, descr, diags, p, l))
    | PERIOD :: p, period_loc :: l
      when Option.is_none stop_column ->
        aux (at_end ~loc:(loc +@+ period_loc) acc) (p, l)
    | p, (p_loc :: _ as l)
      when (let Lexing.{ pos_bol; pos_cnum; _ } = start_pos p_loc in
            Option.fold stop_column
              ~some:(fun col -> pos_cnum - pos_bol < col) ~none:false) ->
        aux (at_end ~loc acc) (p, l)
    | _ :: tlp, l ->
        subst_comment_entry ?stop_column ~loc:(loc +@+ hd l) ~at_end
          descr x acc (tlp, tl l)
  and subst_comment_line ~init_pos ~loc ~at_end descr x acc = function
    | [], _ ->
        Result.Error `MissingInputs  (* found no word starting on anther line *)
    | p, (p_loc :: _ as l)
      when (let Lexing.{ pos_fname; pos_bol; _ } = start_pos p_loc in
            pos_bol > init_pos.Lexing.pos_bol ||
            pos_fname <> init_pos.pos_fname) ->
        aux (at_end ~loc acc) (p, l)
    | _ :: tlp, l ->
        subst_comment_line ~init_pos ~loc:(loc +@+ hd l) ~at_end
          descr x acc (tlp, tl l)
  in
  fun tokens ->
    let p, srclocs = split @@ map (~&@) tokens in
    match aux ([], [], DIAGS.Set.none) (p, srclocs) with
    | Ok (p, l, dgs) ->
        Ok (rev_map2 (&@) p l, dgs)
    | Error (`ReachedEOF (loc, descr, dgs, p, l)) ->
        Error (`ReachedEOF (loc, descr, dgs, rev_map2 (&@) p l))
    | Error `MissingInputs ->
        Error `MissingInputs

(* --- *)

module Make (Config: Cobol_config.T) = struct

  let init_text_lexer ~context_sensitive_tokens =
    Text_lexer.disable_tokens context_sensitive_tokens;
    Text_lexer.reserve_words Config.words

  type 'a memory =
    | Amnesic: Cobol_common.Behaviors.amnesic memory
    | Eidetic: tokens -> Cobol_common.Behaviors.eidetic memory

  type 'a state =
    {
      expect_picture_string: bool;
      leftover_tokens: tokens; (* non-empty only when [preproc_n_combine_tokens]
                                  errors out for lack of input tokens. *)
      memory: 'a memory;
      diags: DIAGS.Set.t;
      lexing_options: Text_lexer.lexing_options;
    }

  let amnesic = Amnesic
  let eidetic = Eidetic []
  let init memory ~context_sensitive_tokens =
    init_text_lexer ~context_sensitive_tokens;
    {
      expect_picture_string = false;
      leftover_tokens = [];
      memory;
      diags = DIAGS.Set.none;
      lexing_options = Text_lexer.default_lexing_options;
    }

  let diagnostics { diags; _ } = diags
  let parsed_tokens { memory = Eidetic tokens; _ } = lazy (List.rev tokens)

  let distinguish_words: (Grammar_tokens.token with_loc as 't) -> 't = function
    | { payload = WORD w; loc } when loc_in_area_a loc ->
        WORD_IN_AREA_A w &@ loc
    | t -> t

  let tokens_of_word
      { lexing_options = options; _ }
    : text_word with_loc -> tokens * DIAGS.Set.t =
    fun { payload = c; loc } ->
    let tok t = [t &@ loc], DIAGS.Set.none in
    match c with
    | TextWord w
    | CDirWord w
      -> let tokens = Text_lexer.tokens_of_string' ~options (w &@ loc) in
        List.map distinguish_words tokens, DIAGS.Set.none
    | Alphanum { knd = Basic; str; qte = quotation; _ }
      when Config.ebcdic_symbolic_characters#value
      -> let token, diags =
           Text_lexer.decode_symbolic_ebcdics' ~quotation (str &@ loc) in
        [token], diags
    | Alphanum { knd = Basic; str; qte; _ }
      -> tok @@ ALPHANUM (str, qte)
    | Alphanum { knd = Bool; str; _ }
      -> tok @@ BOOLIT (Cobol_ast.boolean_of_string ~base:`Bool str)
    | Alphanum { knd = BoolX; str; _ }
      -> tok @@ BOOLIT (Cobol_ast.boolean_of_string ~base:`Hex str)
    | Alphanum { knd = Hex; str; _ }
      -> tok @@ HEXLIT str                 (* TODO: decide on a representation *)
    | Alphanum { knd = NullTerm; str; _ }
      -> tok @@ NULLIT str
    | Alphanum { knd = National | NationalX; str; _ }  (* TODO: differentiate *)
      -> tok @@ NATLIT str
    | AlphanumPrefix { str; qte; _ }
      -> tok @@ ALPHANUM_PREFIX (str, qte)
    | Eof
      -> tok EOF
    | Pseudo _
      -> [], DIAGS.(Acc.error Set.none) ~loc "Unexpected@ pseudotext"

  let tokens_of_text: 'a state -> text -> tokens * 'a state = fun state ->
    (* After text manipulation. We need special handling of `PICTURE [IS]` to
       bypass usual tokenization of picture strings. *)
    let prod (acc, ({ expect_picture_string; _ } as s)) = function
      | { payload = PICTURE; _ } as p ->
          p :: acc, { s with expect_picture_string = true }
      | { payload = IS; _ } as p
        when expect_picture_string ->
          p :: acc, { s with expect_picture_string = true }
      | p ->
          p :: acc, { s with expect_picture_string = false }
    in
    let tokenize_text_word: string with_loc -> _ =
      let tokenizer ~loc lb =
        Text_lexer.tokens ~options:state.lexing_options (lb &@ Lazy.force loc)
      and prod_tokens t acc =
        List.fold_left (fun acc t -> prod acc @@ distinguish_words t) acc ~&t
      in
      fun w ->
        Cobol_common.Tokenizing.fold_tokens ~tokenizer ~f:prod_tokens w
          ~until:(function [{ payload = EOF; _ }] -> true | _ -> false)
    and prod_word (acc, ({ diags; _ } as state)) word =
      let t, diags' = tokens_of_word state word in
      let state = { state with diags = DIAGS.Set.union diags diags' } in
      List.fold_left prod (acc, state) t
    in
    let rec acc_text ((_, ({ expect_picture_string; _ })) as acc) word =
      if expect_picture_string
      then match ~&word with
        | TextWord "IS" -> prod acc (IS &@<- word)
        | TextWord w -> prod acc (PICTURE_STRING w &@<- word)
        | _ -> missing_picstr acc word
      else match ~&word with
        | TextWord w -> tokenize_text_word (w &@<- word) acc
        | _ -> prod_word acc word
    and missing_picstr (acc, ({ diags; _ } as state)) ({ loc; _ } as word) =
      let state =
        { state with
          diags = DIAGS.Acc.error diags ~loc "Missing@ PICTURE@ string@ (got@ \
                                              `%a'@ instead)" TEXT.pp_word word;
          expect_picture_string = false }
      in
      acc_text (acc, state) word
    in
    fun text ->
      let acc, state = List.fold_left acc_text ([], state) text in
      List.rev acc, state

  let tokenize_text ~source_format ({ leftover_tokens; _ } as state) text =
    let state = { state with leftover_tokens = [] } in
    let new_tokens, state = tokens_of_text state text in
    let tokens = leftover_tokens @ new_tokens in
    match preproc_n_combine_tokens (module Config) ~source_format tokens with
    | Ok (tokens, diags) ->
        Ok tokens, { state with diags = DIAGS.Set.union diags state.diags }
    | Error `MissingInputs ->
        Error `MissingInputs, { state with leftover_tokens = tokens }
    | Error (`ReachedEOF (loc, extected_item, diags, tokens)) ->
        Error (`ReachedEOF tokens),
        let diags = DIAGS.Set.union diags state.diags in
        { state with
          diags = DIAGS.Acc.error diags ~loc "Unterminated %(%)" extected_item }

  let emit_token (type m) (s: m state) tok : m state =
    match s.memory with
    | Amnesic -> s
    | Eidetic toks -> { s with memory = Eidetic (tok :: toks) }

  let put_token_back (type m) (s: m state) : m state =
    match s.memory with
    | Amnesic -> s
    | Eidetic [] -> Fmt.invalid_arg "put_token_back: unexpected memory state"
    | Eidetic (_ :: toks) -> { s with memory = Eidetic toks }

  let next_token (type m) (s: m state) =
    let rec aux = function
      | { payload = INTERVENING_ ','; _ } :: tokens ->
          aux tokens
      | { payload = INTERVENING_ '.'; loc } :: tokens ->
          Some (s, PERIOD &@ loc, tokens)
      | token :: tokens ->
          Some (emit_token s token, token, tokens)
      | [] ->
          None
    in
    aux

  type lexer_update =
    | Enabled of Text_lexer.TokenHandles.t
    | Disabled of Text_lexer.TokenHandles.t
    | CommaBecomesDecimalPoint

  let token_of_string { lexing_options = options; _ } =
    Text_lexer.token_of_string' ~options

  let tokens_of_string' { lexing_options = options; _ } =
    Text_lexer.tokens_of_string' ~options

  (** Retokenizes the tokens {e after} the given operation has been perfomed on
      {!module:Text_lexer}. *)
  (* TODO: Find whether everything related to Area A and comma-retokenization
     could be moved to Text_lexer *)
  let retokenize_after: lexer_update -> _ state -> tokens -> tokens = fun update s ->
    match update with
    | Enabled tokens | Disabled tokens
      when Text_lexer.TokenHandles.is_empty tokens ->
        Fun.id
    | Enabled _ ->
        List.map begin fun token -> match ~&token with
          | WORD_IN_AREA_A w
          | WORD w -> distinguish_words @@ token_of_string s (w &@<- token)
          | _ -> token
        end
    | Disabled tokens ->
        let keyword_of_token = Hashtbl.find Text_lexer.keyword_of_token in
        List.map begin fun token ->
          if Text_lexer.TokenHandles.mem_text_token ~&token tokens
          then match token_in_area_a token, keyword_of_token ~&token with
            | true, w -> WORD_IN_AREA_A w &@<- token
            | false, w -> WORD w &@<- token
          else token
        end
    | CommaBecomesDecimalPoint ->
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
              let toks = tokens_of_string' s (show_fixed f &@ loc) in
              aux (List.rev_append toks rev_prefix) suffix
          | _, { payload = FLOATLIT f; loc } :: suffix ->
              let toks = tokens_of_string' s (show_float f &@ loc) in
              aux (List.rev_append toks rev_prefix) suffix
          | _, [] ->
              List.rev rev_prefix
          | _, x :: tl ->
              aux (x :: rev_prefix) tl
        and retokenize_with_comma rev_prefix suffix l l_loc sep_loc r r_loc =
          let loc = Cobol_common.Srcloc.(concat (concat l_loc sep_loc) r_loc) in
          let tks = tokens_of_string' s (Pretty.to_string "%s,%s" l r &@ loc) in
          aux (List.rev_append tks rev_prefix) suffix
        in
        aux []

  (** Enable incoming tokens w.r.t the lexer, and retokenize awaiting tokens
      (i.e. that may have been tokenized according to out-of-date rules) *)
  let enable_tokens state tokens incoming_tokens =
    Text_lexer.enable_tokens incoming_tokens;
    state, retokenize_after (Enabled incoming_tokens) state tokens

  (** Disable incoming tokens w.r.t the lexer, and retokenize awaiting tokens
      (i.e. that may have been tokenized according to out-of-date rules) *)
  let disable_tokens state tokens outgoing_tokens =
    Text_lexer.disable_tokens outgoing_tokens;
    state, retokenize_after (Disabled outgoing_tokens) state tokens

  let decimal_point_is_comma (type m) (state: m state) token tokens =
    let state = put_token_back state in
    let state = { state with
                  lexing_options = { decimal_point_is_comma = true } } in
    let tokens = token :: tokens in
    let tokens = retokenize_after CommaBecomesDecimalPoint state tokens in
    let token, tokens = List.hd tokens, List.tl tokens in
    emit_token state token, token, tokens

  let put_token_back state token tokens =
    put_token_back state, token :: tokens

end
