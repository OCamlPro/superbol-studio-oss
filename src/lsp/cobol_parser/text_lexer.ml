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

open Cobol_common.Srcloc.INFIX
module LIST = Cobol_common.Basics.LIST

module TYPES = struct

  type optional_keyword =
    {
      token: Grammar_tokens.token;
      mutable reserved: bool;
      mutable enabled: bool;
    }
  and keyword_handle = optional_keyword

  type intrinsic_handle =
    {
      (* `intrinsic_handle` stores the actual token; this is an optional_keyword
         so we can use it to override "regular" keywords in `token_of_word` to
         only ever perform single lookups while giving precedence to registered
         intrinsics when they are enabled. *)
      intrinsic_handle: optional_keyword;
      intrinsic_word: string;                                 (* in uppercase *)
      mutable intrinsic_shadowing: optional_keyword option;   (* keyword before
                                                                 registration *)
    }

  type lexer =
    {
      token_of_word: (string, keyword_handle) Hashtbl.t;
      handle_of_intrinsic: (string, intrinsic_handle) Hashtbl.t;
    }

  (** Functional state for the lexer *)
  type pic_state =
    | Pic_unexpected
    | Pic_expected
    | Pic_awaiting_digits

  type lexer_state =
    {
      pic_state: pic_state;
      decimal_point_is_comma: bool;
    }

end
include TYPES

module TokenHandles = struct
  include Set.Make
      (struct
        type t = keyword_handle
        let compare t1 t2 = Stdlib.compare t1.token t2.token
      end)
  let mem_text_token token set =
    mem { token; enabled = false; reserved = false } set
end

module IntrinsicHandles =
  Set.Make (struct
    type t = intrinsic_handle
    let compare t1 t2 =
      Stdlib.compare t1.intrinsic_handle.token t2.intrinsic_handle.token
  end)

(* --- *)

let token_of_punct = Hashtbl.create 15
let punct_of_token = Hashtbl.create 15
let word_of_token = Hashtbl.create 257
let __token_of_word = Hashtbl.create 257       (* copied in `create` below *)
let __token_of_intrinsic = Hashtbl.create 116  (* all intrinsic function name *)

let string_of_word_token t =
  Hashtbl.find word_of_token t

(** Returns [None] if the token is neither a keyword nor a punctuation. *)
let string_of_punct_or_keyword_token t =
  match Hashtbl.find_opt word_of_token t with
  | Some s -> Some s
  | None -> Hashtbl.find_opt punct_of_token t

let string_of_token t =
  match string_of_punct_or_keyword_token t with
  | Some s -> s
  | None -> Grammar_printer.print_token t                       (* last resort *)

let token_of_handle h = h.token

(** Never raises {!Not_found} (by construction of keyword handles). *)
let string_of_keyword_handle h =
  string_of_token @@ token_of_handle h

let pp_tokens_via_handles ppf toks =
  Pretty.list ~fopen:"{@[" ~fclose:"@]}" ~fempty:"{}" begin fun ppf h ->
    Pretty.string ppf (string_of_keyword_handle h)
  end ppf (TokenHandles.elements toks)

let reserve_as_keyword   h = h.reserved <- true
let unreserve_keyword    h = h.reserved <- false
let enable_keyword       h = h.enabled <- true
let disable_keyword      h = h.enabled <- false

let enable_keywords tokens =
  TokenHandles.iter enable_keyword tokens

let disable_keywords tokens =
  TokenHandles.iter disable_keyword tokens

let register_intrinsic { token_of_word; _ } h =
  if h.intrinsic_shadowing <> None
  then Pretty.error "@[<2>>> Internal@ warning@ in@ `%s.register_intrinsic`:@ \
                     registration@ of@ intrinsic@ function@ `%s',@ which@ was@ \
                     already@ registered@]@." __MODULE__ h.intrinsic_word
  else h.intrinsic_shadowing <- Hashtbl.find_opt token_of_word h.intrinsic_word;
  Hashtbl.replace token_of_word h.intrinsic_word h.intrinsic_handle

let unregister_intrinsic { token_of_word; _ } h =
  match h.intrinsic_shadowing with
  | None ->
      Hashtbl.remove token_of_word h.intrinsic_word
  | Some h' ->
      Hashtbl.replace token_of_word h.intrinsic_word h';
      h.intrinsic_shadowing <- None

let register_intrinsics lexer : IntrinsicHandles.t -> unit = fun ih ->
  IntrinsicHandles.iter (register_intrinsic lexer) ih

let unregister_intrinsics lexer : IntrinsicHandles.t -> unit = fun ih ->
  IntrinsicHandles.iter (unregister_intrinsic lexer) ih

(* "Static", internal structures (possibly copied in actual lexers) *)

let __init_puncts =
  List.iter begin fun (punct, token) ->
    Hashtbl.add punct_of_token token punct;
    Hashtbl.add token_of_punct punct token
  end Text_keywords.puncts

let __init_default_keywords =
  List.iter begin fun (kwd, token) ->
    Hashtbl.add word_of_token token kwd;
    (* Every default token needs to be reserved explicitly *)
    Hashtbl.add __token_of_word kwd
      { token; enabled = true; reserved = false }
  end Text_keywords.keywords

let __init_default_intrinsics =
  List.iter begin fun (kwd, token) ->
    Hashtbl.add word_of_token token kwd;
    Hashtbl.add __token_of_intrinsic kwd
      { intrinsic_word = String.uppercase_ascii kwd;
        intrinsic_handle = { token; enabled = true; reserved = true };
        intrinsic_shadowing = None }
  end Text_keywords.intrinsic_functions

let silenced_keywords =
  StringSet.of_list Text_keywords.silenced_keywords

(* --- *)

let copy_optional_keyword h =
  { token = h.token;
    enabled = h.enabled;
    reserved = h.reserved }
let copy_keyword_handle =
  copy_optional_keyword
let copy_intrinsic_handle h =
  { intrinsic_handle = copy_optional_keyword h.intrinsic_handle;
    intrinsic_word = h.intrinsic_word;
    intrinsic_shadowing = h.intrinsic_shadowing }

let create () =
  let token_of_word = Hashtbl.copy __token_of_word in
  Hashtbl.filter_map_inplace
    (fun _ h -> Some (copy_keyword_handle h)) token_of_word;
  let handle_of_intrinsic = Hashtbl.copy __token_of_intrinsic in
  Hashtbl.filter_map_inplace
    (fun _ h -> Some (copy_intrinsic_handle h)) handle_of_intrinsic;
  {
    token_of_word;
    handle_of_intrinsic;
  }

let handle_of_word { token_of_word; _ } kwd =
  Hashtbl.find token_of_word kwd

let handle_of_token { token_of_word; _ } token =
  Hashtbl.find token_of_word (Hashtbl.find word_of_token token)

let reserve_insensitive_token { token_of_word; _ } kwd token_handle =
  Hashtbl.add token_of_word kwd
    { token_handle with enabled = true; reserved = true }

let reserve_sensitive_alias { token_of_word; _ } kwd token_handle =
  Hashtbl.add token_of_word kwd token_handle

let reserve_words lexer : Cobol_config.words_spec -> unit =
  let on_token_handle_of kwd descr ~f =
    try f @@ handle_of_word lexer kwd with
    | Not_found when StringSet.mem kwd silenced_keywords ->
        ()                                          (* Ignore silently? Warn? *)
    | Not_found ->
        Pretty.error "@[Unable@ to@ %s@ keyword:@ %s@]@." descr kwd
  in
  List.iter begin fun (w, word_spec) ->
    match word_spec with
    | Cobol_config.ReserveWord { preserve_context_sensitivity } ->
        on_token_handle_of w "reserve" ~f:begin fun h ->
          if preserve_context_sensitivity
          then reserve_as_keyword h
          else reserve_insensitive_token lexer w h
        end
    | ReserveAlias { alias_for; preserve_context_sensitivity } ->
        on_token_handle_of alias_for "alias" ~f:begin fun h ->
          if preserve_context_sensitivity
          then reserve_sensitive_alias lexer w h
          else reserve_insensitive_token lexer w h
        end
    | NotReserved ->
        on_token_handle_of w "unreserve" ~f:unreserve_keyword
  end

(* --- *)

let intrinsic_handles lexer : string list -> IntrinsicHandles.t = fun intrinsics ->
  List.fold_left begin fun acc w ->
    try
      let ih =
        Hashtbl.find lexer.handle_of_intrinsic @@ String.uppercase_ascii w in
      IntrinsicHandles.add ih acc
    with Not_found ->                            (* TODO: symbolic diagnostics *)
      Pretty.error "@[Unknown@ intrinsic:@ %s@]@." w; acc
  end IntrinsicHandles.empty intrinsics

(* --- *)

let initial_state =
  {
    pic_state = Pic_unexpected;
    decimal_point_is_comma = false;
  }

let expecting_picture_string = function
  | { pic_state = Pic_expected | Pic_awaiting_digits; _ } -> true
  | _ -> false

let cancel_picture_string_expectation ({ pic_state; _ } as state) =
  if pic_state = Pic_unexpected
  then state
  else { state with pic_state = Pic_unexpected }

let decimal_point_is_comma state =
  { state with decimal_point_is_comma = true }

let token_of_word { token_of_word; _ } s =
  match Hashtbl.find token_of_word (String.uppercase_ascii s) with
  | { token; enabled = true; reserved = true; _ } -> token
  | _ -> raise Not_found

let token_of_intrinsic s =
  match Hashtbl.find __token_of_intrinsic (String.uppercase_ascii s) with
  | { intrinsic_handle = { token; _ }; _ } -> token

let read_tokens lexer state w =
  let split_loc loc len =
    Cobol_common.Srcloc.prefix len loc,
    Cobol_common.Srcloc.trunc_prefix len loc
  in
  let lexbuf = Lexing.from_string ~&w in
  let rec aux ~loc ~pic_state acc =
    let open Grammar_tokens in
    let start_pos = lexbuf.Lexing.lex_curr_pos in
    let append ~pic_state x =
      let pic_state = if x = PICTURE then Pic_expected else pic_state in
      let xloc, loc = split_loc loc @@ lexbuf.Lexing.lex_curr_pos - start_pos in
      aux ~loc ~pic_state ((x &@ xloc) :: acc)
    in
    let[@local] decimal_sep w c d e =
      let head, head_len, loc =
        if w = "" then [], 0, loc else
          let wlen = String.length w in
          let wloc, loc = split_loc loc wlen in
          [(if w.[0] <> '-' then DIGITS w else SINTLIT w) &@ wloc], wlen, loc
      and tail =
        match e with
        | None -> DIGITS d
        | Some e -> FLOATLIT (w, c, d, e)
      in
      let sloc, loc = split_loc loc 1 in
      let tail_len = lexbuf.Lexing.lex_curr_pos - start_pos - 1 - head_len in
      let tail_loc, loc = split_loc loc tail_len in
      aux ~loc ~pic_state @@
      LIST.append ~loc:__LOC__ [tail &@ tail_loc; INTERVENING_ c &@ sloc] @@
      LIST.append ~loc:__LOC__ head acc
    in
    let[@local] nominal ~pic_state' =
      let append = append ~pic_state:pic_state' in
      match Text_categorizer.token lexbuf with
      | End ->
          List.rev acc, { state with pic_state }
      | Word s ->
          append (try token_of_word lexer s with Not_found -> WORD s)
      | Punctuation s ->
          append (Hashtbl.find token_of_punct s)
      | Digits "88" ->
          append EIGHTY_EIGHT
      | Digits w ->
          append (DIGITS w)
      | Numeric (w, None) ->
          append (SINTLIT w)
      | Numeric (n, Some (sep, d, None))
        when sep = if state.decimal_point_is_comma then ',' else '.' ->
          append (FIXEDLIT (n, sep, d))
      | Numeric (n, Some (sep, d, Some e))
        when sep = if state.decimal_point_is_comma then ',' else '.' ->
          append (FLOATLIT (n, sep, d, e))
      | Numeric (w, Some (c, d, e)) ->
          decimal_sep w c d e
      | Unexpected (',' | ';') ->
          aux ~loc:(Cobol_common.Srcloc.trunc_prefix 1 loc) acc ~pic_state
      | Unexpected c ->
          append (INTERVENING_ c)
    in
    match pic_state with
    | Pic_unexpected ->
        nominal ~pic_state':pic_state
    | Pic_awaiting_digits ->
        nominal ~pic_state':Pic_expected
    | Pic_expected ->
        match Text_categorizer.pic_token lexbuf with
        | PIC_end ->
            List.rev acc, { state with pic_state }
        | PIC_is ->
            append IS ~pic_state
        | PIC_string w ->
            append (PICTURE_STRING w) ~pic_state:Pic_unexpected
        | PIC_suffix w ->
            append (PICTURE_SUFFIX w) ~pic_state:Pic_unexpected
        | PIC_prefix w ->
            append (PICTURE_PREFIX w) ~pic_state:Pic_awaiting_digits
        | PIC_interm w ->
            append (PICTURE_INTERM w) ~pic_state:Pic_awaiting_digits
        | PIC_unexpected (',' | ';') ->
            aux ~loc:(Cobol_common.Srcloc.trunc_prefix 1 loc) acc ~pic_state
        | PIC_unexpected c ->
            append (INTERVENING_ c) ~pic_state
  in
  aux ~loc:~@w [] ~pic_state:state.pic_state


(* --- Symbolic EBCDICs --- *)

let ebcdic_char i =
  (* TODO: (fixed/configurable tables) *)
  String.make 1 (Char.chr @@ Ebcdic.default.to_ascii.(i))

let decode_symbolic_ebcdics' ~quotation w =
  let open Text_categorizer in
  let open Parser_diagnostics_types in
  let acc_error e (acc, diags) = acc, e :: diags in
  let symbolic_ebcdic ~loc:_ = symbolic_ebcdic
  and alphanum_string ~loc:_ = alphanum_string in
  let str, diags =
    Cobol_common.Tokenizing.fold_tokens w ("", [])
      ~tokenizer:alphanum_string
      ~until:(function AEnd _ -> true | _ -> false)
      ~next_tokenizer:(function
          | AEBCDIC _
          | AStr (_, EBCDIC) | AUnexpected (_, EBCDIC) -> symbolic_ebcdic
          | AStr (_, STR) | AUnexpected (_, STR) | AEnd _ -> alphanum_string)
      ~f:begin fun t -> match ~&t with      (* TODO: (fixed/configurable tables) *)
        | AStr (s, _) ->
            fun (acc, diags) -> acc ^ s, diags
        | AEBCDIC i when i < 1 || i > 256 ->
            acc_error @@ Unexpected { loc = ~@t;
                                      stuff = Symbolic_EBCDIC_orginal i }
        | AEBCDIC i ->
            fun (acc, diags) -> acc ^ ebcdic_char i, diags
        | AEnd { wellformed = true } ->
            Fun.id
        | AEnd { wellformed = false } ->
            acc_error @@ Malformed { loc = ~@w; stuff = Alphanumeric_literal }
        | AUnexpected (c, _) ->
            acc_error @@ Unexpected { loc = ~@t;
                                      stuff = Character_in_symbolic_EBCDIC c }
      end
  in
  Grammar_tokens.ALPHANUM { quotation; hexadecimal = false;
                            str;             (* CHECKME: not the given string *)
                            runtime_repr = Native_bytes } &@<- w,
  diags

(* --- *)
