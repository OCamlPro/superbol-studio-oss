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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module DIAGS = Cobol_common.Diagnostics

module TYPES = struct

  type optional_token =
    {
      token: Grammar_tokens.token;
      mutable reserved: bool;
      mutable enabled: bool;
    }
  and token_handle = optional_token

  type lexer =
    {
      token_of_keyword: (string, token_handle) Hashtbl.t;
      decimal_point_is_comma: bool;
    }

end
include TYPES

module TokenHandles = struct
  include Set.Make
      (struct
        type t = token_handle
        let compare t1 t2 = Stdlib.compare t1.token t2.token
      end)
  let mem_text_token token =
    mem { token; enabled = false; reserved = false }
end

(* --- *)

let token_of_punct = Hashtbl.create 15
let punct_of_token = Hashtbl.create 15
let keyword_of_token = Hashtbl.create 257
let __token_of_keyword = Hashtbl.create 257         (* copied in `Make` below *)

(** Raises {!Not_found} if the token is neither a keyword nor a
    punctuation. *)
let show_token t =
  try Hashtbl.find keyword_of_token t with
  | Not_found -> Hashtbl.find punct_of_token t

let token_of_handle h = h.token

(** Never raises {!Not_found}. *)
let show_token_of_handle h =
  show_token @@ token_of_handle h

let pp_tokens_via_handles ppf toks =
  Pretty.list ~fopen:"{@[" ~fclose:"@]}" ~fempty:"{}" begin fun ppf h ->
    Pretty.string ppf (show_token_of_handle h)
  end ppf (TokenHandles.elements toks)

let reserve_token   h = h.reserved <- true
let unreserve_token h = h.reserved <- false
let enable_token    h = h.enabled <- true
let disable_token   h = h.enabled <- false

let enable_tokens tokens =
  TokenHandles.iter enable_token tokens

let disable_tokens tokens =
  TokenHandles.iter disable_token tokens

let __init_puncts =
  List.iter begin fun (punct, token) ->
    Hashtbl.add punct_of_token token punct;
    Hashtbl.add token_of_punct punct token
  end Text_keywords.puncts

let __init_default_keywords =
  List.iter begin fun (kwd, token) ->
    Hashtbl.add keyword_of_token token kwd;
    (* Every default token needs to be reserved explicitly *)
    Hashtbl.add __token_of_keyword kwd
      { token; enabled = true; reserved = false }
  end Text_keywords.keywords

let silenced_keywords =
  StringSet.of_list Text_keywords.silenced_keywords

(* --- *)

let create ?(decimal_point_is_comma = false) () =
  {
    token_of_keyword = Hashtbl.copy __token_of_keyword;
    decimal_point_is_comma;
  }

let decimal_point_is_comma lexer =
  {
    lexer with decimal_point_is_comma = true;
  }

let handle_of_keyword { token_of_keyword; _ } kwd =
  Hashtbl.find token_of_keyword kwd

let handle_of_token { token_of_keyword; _ } token =
  Hashtbl.find token_of_keyword (Hashtbl.find keyword_of_token token)

let reserve_insensitive_token { token_of_keyword; _ } kwd token_handle =
  Hashtbl.add token_of_keyword kwd
    { token_handle with enabled = true; reserved = true }

let reserve_sensitive_alias { token_of_keyword; _ } kwd token_handle =
  Hashtbl.add token_of_keyword kwd token_handle

let reserve_words lexer : Cobol_config.Types.words_spec -> unit =
  let on_token_handle_of kwd descr ~f =
    try f @@ handle_of_keyword lexer kwd with
    | Not_found when StringSet.mem kwd silenced_keywords ->
        ()                                        (* Ignore silently? Warn? *)
    | Not_found ->
        Pretty.error "@[Unable@ to@ %s@ keyword:@ %s@]@." descr kwd
  in
  List.iter begin fun (w, word_spec) ->
    match word_spec with
    | Cobol_config.Types.ReserveWord { preserve_context_sensitivity } ->
        on_token_handle_of w "reserve" ~f:begin fun h ->
          if preserve_context_sensitivity
          then reserve_token h
          else reserve_insensitive_token lexer w h
        end
    | ReserveAlias { alias_for; preserve_context_sensitivity } ->
        on_token_handle_of alias_for "alias" ~f:begin fun h ->
          if preserve_context_sensitivity
          then reserve_sensitive_alias lexer w h
          else reserve_insensitive_token lexer w h
        end
    | NotReserved ->
        on_token_handle_of w "unreserve" ~f:unreserve_token
  end

(* --- *)

let token_of_keyword { token_of_keyword; _ } s =
  match Hashtbl.find token_of_keyword (String.uppercase_ascii s) with
  | { token; enabled = true; reserved = true } -> token
  | _ -> raise Not_found

let tokens lexer ~loc lexbuf : Grammar_tokens.token with_loc list =
  let split_loc loc len =
    Cobol_common.Srcloc.prefix len loc,
    Cobol_common.Srcloc.trunc_prefix len loc
  in
  let rec aux ~loc acc =
    let open Grammar_tokens in
    let start_pos = lexbuf.Lexing.lex_curr_pos in
    let append x =
      let xloc, loc = split_loc loc @@ lexbuf.Lexing.lex_curr_pos - start_pos in
      aux ~loc ((x &@ xloc) :: acc)
    in
    let decimal_sep w c d e =
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
      aux ~loc ([tail &@ tail_loc; INTERVENING_ c &@ sloc] @ head @ acc)
    in
    match Text_categorizer.token lexbuf with
    | End ->
        List.rev acc
    | Digits "88" ->
        append EIGHTY_EIGHT
    | Digits w ->
        append (DIGITS w)
    | Numeric (w, None) ->
        append (SINTLIT w)
    | Numeric (n, Some (sep, d, None))
      when sep = if lexer.decimal_point_is_comma then ',' else '.' ->
        append (FIXEDLIT (n, sep, d))
    | Numeric (n, Some (sep, d, Some e))
      when sep = if lexer.decimal_point_is_comma then ',' else '.' ->
        append (FLOATLIT (n, sep, d, e))
    | Word s ->
        append (try token_of_keyword lexer s with Not_found -> WORD s)
    | Punctuation s ->
        append (Hashtbl.find token_of_punct s)
    | Numeric (w, Some (c, d, e)) ->
        decimal_sep w c d e
    | Unexpected c -> (* likely to be a comma; will produce syntax errors
                         otherwise *)
        append (INTERVENING_ c)
  in
  aux ~loc []

let tokens lexer
  : Lexing.lexbuf with_loc -> Grammar_tokens.token with_loc list = fun t ->
  tokens lexer ~loc:~@t ~&t

let tokens_of_string' lexer
  : string with_loc -> Grammar_tokens.token with_loc list =
  fun t -> tokens lexer ((Lexing.from_string ~&t) &@<- t)

(* --- Symbolic EBCDICs --- *)

let ebcdic_char i =
  (* TODO: (fixed/configurable tables) *)
  String.make 1 (Char.chr @@ Ebcdic.default.to_ascii.(i))

let decode_symbolic_ebcdics' ~quotation w =
  let open Text_categorizer in
  let acc_error ?loc fmt =
    DIAGS.kerror
      (fun diag (acc, diags) -> acc, DIAGS.Set.cons diag diags) ?loc fmt
  in
  let symbolic_ebcdic ~loc:_ = symbolic_ebcdic
  and alphanum_string ~loc:_ = alphanum_string in
  let str, diags =
    Cobol_common.Tokenizing.fold_tokens w ("", DIAGS.Set.none)
      ~tokenizer:alphanum_string
      ~until:(function AEnd _ -> true | _ -> false)
      ~next_tokenizer:(function
          | AEBCDIC _
          | AStr (_, EBCDIC) | AUnexpected (_, EBCDIC) -> symbolic_ebcdic
          | AStr (_, STR) | AUnexpected (_, STR) | AEnd _ -> alphanum_string)
      ~f:begin fun t -> match ~&t with    (* TODO: (fixed/configurable tables) *)
        | AStr (s, _) ->
            fun (acc, diags) -> acc ^ s, diags
        | AEBCDIC i when i < 1 || i > 256 ->
            acc_error ~loc:~@t "Invalid@ symbolic@ character@ ordinal@ \
                                (expected@ range@ is@ {1, ..., 256})"
        | AEBCDIC i ->
            fun (acc, diags) -> acc ^ ebcdic_char i, diags
        | AEnd { wellformed = true } ->
            Fun.id
        | AEnd { wellformed = false } ->
            acc_error ~loc:~@w "Malformed@ alphanumeric@ literal"
        | AUnexpected (c, _) ->
            acc_error ~loc:~@t "Unexpected@ character:@ `%c'" c
      end
  in
  Grammar_tokens.ALPHANUM { str; quotation; hexadecimal = false } &@<- w, diags

(* include Make (Text_keywords) *)

(* --- *)
