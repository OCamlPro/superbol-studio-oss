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

  type lexing_options =
    {
      decimal_point_is_comma: bool;
    }

  type optional_token =
    {
      token: Grammar_tokens.token;
      mutable reserved: bool;
      mutable enabled: bool;
    }
  and token_handle = optional_token

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

let default_lexing_options =
  {
    decimal_point_is_comma = false;
  }

(* --- *)

module Make (Words: module type of Text_keywords) = struct

  let token_of_punct = Hashtbl.create 15
  let punct_of_token = Hashtbl.create 15
  let token_of_keyword = Hashtbl.create 257
  let keyword_of_token = Hashtbl.create 257

  (** Raises {!Not_found} if the token is neither a keyword nor a
      punctuation. *)
  let show_token t =
    try Hashtbl.find keyword_of_token t with
    | Not_found -> Hashtbl.find punct_of_token t

  let handle_of_keyword kwd =
    Hashtbl.find token_of_keyword kwd

  let handle_of_token token =
    Hashtbl.find token_of_keyword (Hashtbl.find keyword_of_token token)

  let token_of_handle h = h.token

  (** Never raises {!Not_found}. *)
  let show_token_of_handle h =
    show_token @@ token_of_handle h

  let reserve_token   h = h.reserved <- true
  let unreserve_token h = h.reserved <- false
  let enable_token    h = h.enabled <- true
  let disable_token   h = h.enabled <- false

  let __init_puncts =
    List.iter begin fun (punct, token) ->
      Hashtbl.add punct_of_token token punct;
      Hashtbl.add token_of_punct punct token
    end Words.puncts

  let __init_default_keywords =
    List.iter begin fun (kwd, token) ->
      Hashtbl.add keyword_of_token token kwd;
      (* Every default token needs to be reserved explicitly *)
      Hashtbl.add token_of_keyword kwd
        { token; enabled = true; reserved = false }
    end Words.keywords

  let reserve_insensitive_token kwd token_handle =
    Hashtbl.add token_of_keyword kwd
      { token_handle with enabled = true; reserved = true }

  let reserve_sensitive_alias kwd token_handle =
    Hashtbl.add token_of_keyword kwd token_handle

  let silenced_keywords =
    StringSet.of_list Words.silenced_keywords

  let reserve_words: Cobol_config.words_spec -> unit =
    let on_token_handle_of kwd descr ~f =
      try f @@ handle_of_keyword kwd with
      | Not_found when StringSet.mem kwd silenced_keywords ->
          ()                                        (* Ignore silently? Warn? *)
      | Not_found ->
          Pretty.error "@[Unable@ to@ %s@ keyword:@ %s@]@." descr kwd
    in
    List.iter begin fun (w, word_spec) -> match word_spec with
      | Cobol_config.ReserveWord { preserve_context_sensitivity } ->
          on_token_handle_of w "reserve" ~f:begin fun h ->
            if preserve_context_sensitivity
            then reserve_token h
            else reserve_insensitive_token w h
          end
      | ReserveAlias { alias_for; preserve_context_sensitivity } ->
          on_token_handle_of alias_for "alias" ~f:begin fun h ->
            if preserve_context_sensitivity
            then reserve_sensitive_alias w h
            else reserve_insensitive_token w h
          end
      | NotReserved ->
          on_token_handle_of w "unreserve" ~f:unreserve_token
    end

  let enable_tokens tokens =
    TokenHandles.iter enable_token tokens

  let disable_tokens tokens =
    TokenHandles.iter disable_token tokens

  (* --- *)

  let token_of_keyword s =
    match Hashtbl.find token_of_keyword s with
    | { token; enabled = true; reserved = true } -> token
    | _ -> raise Not_found

  exception MultiToks of
      (Grammar_tokens.token * int) list       (* with length, except for last *)

  let token =
    let open Grammar_tokens in
    let unexpected_decimal_sep w c d e =
      let head =
        if w = ""
        then []
        else [(if w.[0] <> '-' then DIGITS w else SINTLIT w), String.length w]
      and tail = [
        INTERVENING_ c, 1;
        match e with
        | None -> DIGITS d, 0
        | Some e -> FLOATLIT (w, c, d, e), 0
      ] in
      raise @@ MultiToks (head @ tail)
    in
    fun ~options:{ decimal_point_is_comma } lexbuf : token ->
      match Text_categorizer.token lexbuf with
      | Digits "88" ->
          EIGHTY_EIGHT
      | Digits w ->
          DIGITS w
      | Numeric (w, None) ->
          SINTLIT w
      | Numeric (n, Some (sep, d, None))
        when sep = if decimal_point_is_comma then ',' else '.' ->
          FIXEDLIT (n, sep, d)
      | Numeric (n, Some (sep, d, Some e))
        when sep = if decimal_point_is_comma then ',' else '.' ->
          FLOATLIT (n, sep, d, e)
      | Word s ->
          (try token_of_keyword s with Not_found -> WORD s)
      | Punctuation s ->
          Hashtbl.find token_of_punct s
      | End ->
          EOF
      | Numeric (w, Some (c, d, e)) ->
          unexpected_decimal_sep w c d e
      | Unexpected c -> (* likely to be a comma; will produce syntax errors
                          otherwise *)
          INTERVENING_ c

  let token_of_string' ~options
    : string with_loc -> Grammar_tokens.token with_loc =
    fun t -> token ~options (Lexing.from_string ~&t) &@<- t

  let tokens ~options
    : Lexing.lexbuf with_loc -> Grammar_tokens.token with_loc list = fun t ->
    try [ token ~options ~&t &@<- t ]
    with MultiToks toks ->
      let rec aux acc loc = function
        | [] -> acc
        | [t, _] -> (t &@ loc) :: acc
        | (t, len) :: (_ :: _ as tl) ->
            let tloc = Cobol_common.Srcloc.prefix len loc
            and loc = Cobol_common.Srcloc.trunc_prefix len loc in
            aux ((t &@ tloc) :: acc) loc tl
      in
      List.rev @@ aux [] ~@t toks

  let tokens_of_string' ~options
    : string with_loc -> Grammar_tokens.token with_loc list =
    fun t -> tokens ~options ((Lexing.from_string ~&t) &@<- t)

  let ebcdic_char i =
    (* TODO: (fixed/configurable tables) *)
    String.make 1 (Char.chr @@ Ebcdic.to_ascii.(i))

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
    Grammar_tokens.ALPHANUM (str, quotation) &@<- w, diags

end

include Make (Text_keywords)

(* --- *)
