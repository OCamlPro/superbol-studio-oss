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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Cobol_common.Diagnostics.TYPES
open Text.TYPES
open Preproc_directives                         (* import types of directives *)
open Preproc_trace                              (* import types of log events *)

module DIAGS = Cobol_common.Diagnostics

(* --- Compiler Directives -------------------------------------------------- *)

(* COPY/REPLACING *)

let concat_strings = Cobol_common.Srcloc.concat_strings_with_loc
let lift_textword w = TextWord ~&w &@<- w

let nonempty_words words : _ result = match ~&words with
  | [] ->
      Error (DIAGS.One.error ~loc:~@words "Expected@ at@ least@ one@ text-word")
  | _ ->
      Ok words

type _ partial_word_request =
  | ExactlyOne: string with_loc partial_word_request
  | AtMostOne: string with_loc option partial_word_request

let partial_word (type k) (req: k partial_word_request) words : (k, _) result =
  match ~&words, req with
  | [], AtMostOne ->
      Ok None
  | [{ payload = PseudoWord [{ payload = PwText w | PwDelim (w, _); _ }];
       loc }], AtMostOne ->
      Ok (Some (w &@ loc))
  | [{ payload = PseudoWord [{ payload = PwText w | PwDelim (w, _); _ }];
       loc }], ExactlyOne ->
      Ok (w &@ loc)
  | [{ payload = PseudoAlphanum _; _ }], _ ->
      Error (DIAGS.One.error ~loc:~@words "Unexpected@ alphanumeric@ literal")
  | _, AtMostOne ->
      Error (DIAGS.One.error ~loc:~@words "Expected@ at@ most@ one@ text-word")
  | _, _ ->
      Error (DIAGS.One.error ~loc:~@words "Expected@ one@ text-word")

let partial_subst (k: partial_replacing) ({ payload = pat; _ } as repl_from) =
  { partial_subst_dir = k.repl_dir;
    partial_subst_len = String.length pat;
    partial_subst_regexp = Str.regexp @@ match k.repl_dir with
      | Leading  when k.repl_strict -> "^" ^ Str.quote pat ^ "\\(.+\\)$"
      | Leading                     -> "^" ^ Str.quote pat ^ "\\(.*\\)$"
      | Trailing when k.repl_strict -> "^\\(.+\\)" ^ Str.quote pat ^ "$"
      | Trailing                    -> "^\\(.*\\)" ^ Str.quote pat ^ "$"
  } &@<- repl_from

let exact_replacing repl_from repl_to =
  match nonempty_words repl_from with
  | Ok repl_from ->
      DIAGS.some_result @@ ReplaceExact { repl_from; repl_to }
  | Error diag ->
      DIAGS.no_result ~diags:(DIAGS.Set.one diag)

let partial_replacing partial_replacing repl_from repl_to =
  match partial_word ExactlyOne repl_from,
        partial_word AtMostOne repl_to with
  | Ok repl_from,
    Ok repl_to ->
      let repl_subst = partial_subst partial_replacing repl_from in
      DIAGS.some_result @@ ReplacePartial { repl_subst; repl_to }
  | Error diag, Ok _
  | Ok _, Error diag ->
      DIAGS.no_result ~diags:(DIAGS.Set.one diag)
  | Error diag,
    Error diag' ->
      DIAGS.no_result ~diags:(DIAGS.Set.two diag diag')

let replacing ?partial repl_from repl_to = match partial with
  | None -> exact_replacing repl_from repl_to
  | Some k -> partial_replacing k repl_from repl_to

(* --- *)

let try_partial_subst ~replloc { partial_subst_dir = dir;
                                 partial_subst_len = pat_len;
                                 partial_subst_regexp } s =
  let replace s_loc =
    Cobol_common.Srcloc.replacement ~replloc
      ~in_area_a:(Cobol_common.Srcloc.in_area_a s_loc)
  in
  let leading s_suffix s_loc prefix =       (* replacing s_prefix with prefix *)
    let pref_loc = Cobol_common.Srcloc.prefix       pat_len s_loc
    and suff_loc = Cobol_common.Srcloc.trunc_prefix pat_len s_loc in
    let suff = s_suffix &@ suff_loc in
    let res = match prefix with
      | None ->                                         (* replace with SPACES *)
          suff
      | Some { payload = new_prefix; loc = new_ } ->
          concat_strings (new_prefix &@ replace s_loc ~old:pref_loc ~new_) suff
    in
    res, pref_loc
  and trailing s_prefix s_loc suffix =      (* replacing s_suffix with suffix *)
    let suff_loc = Cobol_common.Srcloc.suffix       pat_len s_loc
    and pref_loc = Cobol_common.Srcloc.trunc_suffix pat_len s_loc in
    let pref = s_prefix &@ pref_loc in
    let res = match suffix with
      | None ->                                         (* replace with SPACES *)
          pref
      | Some { payload = new_suffix; loc = new_ } ->
          concat_strings pref (new_suffix &@ replace s_loc ~old:suff_loc ~new_)
    in
    res, suff_loc
  in
  let replace = match dir with Leading -> leading | Trailing -> trailing in
  if Str.string_match partial_subst_regexp ~&s 0
  then Ok (replace (Str.matched_group 1 ~&s) ~@s)
  else Error ()

(* --- *)

let alphanum_exact_match
    { str = s1; qte = q1; knd = k1 }
    { str = s2; qte = q2; knd = k2 }
  =
  q1 = q2 && k1 = k2 && s1 = s2

(** [pseudotext_exact_match pseudotext text] returns either a triple [(prefix,
    suffix, text_suffix)] if [text] exactly matches a non-empty [pseudotext], an
    error code otherwise.

    The error code indicates whether [text] does not match [pseudotext]
   ([`Mismatch]), or [text] matches a prefix of [pseudotext] ([`MissingText]).

    In case of a successful match, [prefix = Some p] iff [text] starts with a
   text-word [w = p ^ d ^ w'] where [p] is a non-empty string, [d] is a
   pseudotext delimiter (`:', `(', or `)'), and [d ^ w'] participated in the
   match.

    Likewise, in case of a successful match, [suffix = Some s] iff [text]
   terminates with a text-word [w = w' ^ d ^ s] where [s] is a non-empty string,
   [d] is a pseudotext delimiter (`:', `(', or `)'), and [w' ^ d] participated
   in the match.

    [text_suffix] is the suffix of text that did not participate in the
   match. *)
let pseudotext_exact_match
  : pseudotext -> text ->
    ('s option * 's option * srcloc * text, [> `Mismatch | `MissingText]) result =
  let starts_with ~prefix s =
    s = prefix ||
    let pl = String.length prefix in
    String.length s >= pl && Str.first_chars s pl = prefix
  and cut w d =       (* removes [d], assumed to be a prefix of [w], from [w] *)
    let dlen = String.length d in
    Str.string_after w dlen, dlen
  and take_loc_prefix = Cobol_common.Srcloc.prefix
  and trunc_loc_prefix = Cobol_common.Srcloc.trunc_prefix in
  let cut_text_prefix w wloc pw tl = match cut w pw with
    | "", _ -> tl
    | w, len -> (TextWord w &@ trunc_loc_prefix len wloc) :: tl
  and cut_word_prefix w wloc d = match cut w d with
    | "", _ -> None
    | w, len -> Some (w &@ trunc_loc_prefix len wloc)
  and cut_pw_prefix pw pwloc pwtl pl = match pwtl with
    | [] -> pl
    | tl -> (PseudoWord tl &@ trunc_loc_prefix (String.length pw) pwloc) :: pl
  in
  let concat_rev_srclocs l =
    Option.get @@ Cobol_common.Srcloc.concat_srclocs @@ List.rev l
  in
  let rec aux ?prefix ?(seek_delim = false) tlocs pseudotext text : _ result =
    match text, pseudotext with
    | [], _ ->
        (* We assume [pseudotext] is never initially empty upon call, so we can
           start by decomposing text. *)
        Error `MissingText
    | _, [] ->
        (* If pseudotext is empty at this point, we have a match. *)
        Ok (prefix, None, concat_rev_srclocs tlocs, text)
    | t :: tl, p :: pl ->
        (* Otherwise, consider the next pseudoword and match against the
           left-most word in [text] ([t]): *)
        match ~&t, ~&p with
        | TextWord w,
          PseudoWord [{ payload = PwText pw; _ }]
          when w <> pw ->
            (* Pseudotext ends with a text-word [pw] that does not match [w]. *)
            Error `Mismatch

        | TextWord w,
          PseudoWord [{ payload = PwDelim (d, _); _ }]
          when pl = [] && starts_with ~prefix:d w ->
            (* Pseudotext ends with delimiter [d], matched by [w]. *)
            let d_loc = take_loc_prefix (String.length d) ~@t in
            let matchloc = concat_rev_srclocs (d_loc :: tlocs) in
            Ok (prefix, cut_word_prefix w ~@t d, matchloc, tl)

        | TextWord w,
          PseudoWord ({ payload = PwText pw | PwDelim (pw, _); _ } as pwt :: pwtl)
          when starts_with ~prefix:pw w ->          (* [w] matches pseudotext. *)
            let res =
              let pw_loc = take_loc_prefix (String.length pw) ~@t in
              aux ?prefix (pw_loc :: tlocs) (* Recurse without matched prefix. *)
                (cut_pw_prefix pw ~@p pwtl pl) (cut_text_prefix w ~@t pw tl)
            in
            (match res, ~&pwt with
             | Ok _ as ok, _ -> ok
             | Error `Mismatch, PwDelim d when seek_delim ->
                 (* Continue searching in [w] if [pwt] is a delimiter [d]. *)
                 aux_seek_delim ?prefix tlocs w ~@t d p pl tl
             | Error _ as e, _ -> e)

        | TextWord w,
          PseudoWord ({ payload = PwDelim d; _ } :: _)
          when seek_delim ->
            (* Case of initial search for a delimiter. *)
            aux_seek_delim ?prefix tlocs w ~@t d p pl tl

        | Alphanum ta,
          PseudoAlphanum pa
          when alphanum_exact_match ta pa ->
            (* Matched an alphanumeric literal: simply recurse. *)
            aux ?prefix (~@t :: tlocs) pl tl

        | _ ->
            Error `Mismatch

  and aux_seek_delim ?prefix tlocs w wloc (d, d_re) p pl tl =
    let dlen = String.length d in
    (* Try matching from a _second_ occurrence of a pseudotext delimiter [d] in
       [w] if [d] is a prefix of [w], a _first_ occurrence otherwise. *)
    let vw = match Str.bounded_full_split d_re w 2 with
      | Str.[Delim _] ->
          Some (d &@ take_loc_prefix dlen wloc, tl)
      | Str.[Delim _; Text w] ->
          let dloc = take_loc_prefix dlen wloc in
          let wloc = trunc_loc_prefix dlen wloc in
          Some (d &@ dloc, lift_textword (w &@ wloc) :: tl)
      | Str.[Text v; Delim _] ->
          let vlen = String.length v in
          let vloc = take_loc_prefix vlen wloc in
          let dwloc = trunc_loc_prefix vlen wloc in
          let dloc = take_loc_prefix dlen dwloc in
          Some (v &@ vloc, lift_textword (d &@ dloc) :: tl)
      | Str.[Text v; Delim _; Text w] ->
          let vlen = String.length v in
          let vloc = take_loc_prefix vlen wloc in
          let dwloc = trunc_loc_prefix vlen wloc in
          let dloc = take_loc_prefix dlen dwloc in
          let dw = concat_strings (d &@ dloc) (w &@ wloc) in
          Some (v &@ vloc, lift_textword dw :: tl)
      | _ -> None
    in
    match vw with
    | Some (v, w) ->
        let prefix = match prefix with
          | None when ~&v = "" -> None
          | None -> Some v
          | Some p -> Some (concat_strings p v)
        in
        aux ?prefix ~seek_delim:true tlocs (p :: pl) w
    | None ->
        Error `Mismatch
  in
  aux ?prefix:None ~seek_delim:true []

(** [textword_partial_replace ~replloc partial_subst str text] checks whether
    the left-most word in [text] is a text-word [tw], and then tries to apply
    [partial_subst str tw] to obtain [y].  It returns either a pair [(y,
    text_suffix)] if successful, or an error code otherwise.

    The error code indicates whether [tw] does not comply with [try_subst]
    ([`Mismatch]), or [text] is empty ([`MissingText]). *)
let textword_partial_replace
  : replloc:srcloc -> partial_subst with_loc -> string with_loc option -> text ->
    ((string with_loc * srcloc) * text, [>`MissingText | `Mismatch]) result
  = fun ~replloc partial_subst repl_to -> function
    | [] -> Error `MissingText
    | { payload = TextWord w; loc } :: tl ->
        (match try_partial_subst ~replloc ~&partial_subst (w &@ loc) with
         | Ok subst -> Ok (subst repl_to, tl)
         | Error () -> Error `Mismatch)
    | _ -> Error `Mismatch

let to_text ~replloc ~old pseudotext : text =
  let replace item_loc =
    Cobol_common.Srcloc.replacement ~replloc ~old ~new_:item_loc
      ~in_area_a:(Cobol_common.Srcloc.in_area_a item_loc)
  in
  let string s = ~&s &@ replace ~@s in
  List.filter_map begin fun pseudoword -> match ~&pseudoword with
    | PseudoAlphanum a ->
        Some (Alphanum a &@ replace ~@pseudoword)
    | PseudoWord ps ->
        Option.map lift_textword (Text.join_pseudo_string ~string ps)
  end ~&pseudotext

(** [delim left text right], prepends [left] to the left-most text-word in
    [text] if [left] is not [None], and appends [right] to the right-most
    text-word in [text], if [right] is not [None]. *)
let delim left text right =
  let textword_cat op top s text = match s with
    | None -> text
    | Some s ->
        let s' = lift_textword s in
        top @@ match top text with
        | [] ->
            [s']
        | { payload = TextWord w; loc } :: tl ->
            lift_textword (op (w &@ loc) s) :: tl
        | t :: tl ->
            s' :: t :: tl
  in
  text |>
  textword_cat (fun w s -> concat_strings w s) List.rev right |>
  textword_cat (fun w s -> concat_strings s w) Fun.id left

let try_replacing_clause: replacing with_loc -> text -> _ result = fun replacing ->
  let replloc = ~@replacing in
  match ~&replacing with
  | ReplaceExact { repl_from; repl_to } ->
      begin fun text ->
        match pseudotext_exact_match ~&repl_from text with
        | Ok (l, r, matched_loc, suffix) ->
            let replacement_text = to_text ~replloc repl_to ~old:matched_loc in
            let log_entry = Replacement { matched_loc; replacement_text } in
            Ok (delim l replacement_text r, log_entry, suffix)
        | Error _ as e ->
            e
      end
  | ReplacePartial { repl_subst; repl_to } ->
      begin fun text ->
        match textword_partial_replace ~replloc repl_subst repl_to text with
        | Ok ((t, matched_loc), suffix) when ~&t = "" ->
            Ok ([], Replacement { matched_loc; replacement_text = [] }, suffix)
        | Ok ((t, matched_loc), suffix) ->
            let replacement_text = [lift_textword t] in
            let log_entry = Replacement { matched_loc; replacement_text } in
            Ok (replacement_text, log_entry, suffix)
        | Error _ as e ->
            e
      end

type partial_text_repl = [`NoReplacement | `MissingText]
type full_text_repl = [`NoReplacement]
type partial_text_repl_result =
  (text * log, [`MissingText of text * log * text]) result
type (_, _) repl_attempt =
  | OnPartText: (partial_text_repl, partial_text_repl_result) repl_attempt
  | OnFullText: (full_text_repl, text * log) repl_attempt

let rec try_replacing_phrase
  : type p q. (p, q) repl_attempt -> _ -> text -> (_, p) result =
  fun k repl text ->
  match repl, k with
  | [], OnPartText -> Error `NoReplacement
  | [], OnFullText -> Error `NoReplacement
  | repl :: tl, _ -> match try_replacing_clause repl text, k with
    | Ok _ as res, _ -> res
    | Error `MissingText, OnPartText -> Error `MissingText
    | Error _, _ -> try_replacing_phrase k tl text

(** [apply_replacing attempt repl text] applies the replacing clauses [repl] to
    [text], and returns a result that depends on whether the given text may be
    continued ([attempt = OnPartText]) or not ([attempt = OnFullText]). *)
let apply_replacing k repl log =
  let rec aux: type p q. (p, q) repl_attempt -> text -> log -> text -> q =
    fun k done_text log text ->
      match k, try_replacing_phrase k repl text, text with
      | OnPartText, Ok (done_text', le, []), _ ->
          Ok (done_text @ done_text', Preproc_trace.append le log)
      | OnFullText, Ok (done_text', le, []), _ ->
          done_text @ done_text', Preproc_trace.append le log
      | _, Ok (done_text', le, text), _ ->
          aux k (done_text @ done_text') (Preproc_trace.append le log) text
      | OnPartText, Error `MissingText, _ ->
          Error (`MissingText (done_text, log, text))
      | OnPartText, Error `NoReplacement, [] ->
          Ok (done_text, log)
      | OnFullText, Error `NoReplacement, [] ->
          done_text, log
      | _, Error _, x :: text ->
          aux k (done_text @ [x]) log text
  in
  aux k [] log

(* --- *)

module type ENTRY_POINTS = sig
  type 'x entry
  val replace_statement: replace_statement with_diags with_loc entry
  val lexing_directive: lexing_directive option with_diags with_loc entry
  val copy_statement: copy_statement with_diags with_loc entry
end

module type PPPARSER = sig
  exception Error

  (* The incremental API. *)
  module MenhirInterpreter: MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = Preproc_tokens.token

  (* The entry point(s) to the incremental API. *)
  module Incremental: ENTRY_POINTS with type
    'x entry := Lexing.position -> 'x MenhirInterpreter.checkpoint
end
