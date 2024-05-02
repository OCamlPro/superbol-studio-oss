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

open Cobol_common.Srcloc.INFIX
open Text.TYPES

type 'b supplier = unit -> 'b * Lexing.position * Lexing.position

(** [cdtoks_of_word w] translates a COBOL character string [w] into compiler
    directive tokens. *)
let cdtoks_of_word directive_kind
    (word: text_word Cobol_common.Srcloc.with_loc) acc =
  let open Compdir_grammar in
  let ( ~@@ ) t = Cobol_common.Srcloc.as_lexloc ~@t in
  let fixedlit f (toks, acc) =
    let f = Cobol_data.Literal.fixed f in
    List.cons (FIXEDLIT ~&(f.result), ~@@(f.result)) toks,
    Preproc_diagnostics.add_literal_diagnostics f.diags acc
  and boollit ~prefix_length ~base str =
    let l = Cobol_ptree.boolean_of_string ~base str in
    (* CHECKME: remove prefix & quotes from literal location here? *)
    let lloc = Cobol_common.Srcloc.trunc_prefix (prefix_length + 1) ~@word in
    let lloc = Cobol_common.Srcloc.trunc_suffix 1 lloc in
    let b = Cobol_data.Literal.boolean (l &@ lloc) in
    [BOOLLIT ~&(b.result), ~@@word],
    Preproc_diagnostics.add_literal_diagnostics b.diags acc
  in
  let lexer = Src_lexer.cdtoken directive_kind in
  match ~&word with
  | CDirWord w
  | TextWord w ->
      let toks, acc =
        Cobol_common.Tokenizing.fold_tokens (w &@<- word) ([], acc)
          ~tokenizer:(fun ~loc:_ -> lexer)
          ~until:(function CDEnd -> true | _ -> false)
          ~f:begin fun t -> match ~&t with
            | CDEnd ->
                Fun.id
            | CDInt s ->
                fixedlit (Cobol_ptree.fixed_of_strings s "1" &@<- t)
            | CDFxd (n, _, f) ->
                fixedlit (Cobol_ptree.fixed_of_strings n f &@<- t)
            | CDTok tok ->
                fun (toks, acc) -> List.cons (tok, ~@@t) toks, acc
          end
      in
      List.rev toks, acc
  | Alphanum { knd = Basic; str; qte = _ } ->
      [ALPHANUM str, ~@@word],
      acc
  | Alphanum { knd = Bool; str; qte = _ } ->
      boollit ~prefix_length:1 ~base:`Bool str
  | Alphanum { knd = BoolX; str; qte = _ } ->
      boollit ~prefix_length:2 ~base:`Hex str
  | Separator _ ->
      [], acc
  | Eof ->
      [EOL, ~@@word], acc
  | Pseudo _
  | ExecBlock _
  | Alphanum _
  | AlphanumPrefix _ ->
      [INVALID_ ~&word, ~@@word], acc

(** [pptoks_of_word w] translates a COBOL character string [w] into
    pre-processor tokens.  Note numeric literals keep their representation as
    text-words ({!TEXT_WORD}) in the result.  *)
let pptoks_of_word (word: text_word Cobol_common.Srcloc.with_loc) acc =
  let open Preproc_tokens in
  let pptok = match ~&word with
    | CDirWord w
    | TextWord w
      -> List.rev @@ Cobol_common.Tokenizing.fold_tokens (w &@<- word) []
          ~tokenizer:(fun ~loc:_ -> Src_lexer.pptoken)
          ~until:(function PPEnd -> true | _ -> false)
          ~f:begin fun t -> match ~&t with
            | PPEnd -> Fun.id
            | PPTok tok -> List.cons (tok &@<- t)
          end
    | Alphanum { knd = Basic; str; qte; _ }
      -> [ALPHANUM (str, qte) &@<- word]
    | Alphanum { knd = National | NationalX; str; _ }
      -> [NATLIT str &@<- word]
    | Alphanum { knd = Bool | BoolX; str; _ }
      -> [BOOLIT str &@<- word]
    | Alphanum { knd = Hex; str; _ }
      -> [HEXLIT str &@<- word]
    | Alphanum { knd = NullTerm; str; _ }
      -> [NULLIT str &@<- word]
    | AlphanumPrefix { str; qte; _ }
      -> [ALPHANUM_PREFIX (str, qte) &@<- word]
    | Pseudo p
      -> [PSEUDO_TEXT p &@<- word]
    | ExecBlock text
      -> [EXEC_BLOCK text &@<- word]
    | Separator _c
      -> []
    | Eof
      -> [EOL &@<- word]
  in
  pptok, acc

(** [ondemand_list_supplier ~pp ~eoi l] is a supplier that returns all tokens
    obtained after pre-processing of [l] by [pp], and then [eoi]. *)
let ondemand_list_supplier ~decompose ~endlimit ~pp ~eoi l acc =
  let y_l = ref [] and l = ref l in
  let acc = ref acc in
  let rec aux () =
    supply !y_l ~otherwise:begin fun () -> match !l with
      | x :: tl ->
          l := tl;
          let y, acc' = pp x !acc in
          acc := acc';
          supply y ~otherwise:aux
      | [] ->
          let b = endlimit () in
          (eoi, b, b)
    end
  and supply ~otherwise = function
    | y :: y_tl ->
        y_l := y_tl;
        decompose y
    | [] ->
        otherwise ()
  in
  aux, (fun () -> !acc)

(** [text] must be a valid compiler directive (i.e, start with a {!CDirWord}) *)
let cdtoks_of_text_supplier directive_kind text =
  ondemand_list_supplier text Preproc_diagnostics.none
    ~eoi:Compdir_grammar.EOL
    ~pp:(cdtoks_of_word directive_kind)
    ~decompose:(fun (y, (s, e)) -> y, s, e)
    ~endlimit:(fun () -> Lexing.dummy_pos)

let pptoks_of_text_supplier (module Om: Src_overlay.MANAGER) text =
  Om.restart ();
  let prev_limit = ref None in
  fst @@ ondemand_list_supplier text ()
    ~eoi:Preproc_tokens.EOL
    ~pp:pptoks_of_word
    ~decompose:begin fun y ->
      let s, e = Om.limits ~@y in
      Option.iter (fun e -> Om.link_limits e s) !prev_limit;
      prev_limit := Some e;
      ~&y, s, e
    end
    ~endlimit:begin fun () ->
      Option.value !prev_limit ~default:Om.dummy_limit
    end
