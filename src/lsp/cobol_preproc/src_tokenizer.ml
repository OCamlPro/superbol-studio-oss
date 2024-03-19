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

(** [cdtoks_of_chstr w] translates a COBOL character string [w] into compiler
    directive tokens.  Note numeric literals keep their representation as
    text-words ({!TEXT_WORD}) in the result.  *)
let cdtoks_of_chstr directive_kind
    (chstr: text_word Cobol_common.Srcloc.with_loc) acc =
  let open Compdir_grammar in
  let ( ~@@ ) t = Cobol_common.Srcloc.as_lexloc ~@t in
  match ~&chstr with
  | CDirWord w
  | TextWord w ->
      List.rev @@ Cobol_common.Tokenizing.fold_tokens (w &@<- chstr) []
        ~tokenizer:(fun ~loc:_ -> Src_lexer.cdtoken directive_kind)
        ~until:(function CDEnd -> true | _ -> false)
        ~f:begin fun t -> match ~&t with
          | CDEnd -> Fun.id
          | CDTok tok -> List.cons (tok, ~@@t)
        end,
      acc
  | Alphanum { knd = Basic; str; qte = _ } ->
      [ALPHANUM str, ~@@chstr],
      acc
  | Alphanum { knd = Bool; str; qte = _ } ->
      let l = Cobol_ptree.boolean_of_string ~base:`Bool str in
      let b = Cobol_data.Literal.boolean (l &@<- chstr) in
      [BOOLIT ~&(b.result), ~@@chstr],
      Preproc_diagnostics.add_literal_diagnostics b.diags acc
  | Eof ->
      [EOL, ~@@chstr], acc
  | Pseudo _
  | Alphanum _
  | AlphanumPrefix _ ->
      [INVALID_ ~&chstr, ~@@chstr], acc

(** [pptoks_of_chstr w] translates a COBOL character string [w] into
    pre-processor tokens.  Note numeric literals keep their representation as
    text-words ({!TEXT_WORD}) in the result.  *)
let pptoks_of_chstr (chstr: text_word Cobol_common.Srcloc.with_loc) acc =
  let open Preproc_tokens in
  let pptok = match ~&chstr with
    | CDirWord w
    | TextWord w
      -> List.rev @@ Cobol_common.Tokenizing.fold_tokens (w &@<- chstr) []
          ~tokenizer:(fun ~loc:_ -> Src_lexer.pptoken)
          ~until:(function PPEnd -> true | _ -> false)
          ~f:begin fun t -> match ~&t with
            | PPEnd -> Fun.id
            | PPTok tok -> List.cons (tok &@<- t)
          end
    | Alphanum { knd = Basic; str; qte; _ }
      -> [ALPHANUM (str, qte) &@<- chstr]
    | Alphanum { knd = National | NationalX; str; _ }
      -> [NATLIT str &@<- chstr]
    | Alphanum { knd = Bool | BoolX; str; _ }
      -> [BOOLIT str &@<- chstr]
    | Alphanum { knd = Hex; str; _ }
      -> [HEXLIT str &@<- chstr]
    | Alphanum { knd = NullTerm; str; _ }
      -> [NULLIT str &@<- chstr]
    | AlphanumPrefix { str; qte; _ }
      -> [ALPHANUM_PREFIX (str, qte) &@<- chstr]
    | Pseudo p
      -> [PSEUDO_TEXT p &@<- chstr]
    | Eof
      -> [EOL &@<- chstr]
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
    ~pp:(cdtoks_of_chstr directive_kind)
    ~decompose:(fun (y, (s, e)) -> y, s, e)
    ~endlimit:(fun () -> Lexing.dummy_pos)

let pptoks_of_text_supplier (module Om: Src_overlay.MANAGER) text =
  Om.restart ();
  let prev_limit = ref None in
  fst @@ ondemand_list_supplier text ()
    ~eoi:Preproc_tokens.EOL
    ~pp:pptoks_of_chstr
    ~decompose:begin fun y ->
      let s, e = Om.limits ~@y in
      Option.iter (fun e -> Om.link_limits e s) !prev_limit;
      prev_limit := Some e;
      ~&y, s, e
    end
    ~endlimit:begin fun () ->
      Option.value !prev_limit ~default:Om.dummy_limit
    end
