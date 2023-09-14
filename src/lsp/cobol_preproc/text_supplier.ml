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

(** [pptoks_of_chstr w] translates a COBOL character string [w] into
    pre-processor tokens.  Note numeric literals keep their representation as
    text-words ({!TEXT_WORD}) in the result.  *)
let pptoks_of_chstr (chstr: text_word Cobol_common.Srcloc.with_loc) =
  let open Preproc_tokens in
  match ~&chstr with
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

type 'b supplier = unit -> 'b * Lexing.position * Lexing.position

(** [ondemand_list_supplier ~pp ~eoi l] is a supplier that returns all tokens
    obtained after pre-processing of [l] by [pp], and then [eoi]. *)
let ondemand_list_supplier (module Om: Src_overlay.MANAGER) ~pp ~eoi l =
  let y_l = ref [] and l = ref l and prev_limit = ref None in
  let rec aux () =
    supply !y_l ~otherwise:begin fun () -> match !l with
      | x :: tl ->
          l := tl;
          supply (pp x) ~otherwise:aux
      | [] ->
          let b = Option.value !prev_limit ~default:Om.dummy_limit in
          eoi, b, b
    end
  and supply ~otherwise = function
    | y :: y_tl ->
        y_l := y_tl;
        let s, e = Om.limits ~@y in
        Option.iter (fun e -> Om.link_limits e s) !prev_limit;
        prev_limit := Some e;
        ~&y, s, e
    | [] ->
        otherwise ()
  in
  aux

let pptoks_of_text_supplier om text =
  ondemand_list_supplier ~eoi:Preproc_tokens.EOL ~pp:pptoks_of_chstr om text

(** Tokenize the given text into pptokens if it starts with a {!CDirWord}. *)
let supply_text_if_compiler_directive om = function
  | t :: _ as text when Text.cdirp t ->
      Ok (pptoks_of_text_supplier om text)
  | _ ->
      Error `NotCDir
