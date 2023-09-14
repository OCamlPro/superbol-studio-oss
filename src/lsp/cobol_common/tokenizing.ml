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

open Srcloc.INFIX

(** [fold_tokens ~tokenizer ~until ~f acc loc w] tokenizes [w] using
    [tokenizer], and folds over generated tokens using [f].  *)
let fold_tokens ~tokenizer ~until ?next_tokenizer ~f w acc =
  let lb = Lexing.from_string ~with_positions:true ~&w in
  let rec aux ~loc ~tokenizer acc = match tokenizer ~loc lb with
    | t when until t ->
        acc
    | t ->
        let len = Lexing.(lexeme_end lb - lexeme_start lb) in
        let loc = Lazy.force loc in
        let loc = lazy (Srcloc.trunc_prefix len loc)
        and tloc = Srcloc.prefix len loc in
        aux ~loc (f (t &@ tloc) acc)
          ~tokenizer:(match next_tokenizer with Some f -> f t | _ -> tokenizer)
  in
  aux ~tokenizer ~loc:(lazy ~@w) acc
