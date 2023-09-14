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

val fold_tokens
  : tokenizer:((loc:Srcloc.srcloc Lazy.t -> Lexing.lexbuf -> 'a) as 't)
  -> until:('a -> bool)
  -> ?next_tokenizer:('a -> 't)
  -> f:('a Srcloc.with_loc -> 'b -> 'b)
  -> string Srcloc.with_loc -> 'b -> 'b
