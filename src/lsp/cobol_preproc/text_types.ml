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

open Cobol_common.Srcloc

(* The output of the preprocessor is a `text`, i.e. a list of words with
   locations. *)

type text = text_word with_loc list
and t = text                                                         (* alias *)
and text_word =
  | TextWord of string                                         (* upper-cased *)
  | CDirWord of string                                         (* upper-cased *)
  | Alphanum of alphanum
  | AlphanumPrefix of alphanum
  | Pseudo of pseudotext
  | Eof
and alphanum =
  {
    knd: literal_kind;
    qte: quotation;
    str: string;
  }
and quotation =
  | Quote
  | Apostrophe
and literal_kind =
  | Basic
  | Bool | BoolX                                           (* B, BX *)
  | Hex                                                    (* X *)
  | NullTerm                                               (* Z (not ISO/IEC) *)
  | National | NationalX                                   (* N, NX *)
and pseudotext =
  pseudoword list
and pseudoword =
  pseudotok with_loc
and pseudotok =
  | PseudoWord of pseudoword_item with_loc list               (* upper-cased? *)
  | PseudoAlphanum of alphanum
and pseudoword_item =
  | PwText of string
  | PwDelim of pseudotext_delimiter
and pseudotext_delimiter = string * Str.regexp       (* with pre-built regexp *)

type comment =
  {
    comment_loc: lexloc;
    comment_kind: [`Line | `Floating];
    comment_contents: string;
  }
