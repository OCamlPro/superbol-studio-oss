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

module TYPES = Text_types
include module type of Text_types
  with type text = Text_types.text
   and type t = Text_types.t
   and type text_word = Text_types.text_word
   and type alphanum = Text_types.alphanum
   and type quotation = Text_types.quotation
   and type pseudotext = Text_types.pseudotext
   and type comment = Text_types.comment
   and type comments = Text_types.comments

val textwordp : text_word with_loc -> bool
val textword_eqp : eq:string -> text_word with_loc -> bool

val cdirp : text_word with_loc -> bool
val cdir_eqp : eq:string -> text_word with_loc -> bool

val join_pseudo_string
  : string:((string with_loc as 'a) -> 'a)
  -> pseudoword_item with_loc list
  -> 'a option

val pseudo_string: string with_loc -> pseudoword
val pseudoword_of_string :  string with_loc -> pseudoword
val pseudoword_of_alphanum : (string * quotation) with_loc -> pseudoword
val pseudoword_of_integer : int64 with_loc -> pseudoword

val alphanum_as_pseudoword : (string * 'a) with_loc -> pseudoword

val pseudotext_of_string :
  string with_loc -> pseudotext with_loc
val pseudotext_of_alphanum :
  (string * quotation) with_loc -> pseudotext with_loc
val pseudotext_of_integer :
  int64 with_loc -> pseudotext with_loc
val alphanum_as_pseudotext :
  (string * 'a) with_loc -> pseudotext with_loc


val pp_text: text Pretty.printer
val pp_quote: quotation Pretty.printer
val pp_word: text_word with_loc Pretty.printer
val pp_pseudotext: pseudotext Pretty.printer
val pp_literal_kind: literal_kind Pretty.printer
val pp_alphanum: alphanum Pretty.printer

val prefix_of_literal_kind: literal_kind -> string
val char_of_quotation: quotation -> char
val strip_eof: text -> text
